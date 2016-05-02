{-# LANGUAGE Arrows #-}

module Feldspar.Synch where



import qualified Prelude

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as Category

import Feldspar.Run
import Feldspar.Vector
import Feldspar.Validated
import Feldspar.Synch.System



-- | Synchronous stream transformer
newtype Synch m a b = Synch (m (Kleisli m a b))

instance Monad m => Category (Synch m)
  where
    id = Synch $ return Category.id

    Synch init2 . Synch init1 = Synch $ do
        f1 <- init1
        f2 <- init2
        return $ f2 Category.. f1

instance Monad m => Arrow (Synch m)
  where
    arr = Synch . return . arr
    first (Synch init) = Synch $ do
        f <- init
        return $ first f

type Event = Validated

eventWhen :: Data Bool -> a -> Event a
eventWhen = validWhen

-- | Singleton types
class Singleton s where single :: s
instance Singleton () where single = ()
instance (Singleton a, Singleton b) => Singleton (a,b) where single = (single,single)

-- | Run a synchronous stream transformer
runSynch :: (Singleton a, Monad m) => Synch m a b -> System m b
runSynch (Synch init) = System $ do
    f <- init
    return $ runKleisli f single

-- | Run a synchronous stream transformer with stdin/stdout as input/output
interactSynch
    :: (PrimType a, Formattable a, PrimType b, Formattable b)
    => Synch Run (Data a) (Data b)
    -> Run ()
interactSynch (Synch init) = do
    f <- init
    while (return true) $ do
        b <- runKleisli f =<< fget stdin
        fput stdout "" b ""

-- | Add an initialization action to a synchronous stream transformer
initSynch :: Monad m => m a -> (a -> Synch m b c) -> Synch m b c
initSynch p k = Synch $ do
    Synch init <- fmap k p
    init

-- | An identity stream transformer that runs the given action in every
-- iteration
actSynch :: Monad m => (a -> m ()) -> Synch m a a
actSynch p = Synch $ stepper $ \a -> p a >> return a

-- | Helper function for creating stream transformers from monadic code.
-- Typically used as
--
-- > Synch $ do
-- >    ... -- initialization code
-- >    stepper $ \a -> do
-- >        ... -- code to run each iteration
stepper :: Monad m => (a -> m b) -> m (Kleisli m a b)
stepper = return . Kleisli

-- | Lift a 'Program' function to a stream transformer
arrProg :: Monad m => (a -> m b) -> Synch m a b
arrProg = Synch . stepper

-- | Lift a source 'Program' to a stream transformer with '()' inputs
arrSource :: Monad m => m a -> Synch m () a
arrSource = arrProg . const

-- | Constant stream transformer with '()' inputs
constA :: Monad m => a -> Synch m () a
constA = arrSource . return

-- | Lift a stream transformer to work on events. The network will only run when
-- an input event occurs.
liftEvent :: (Syntax b, MonadComp m) =>
    Synch m a b -> Synch m (Event a) (Event b)
liftEvent (Synch init) = Synch $ do
    f <- init
    r <- newRef
    stepper $ \(Validated valid a) -> do
        iff valid (runKleisli f a >>= setRef r) (return ())
        b <- unsafeFreezeRef r
        return (Validated valid b)

-- | Identity stream transformer that ensures that the stream elements are
-- represented \"by value\"; i.e. they can be shared without recomputation.
forceS :: (Forcible a, MonadComp m) => Synch m a a
forceS = arrProg force

-- | Create a feedback loop. If no initial value is given, the fed-back stream
-- (of type @`Data` b@) must not be used in the first cycle.
feedback :: (Syntax c, MonadComp m)
    => Maybe c  -- ^ Initial value
    -> Synch m (a,c) (b,c)
    -> Synch m a b
feedback binit (Synch init) = Synch $ do
    f <- init
    r <- case binit of
           Just binit' -> initRef binit'
           Nothing     -> newRef
    stepper $ \a -> do
      c      <- getRef r
      (b,c') <- runKleisli f (a,c)
      setRef r c'
      return b

-- | Delay a stream by one cycle
delay :: (Syntax a, MonadComp m)
    => a  -- ^ Initial value
    -> Synch m a a
delay init = feedback (Just init) $ arr $ \(a,aPrev) -> (aPrev,a)

-- | Settable up-counter. An input event will set the counter to the received
-- value. Whenever there is no input event, the counter counts upwards from the
-- previous value. If there is no set event in the first cycle, the counter
-- starts from 0.
count :: (Num a, PrimType a, MonadComp m) => Synch m (Event (Data a)) (Data a)
count = feedback (Just 0) $ arr $ \(newEv,old) ->
    share (fromValidated newEv (old+1)) $ \next -> (next,next)

-- | Settable counter that counts down to, and stops at, 0
countDown :: (Num a, Ord a, PrimType a, MonadComp m) =>
    Synch m (Data a) (Data a)
countDown = forceS >>> feedback (Just 0) (arr step)
    -- `forceS` needed to get value sharing in `step`
  where
    step (set,count) =
        ( (set>0) ? set $ count
        , (set>0) ? set-1 $
          (count>0) ? count-1 $ count
        )

-- | Whenever the input stream is true, it will stay true for the specified
-- number of cycles
hold :: MonadComp m
    => Data Length  -- ^ Number of cycles to hold
    -> Synch m (Data Bool) (Data Bool)
hold n = arr (\a -> a ? n $ 0) >>> countDown >>> arr (>0)

-- | Create a stream of values that cycle through the given range, separated by
-- the given step length. It is assumed that @0 < stepLen < hi-lo@.
cycleStep :: (Num a, Ord a, PrimType a, MonadComp m)
    => Data a                     -- ^ From
    -> Data a                     -- ^ To
    -> Synch m (Data a) (Data a)  -- ^ Step length -> value
cycleStep lo hi = feedback (Just lo) $ arr $ \(stepLen,a) ->
    let a' = a+stepLen
    in  (a, (a'>hi) ? (a' - (hi-lo)) $ a')

-- | A latch circuit. Whenever there's an input event, the received value is
-- passed to the output; otherwise the previous output value is retained.
--
-- If no initial value is given, there must be en event in the first cycle.
latch :: (Syntax a, Forcible a, MonadComp m)
    => Maybe a
        -- ^ Initial value. This will be the initial output if there is no input
        -- event in the first cycle.
    -> Synch m (Event a) a
latch def = feedback def $ proc arg -> do
    b <- forceS <<< arr (\(aEv,aPrev) -> fromValidated aEv aPrev) -< arg
    returnA -< (b,b)

-- | The output stream will hold events for the specified number of cycles. New
-- events will replace old events immediately, even if the hold time for the old
-- event has not yet passed.
holdEvent :: (Syntax a, Forcible a, MonadComp m)
    => Data Length  -- ^ Number of cycles to hold
    -> Synch m (Event a) (Event a)
holdEvent n = forceS >>> proc (Validated valid as) -> do
    hs   <- hold n -< valid
    pass <- arr (\(v,vHold) -> v || not vHold) -< (valid,hs)
    as' <- latch Nothing -< Validated pass as
    returnA -< Validated hs as'

-- | A version of 'holdEvent' that generates more compact code (but probably not
-- better code)
holdEvent' :: (Syntax a, Forcible a, MonadComp m)
    => Data Length  -- ^ Number of cycles to hold
    -> Synch m (Event a) (Event a)
holdEvent' n = forceS >>> Synch ( do
    cr   <- initRef (0 :: Data Word32)
    oldr <- newRef
    stepper $ \(Validated valid a) -> do
      iff valid
        (setRef cr n >> setRef oldr a)
        (modifyRef cr (\c -> c==0 ? 0 $ c-1))
      c <- getRef cr
      a' <- ifE (c == 0)
        (return a)
        (unsafeFreezeRef oldr)
      return (validWhen (c/=0) a')
    )

-- | Run a stream function in chunks
chunk :: (Forcible a, Syntax b, MonadComp m)
    => Data Length             -- ^ Chunk size
    -> Synch m a b             -- ^ Computation to speed up
    -> Synch m a (Manifest b)  -- ^ Slow input -> slow output
chunk n (Synch init) = forceS >>> Synch (do
    f   <- init
    arr <- newArr n
    stepper $ \a -> do
      for (0,1,Excl n) $ \i -> do
        b <- runKleisli f a
        setArr i b arr
      iarr <- unsafeFreezeArr arr
      return $ Manifest n iarr
    )
  -- Note: It's important that the argument is initialized outside of `stepper`.
  --       Otherwise it would be re-initialized at every chunk.

-- | Identity stream transformer that traces to stdout
tracer :: (PrimType a, Formattable a) => String -> Synch Run (Data a) (Data a)
tracer prefix = forceS >>> arrProg (\a -> do
    fput stdout prefix a "\n"
    return a)

-- | Run a number of copies of a network in parallel. The input is split to the
-- sub-networks using the provided selector function.
parSplit :: (Forcible a, MonadComp m)
    => Int                   -- ^ Number of parallel networks
    -> (Int -> a -> b)       -- ^ Input selector
    -> (Int -> Synch m b c)  -- ^ Network to parallelize
    -> Synch m a [c]
parSplit n select s = forceS >>> Prelude.foldr
    (\i sPar -> ((arr (select i) >>> s i) &&& sPar) >>> arr (uncurry (:)))
    (arr (select 0) >>> s 0 >>> arr return)
    [1..n-1]

-- | Run a number of copies of a network in parallel
parList :: Monad m
    => Int                   -- ^ Number of parallel networks
    -> (Int -> Synch m a b)  -- ^ Network to parallelize
    -> Synch m [a] [b]
parList n s = Prelude.foldr
    (\i sPar -> arr (\(a:as) -> (a,as)) >>> s i *** sPar >>> arr (uncurry (:)))
    (arr Prelude.head >>> s 0 >>> arr return)
    [1..n-1]

