{-# LANGUAGE Arrows #-}

module Feldspar.Synch where



import Prelude ()

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as Category

import Feldspar
import Feldspar.Vector
import Feldspar.Software
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
    :: (SmallType a, Formattable a, SmallType b, Formattable b)
    => Synch Software (Data a) (Data b)
    -> Software ()
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

-- | Resettable counter that counts upwards from 0
count :: (Num a, SmallType a, MonadComp m) => Synch m (Data Bool) (Data a)
count = feedback (Just 0) $ arr $ \(res,count) -> (count, res ? 0 $ count+1)

-- | Settable counter that counts down to, and stops at, 0
countDown :: (Num a, SmallType a, MonadComp m) => Synch m (Data a) (Data a)
countDown = forceS >>> feedback (Just 0) (arr step)
    -- `store` needed to get value sharing in `step`
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
cycleStep :: (Num a, SmallType a, MonadComp m)
    => Data a                     -- ^ From
    -> Data a                     -- ^ To
    -> Synch m (Data a) (Data a)  -- ^ Step length -> value
cycleStep lo hi = feedback (Just lo) $ arr $ \(stepLen,a) ->
    let a' = a+stepLen
    in  (a, (a'>hi) ? (a' - (hi-lo)) $ a')

-- | A latch circuit. Whenever the lock condition is true, the output from the
-- previous cycle is retained as the output; when the condition is false, the
-- current input is passed as the current output.
--
-- If no initial value is given, the lock condition must be false in the first
-- cycle.
latch :: (Syntax a, Forcible a, MonadComp m)
    => Maybe a
        -- ^ Initial value. This will be the initial output if the lock
        -- condition is true in the first cycle.
    -> Synch m (Data Bool, a) a
         -- ^ (Lock condition, inp) -> outp
latch def = feedback def $ proc arg@((lock,a),aPrev) -> do
    b <- forceS <<< arr (\((lock,a),aPrev) -> lock ? aPrev $ a) -< arg
    returnA -< (b,b)

-- | The output stream will hold \"interesting\" input values for the specified
-- number of cycles. The given predicate decides which values are interesting.
holdPred :: (Syntax a, Forcible a, MonadComp m)
    => (a -> Data Bool)  -- ^ Predicate for interesting values
    -> Data Length       -- ^ Number of cycles to hold
    -> Synch m a a
holdPred interesting n = forceS >>> proc as -> do
    is   <- forceS <<< arr interesting -< as
    hs   <- hold n -< is
    lock <- arr (\(i,iHold) -> not i && iHold) -< (is,hs)
    latch Nothing -< (lock,as)

-- | A version of 'holdPred' that generates more compact code (but probably not
-- better code)
holdPred' :: (Syntax a, Forcible a, MonadComp m)
    => (a -> Data Bool)  -- ^ Predicate for interesting values
    -> Data Length       -- ^ Number of cycles to hold
    -> Synch m a a
holdPred' interesting n = forceS >>> Synch ( do
    cr   <- initRef (0 :: Data Word32)
    oldr <- newRef
    stepper $ \a -> do
      iff (interesting a)
        (setRef cr n >> setRef oldr a)
        (modifyRef cr (\c -> c==0 ? c $ c-1))
      c <- getRef cr
      ifE (c == 0)
        (return a)
        (getRef oldr)
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
tracer :: (SmallType a, Formattable a) =>
    String -> Synch Software (Data a) (Data a)
tracer prefix = forceS >>> arrProg (\a -> do
    fput stdout prefix a "\n"
    return a)

