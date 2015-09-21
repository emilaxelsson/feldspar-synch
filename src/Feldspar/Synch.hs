{-# LANGUAGE Arrows #-}

module Feldspar.Synch where



import Prelude ()

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as Category

import Feldspar
import Feldspar.IO
import Feldspar.Synch.System



-- | Synchronous stream transformer
newtype Synch a b = Synch (Program (Kleisli Program a b))

instance Category Synch
  where
    id = Synch $ return Category.id

    Synch init2 . Synch init1 = Synch $ do
        f1 <- init1
        f2 <- init2
        return $ f2 Category.. f1

instance Arrow Synch
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
runSynch :: Singleton a => Synch a b -> System b
runSynch (Synch init) = System $ do
    f <- init
    return $ runKleisli f single

-- | Helper function for creating stream transformers from monadic code.
-- Typically used as
--
-- > Synch $ do
-- >    ... -- initialization code
-- >    stepper $ \a -> do
-- >        ... -- code to run each iteration
stepper :: (a -> Program b) -> Program (Kleisli Program a b)
stepper = return . Kleisli

-- | Lift a 'Program' function to a stream transformer
arrProg :: (a -> Program b) -> Synch a b
arrProg = Synch . stepper

-- | Lift a source 'Program' to a stream transformer with '()' inputs
arrSource :: Program a -> Synch () a
arrSource = arrProg . const

-- | Constant stream transformer with '()' inputs
constA :: a -> Synch () a
constA = arrSource . return

-- | Identity stream transformer that ensures that the stream elements are
-- represented \"by value\"; i.e. they can be shared without recomputation.
store :: Type a => Synch (Data a) (Data a)
store = arrProg shareVal

-- | Create a feedback loop. If no initial value is given, the fed-back stream
-- (of type @`Data` b@) must not be used in the first cycle.
feedback :: Type c
    => Maybe (Data c)  -- ^ Initial value
    -> Synch (a, Data c) (b, Data c)
    -> Synch a b
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
delay :: Type a
    => Data a  -- ^ Initial value
    -> Synch (Data a) (Data a)
delay init = feedback (Just init) $ arr $ \(a,aPrev) -> (aPrev,a)

-- | Resettable counter that counts upwards from 0
count :: Numeric a => Synch (Data Bool) (Data a)
count = feedback (Just 0) $ arr $ \(res,count) -> (count, res ? 0 $ count+1)

-- | Settable counter that counts down to, and stops at, 0
countDown :: (Numeric a, Ord a) => Synch (Data a) (Data a)
countDown = store >>> feedback (Just 0) (arr step)
    -- `store` needed to get value sharing in `step`
  where
    step (set,count) =
        ( (set>0) ? set $ count
        , (set>0) ? set-1 $
          (count>0) ? count-1 $ count
        )

-- | Whenever the input stream is true, it will stay true for the specified
-- number of cycles
hold
    :: Data Length  -- ^ Number of cycles to hold
    -> Synch (Data Bool) (Data Bool)
hold n = arr (\a -> a ? n $ 0) >>> countDown >>> arr (>0)

-- | Create a stream of values that cycle through the given range, separated by
-- the given step length. It is assumed that @0 < step < hi-lo@.
cycleStep :: (Numeric a, Ord a)
    => Data a                   -- ^ From
    -> Data a                   -- ^ To
    -> Synch (Data a) (Data a)  -- ^ Step length -> value
cycleStep lo hi = feedback (Just lo) $ arr $ \(stepLen,a) ->
    let a' = a+stepLen
    in  (a, (a'>hi) ? (a' - (hi-lo)) $ a')

-- | A latch circuit. If no initial value is given, the lock condition must be
-- false in the first cycle.
latch :: Type a
    => Maybe (Data a)  -- ^ Initial value
    -> Synch (Data Bool, Data a) (Data a)
         -- ^ (Lock condition, inp) -> outp
latch def = feedback def $ proc arg@((lock,a),aPrev) -> do
    b <- store <<< arr (\((lock,a),aPrev) -> lock ? aPrev $ a) -< arg
    returnA -< (b,b)
  -- TODO sharing?

-- | The output stream will hold \"interesting\" input values for the specified
-- number of cycles. The given predicate decides which values are interesting.
holdPred :: Eq a
    => (Data a -> Data Bool)  -- ^ Predicate for interesting values
    -> Data Length            -- ^ Number of cycles to hold
    -> Synch (Data a) (Data a)
holdPred interesting n = store >>> proc as -> do
    is   <- store <<< arr interesting -< as
    hs   <- hold n -< is
    lock <- arr (\(i,iHold) -> not i && iHold) -< (is,hs)
    latch Nothing -< (lock,as)
  -- TODO Check sharing

-- | A version of 'holdPred' that generates more compact code (but probably not
-- better code)
holdPred' :: Eq a
    => (Data a -> Data Bool)  -- ^ Predicate for interesting values
    -> Data Length            -- ^ Number of cycles to hold
    -> Synch (Data a) (Data a)
holdPred' interesting n = store >>> Synch ( do
    cr   <- initRef 0
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
chunk :: (Type a, Type b)
    => Data Length                -- ^ Chunk size
    -> Synch (Data a) (Data b)    -- ^ Computation to speed up
    -> Synch (Data a) (Data [b])  -- ^ Slow input -> slow output
chunk n (Synch init) = store >>> Synch (do
    f   <- init
    arr <- newArr n
    stepper $ \a -> do
      for 0 (n-1) $ \i -> do
        b <- runKleisli f a
        setArr i b arr
      freezeArr arr n
    )
  -- Note: It's important that the argument is initialized outside of `stepper`.
  --       Otherwise it would be re-initialized at every chunk.

-- | Identity stream transformer that traces to stdout
tracer :: (Type a, Formattable a) => String -> Synch (Data a) (Data a)
tracer prefix = store >>> arrProg (\a -> do
    fput stdout prefix a "\n"
    return a)

