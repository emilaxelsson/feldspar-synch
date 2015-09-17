module Feldspar.Synch.Stream.Internal where



import Prelude ()
import Prelude (Applicative (..), (<$>))
import Control.Applicative (liftA2)

import Feldspar
import Feldspar.IO



-- | Infinite stream
--
-- Denotation: @`SF` r s a@ where @r@ is some external state that the
-- stream has access to and @s@ is the stream's local state.
newtype Stream a = Stream { unStream :: Program (Program a) }

instance Functor Stream
  where
    fmap f (Stream init) = Stream $ fmap (fmap f) init

instance Applicative Stream
  where
    pure = Stream . return . return
    Stream initf <*> Stream inita = Stream $ do
        nextf <- initf
        nexta <- inita
        return $ nextf <*> nexta

instance Num a => Num (Stream a)
  where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum

zip :: Stream a -> Stream b -> Stream (a,b)
zip = liftA2 (,)

-- | Share a stream in a stream function. The passed function may use its
-- argument multiple times without risking code duplication.
--
-- The semantics is the same as if the stream is inlined rather than shared;
-- i.e. like using '$'.
shareStream :: Type a
    => Stream (Data a)                -- ^ Stream to share
    -> (Stream (Data a) -> Stream b)  -- ^ Function that uses the shared stream
    -> Stream b
shareStream (Stream init1) k = Stream $ do
    next1 <- init1
    r     <- newRef
    let Stream init2 = k $ Stream $ return $ return $ unsafeFreezeRef r
    next2 <- init2
    return $ do
        setRef r =<< next1
        next2

-- | Create a stream from a feedback loop. If no initial value is given, the
-- fed-back stream (of type @`Stream` (`Data` b)@) must not be used in the first
-- cycle.
feedback :: Type b
    => Maybe (Data b)  -- ^ Initial value
    -> (Stream (Data b) -> Stream (c, Data b))
    -> Stream c
feedback binit f = Stream $ do
    r <- case binit of
           Just binit' -> initRef binit'
           Nothing     -> newRef
    let Stream initcb = f (Stream $ return $ getRef r)
    nextcb <- initcb
    return $ do
      (c,b) <- nextcb
      setRef r b
      return c

-- | Delay a stream by one cycle
delay :: Type a
    => Data a  -- ^ Initial value
    -> Stream (Data a)
    -> Stream (Data a)
delay init as = feedback (Just init) $ \olds -> zip olds as

-- | Resettable counter that counts upwards from 0
count :: Numeric a => Stream (Data Bool) -> Stream (Data a)
count reset = feedback (Just 0) $ \cs -> inc <$> cs <*> reset <*> cs
  where
    inc c1 r c2 = (c1, r ? 0 $ c2+1)

-- | Settable counter that counts down to, and stops at, 0
countDown :: (Numeric a, Ord a) => Stream (Data a) -> Stream (Data a)
countDown set =
    feedback (Just 0) $ \cs ->
      shareStream set $ \set' ->  -- Needed to get value sharing in `step`
        step <$> set' <*> cs
  where
    step s c =
        ( (s>0) ? s $ c
        , (s>0) ? s-1 $
          (c>0) ? c-1 $ c
        )

-- | Whenever the input stream is true, it will stay true for the specified
-- number of cycles
hold
    :: Data Length         -- ^ Number of cycles to hold
    -> Stream (Data Bool)  -- ^ Stream to hold
    -> Stream (Data Bool)
hold n = fmap (>0) . countDown . fmap (\a -> a ? n $ 0)

-- | Create a stream of values that cycle through the given range, separated by
-- the given step length. It is assumed that @0 < step < hi-lo@.
cycleStep :: (Numeric a, Ord a)
    => Data a  -- ^ From
    -> Data a  -- ^ To
    -> Data a  -- ^ Current step length
    -> Stream (Data a)
cycleStep lo hi stepLen = feedback (Just lo) $ \as -> zip as (step <$> as)
  where
    step a = (a'>hi) ? (a' - (hi-lo)) $ a'
      where a' = a+stepLen

-- | A latch circuit. If no initial value is given, the lock condition must be
-- false in the first cycle.
latch :: Type a
    => Maybe (Data a)      -- ^ Initial value
    -> Stream (Data Bool)  -- ^ Lock condition
    -> Stream (Data a)
    -> Stream (Data a)
latch def lock as =
    feedback def $ \old ->
      shareStream (mux <$> lock <*> old <*> as) $ \out ->
        zip out out
  where
    mux lock old a = lock ? old $ a

-- | A slightly more efficient version of 'holdPred'
holdPred' :: Eq a
    => (Data a -> Data Bool)  -- ^ Predicate for interesting values
    -> Data Length            -- ^ Number of cycles to hold
    -> Stream (Data a)
    -> Stream (Data a)
holdPred' interesting n (Stream inita) = Stream $ do
    nexta <- inita
    cr    <- initRef 0
    oldr  <- newRef
    return $ do
      a <- shareVal =<< nexta
      iff (interesting a)
        (setRef cr n >> setRef oldr a)
        (modifyRef cr (\c -> c==0 ? c $ c-1))
      c <- getRef cr
      ifE (c == 0)
        (return a)
        (getRef oldr)

-- | Run a stream function in chunks
chunk :: (Type a, Type b)
    => Data Length                  -- ^ Chunk size
    -> (Data a -> Stream (Data b))  -- ^ Computation to speed up
    -> Stream (Data a)              -- ^ Slow input stream
    -> Stream (Data [b])            -- ^ Slow output stream
chunk n f (Stream inita) = Stream $ do
    nexta <- inita
    r     <- newRef
    let Stream initb = f $ unsafeFreezeRef r
    nextb <- initb
    arr   <- newArr n
    return $ do
      setRef r =<< nexta
      for 0 (n-1) $ \i -> do
        b <- nextb
        setArr i b arr
      freezeArr arr n
  -- Note: It's important that the stream resulting from `f` is initialized
  --       together with the result stream. It should not be re-initialized at
  --       every chunk. Reference `r` makes this possible.

