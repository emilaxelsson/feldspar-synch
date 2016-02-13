module Feldspar.Synch.System where



import Feldspar



-- | An interactive system
--
-- @`System` a@ can be thought of as an infinite stream of @a@ values with
-- interleaved IO: `IO (a, IO (a, IO (a, ...)))`
newtype System m a = System { unSystem :: m (m a) }

instance Functor m => Functor (System m)
  where
    fmap f (System sys) = System $ fmap (fmap f) sys

instance Monad m => Applicative (System m)
  where
    pure = System . return . return
    System initf <*> System inita = System $ do
        nextf <- initf
        nexta <- inita
        return $ nextf <*> nexta

-- | Run a system forever, discarding its results
execSystem :: MonadComp m => System m a -> m ()
execSystem (System sys) = do
    next <- sys
    while (return true) (next >> return ())

-- | Run a system for N iterations, discarding its results
execSystemN :: MonadComp m => Length -> System m a -> m ()
execSystemN n (System sys) = do
    next <- sys
    for (0,1,Excl (value n)) $ \_ -> next >> return ()

-- | Run a system as long as it returns 'true'
execSystemWhile :: MonadComp m => System m (Data Bool) -> m ()
execSystemWhile (System sys) = do
    next <- sys
    while next (return ())

