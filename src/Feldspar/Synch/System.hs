module Feldspar.Synch.System where



import Feldspar
import Feldspar.IO



-- | An interactive system
--
-- @`System` a@ can be thought of as an infinite stream of @a@ values with
-- interleaved IO: `IO (a, IO (a, IO (a, ...)))`
newtype System a = System { unSystem :: Program (Program a) }

instance Functor System
  where
    fmap f (System sys) = System $ fmap (fmap f) sys

instance Applicative System
  where
    pure = System . return . return
    System initf <*> System inita = System $ do
        nextf <- initf
        nexta <- inita
        return $ nextf <*> nexta

-- | Run a system forever, discarding its results
execSystem :: System a -> Program ()
execSystem (System sys) = do
    next <- sys
    while (return true) (next >> return ())

