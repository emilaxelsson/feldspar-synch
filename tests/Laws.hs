module Laws where



import Feldspar.Synch.System



--------------------------------------------------------------------------------
-- Proof of applicative laws for `System`
--------------------------------------------------------------------------------

-- identity: pure id <*> v = v
appLaw1 :: Monad m => System m a -> [System m a]
appLaw1 v@(System v') =
  [ pure id <*> v
  , System (return (return id)) <*> (System v')
  , System $ do
      nextf <- return (return id)
      nexta <- v'
      return $ nextf <*> nexta
  , System $ do
      nexta <- v'
      return $ return id <*> nexta
  , System $ do
      nexta <- v'
      return $ nexta
  , System v'
  , v
  ]

-- composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
appLaw2 :: Monad m =>
    System m (b -> c) -> System m (a -> b) -> System m a -> [System m c]
appLaw2 u@(System u') v@(System v') w@(System w') =
  [ pure (.) <*> u <*> v <*> w
  , System (return (return (.))) <*> System u' <*> v <*> w
  , ( System $ do
        nextf <- return (return (.))
        nexta <- u'
        return $ nextf <*> nexta
    ) <*> v <*> w
  , ( System $ do
        nexta <- u'
        return $ return (.) <*> nexta
    ) <*> System v' <*> w
  , ( System $ do
        nextf <- do
          nexta <- u'
          return $ return (.) <*> nexta
        nexta <- v'
        return $ nextf <*> nexta
    ) <*> System w'
  , System $ do
      nextf3 <- do
        nextf2 <- do
          nexta1 <- u'
          return $ return (.) <*> nexta1
        nexta2 <- v'
        return $ nextf2 <*> nexta2
      nexta3 <- w'
      return $ nextf3 <*> nexta3
  , System $ do
      nexta1 <- u'
      nextf2 <- return $ return (.) <*> nexta1
      nexta2 <- v'
      nextf3 <- return $ nextf2 <*> nexta2
      nexta3 <- w'
      return $ nextf3 <*> nexta3
  , System $ do
      nexta1 <- u'
      nexta2 <- v'
      nexta3 <- w'
      return $ ((return (.) <*> nexta1) <*> nexta2) <*> nexta3
  , System $ do
      nexta1 <- u'
      nexta2 <- v'
      nexta3 <- w'
      return $ nexta1 <*> (nexta2 <*> nexta3)
        -- Composition law for `Program` monad
  , System $ do
      nexta1 <- u'
      nextx  <- do
        nexta2 <- v'
        nexta3 <- w'
        return $ nexta2 <*> nexta3
      return $ nexta1 <*> nextx
  , System $ do
      nexta1 <- u'
      nextx  <- unSystem (System v' <*> System w')
      return $ nexta1 <*> nextx
  , System u' <*> System (unSystem (System v' <*> System w'))
  , u <*> (v <*> w)
  ]

-- homomorphism: pure f <*> pure x = pure (f x)
appLaw3 :: Monad m => (a -> b) -> a -> [System m b]
appLaw3 f a =
  [ pure f <*> pure a
  , System (return $ return f) <*> System (return $ return a)
  , System $ do
      nextf <- return $ return f
      nexta <- return $ return a
      return $ nextf <*> nexta
  , System $ return $ return f <*> return a
  , System $ return $ return (f a)
      -- Homomorphism law for `Program` monad
  , pure (f a)
  ]

-- interchange: u <*> pure y = pure ($ y) <*> u
appLaw4 :: Monad m => System m (a -> b) -> a -> [System m b]
appLaw4 u@(System u') a =
  [ u <*> pure a
  , System u' <*> System (return $ return a)
  , System $ do
      nextf <- u'
      nexta <- return $ return a
      return $ nextf <*> nexta
  , System $ do
      nextf <- u'
      return $ nextf <*> return a
  , System $ do
      nextf <- u'
      return $ pure ($ a) <*> nextf
        -- Interchange law for `Program` monad
  , System $ do
      nexta <- return $ pure ($ a)
      nextf <- u'
      return $ nexta <*> nextf
  , System (return $ pure ($ a)) <*> System u'
  , pure ($ a) <*> u
  ]

