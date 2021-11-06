{-# LANGUAGE RankNTypes #-}

module RankNTypes where

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

-- continuation monad
cont :: a -> (forall r. (a -> r) -> r)
cont a call = call a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = f id

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
    fmap f (Cont ar) = Cont ($ ar f)

instance Applicative Cont where
    pure a = Cont ($ a)
    (Cont f) <*> (Cont a) = Cont (cont $ runCont f (runCont a))

instance Monad Cont where
    (Cont a) >>= f = f (runCont a)

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
    version <- Cont withVersionNumber
    date <- Cont withTimestamp
    os <- Cont withOS
    pure $ os ++ "-" ++ show version ++ "-" ++ show date
