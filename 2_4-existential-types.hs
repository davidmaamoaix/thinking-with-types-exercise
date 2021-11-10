{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExistentialTypes where

import Data.Kind
import Data.Maybe
import Data.Typeable
import Control.Applicative

type Dynamic = Has Typeable

data Error = Error String

data Any where
    Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

data Has (c :: Type -> Constraint) where
    Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

instance Show (Has Show) where
    show = elimHas (("HasShow " ++) . show)

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimHas cast

-- different order than the book example
liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r) => (a -> b -> r) -> Dynamic -> Dynamic -> Maybe Dynamic
liftD2 f x y = Has <$> (f <$> (fromDynamic @a x) <*> (fromDynamic @b y))

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = fromMaybe (Has $ Error "bad types") $ liftD2 @String @String (++) a b
                                                   <|> liftD2 @Int @Int (+) a b
                                                   <|> liftD2 @String @Int ((. show) . (++)) a b
                                                   <|> liftD2 @Int @String ((++) . show) a b
