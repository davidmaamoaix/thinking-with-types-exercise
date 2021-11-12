{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Roles where

import Data.Coerce (Coercible (..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum (..), Product (..))

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

data BST v = Empty | Branch (BST v) v (BST v)

type role BST nominal
