{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib where

import GHC.TypeLits
import Data.Proxy (Proxy(..))
import Data.Kind(Constraint, Type)


libMain :: IO ()
libMain =  undefined 

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y  = 'True
  Or 'False y = y

type family Not (x :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[]       = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs


instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  (<=) HNil HNil = True
  (<=) (a :# as) (b :# bs)
    | a < b = True
    | b < a = False
    | otherwise = as <= bs


instance All Show ts => Show (HList ts) where
  show HNil = "'[]"
  show (a :# as) = "'[" ++ show a ++ show as ++ "]"

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts


hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ ) = show b


