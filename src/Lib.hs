{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import GHC.TypeLits
import Data.Proxy (Proxy(..))

libMain :: IO ()
libMain =  undefined 

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y  = 'True
  Or 'False y = y

type family Not (x :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True



maybeMap = fmap @Maybe

-- $> :t maybeMap
