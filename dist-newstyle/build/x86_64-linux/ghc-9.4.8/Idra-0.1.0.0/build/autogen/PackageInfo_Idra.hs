{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Idra (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Idra"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A simple and ergonomic text RPG EDSL in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
