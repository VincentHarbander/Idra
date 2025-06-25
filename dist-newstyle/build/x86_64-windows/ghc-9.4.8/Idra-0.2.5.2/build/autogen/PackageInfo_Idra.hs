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
version = Version [0,2,5,2] []

synopsis :: String
synopsis = "A composable, monadic and ergonomic EDSL for text RPG games in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
