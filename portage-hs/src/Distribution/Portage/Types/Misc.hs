{-|
Module      : Distribution.Portage.Types.Misc

Misc. types in the Gentoo Package Manager Specification
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}

module Distribution.Portage.Types.Misc
(   -- * Full dependency spec
      Repository(..)
    ) where

import Data.Data (Data)
import GHC.Generics (Generic)

import Data.Parsable

import Distribution.Portage.Types.Package

newtype Repository = Repository { unwrapRepository :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance Parsable Repository st String where
    parserName = "portage repository"
    parser = Repository <$> pkgParser wordStart wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]

        wordRest = (== '-') : wordStart
