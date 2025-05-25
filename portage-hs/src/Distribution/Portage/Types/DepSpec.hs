{-|
Module      : Distribution.Portage.Types.DepSpec

Full dependency specification as defined in the Gentoo Package Manager
Specification.
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

module Distribution.Portage.Types.DepSpec
(   -- * Full dependency spec
      DepSpec(..)
    -- * Misc
    , Slot(..)
    , SubSlot(..)
    , Repository(..)
    ) where

import Data.Data (Data)
import GHC.Generics (Generic)

import Data.Parsable

import Distribution.Portage.Types.Package
import Distribution.Portage.Types.UseDep
import Distribution.Portage.Types.VersionedPkg

data DepSpec =
    VersionedDepSpec
    { depSpecVersionedPkg :: VersionedPkg
    , depSpecUseDependency :: Maybe UseDependency
    , depSpecSlot :: Maybe Slot
    , depSpecRepository :: Maybe Repository
    }
    | UnversionedDepSpec
    { depSpecPackage :: Package
    , depSpecUseDependency :: Maybe UseDependency
    , depSpecSlot :: Maybe Slot
    , depSpecRepository :: Maybe Repository
    }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Parsable DepSpec st String where
    parserName = "portage full dependency spec"
    parser = choice
        [ try $ do
            vp <- parser
            ud <- optional parser
            s <- optional $ $( char ':' ) *> parser
            r <- optional $ $( string "::" ) *> parser
            pure $ VersionedDepSpec vp ud s r
        , do
            p <- parser
            ud <- optional parser
            s <- optional $ $( char ':' ) *> parser
            r <- optional $ $( string "::" ) *> parser
            pure $ UnversionedDepSpec p ud s r
        ]

instance Printable DepSpec where
    toString (VersionedDepSpec vp ud ms mr)
        =  toString vp
        ++ foldMap toString ud
        ++ foldMap (\s -> ":" ++ toString s) ms
        ++ foldMap (\r -> "::" ++ toString r) mr
    toString (UnversionedDepSpec p ud ms mr)
        =  toString p
        ++ foldMap toString ud
        ++ foldMap (\s -> ":" ++ toString s) ms
        ++ foldMap (\r -> "::" ++ toString r) mr

data Slot
    = AnySlot
    | AnySlotBreakable
    | SlotBreakable
        { unwrapSlot :: String
        , getSubSlot :: Maybe SubSlot
        }
    | Slot
        { unwrapSlot :: String
        , getSubSlot :: Maybe SubSlot
        }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable Slot where
    toString AnySlot = "*"
    toString AnySlotBreakable = "="
    toString (SlotBreakable s mss)
        =  s
        ++ foldMap (\ss -> "/" ++ toString ss) mss
        ++ "="
    toString (Slot s mss)
        =  s
        ++ foldMap (\ss -> "/" ++ toString ss) mss

instance Parsable Slot st e where
    parserName = "portage slot"
    parser = choice
        [ $( char '*' ) *> pure AnySlot
        , $( char '=' ) *> pure AnySlotBreakable
        , do
            s <- slotParser
            mss <- optional $ $(char '/') *> parser
            choice
                [ $( char '=' ) *> pure (SlotBreakable s mss)
                , pure (Slot s mss)
                ]
        ]

newtype SubSlot = SubSlot { unwrapSubSlot :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance Parsable SubSlot st e where
    parserName = "portage sub-slot"
    parser = SubSlot <$> slotParser

slotParser :: ParserT st e String
slotParser = wordAllowed wordStart wordRest
  where
    wordStart =
        [ isAsciiUpper
        , isAsciiLower
        , isDigit
        , (== '_')
        ]
    wordRest = wordStart ++
        [ (== '+')
        , (== '.')
        , (== '-')
        ]

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
