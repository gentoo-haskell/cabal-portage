{-|
Module      : Distribution.Portage.Types.UseDep

Types for USE flag dependencies as defined in the Gentoo Package Manager
Specification.
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Distribution.Portage.Types.UseDep
(   -- * USE dependencies
      UseDependency(..)
    , UseDep(..)
    , UseDepDefault(..)
    , UseFlag(..)
    ) where

import Data.Data (Data)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)

import Data.Parsable

-- | A full USE dependency, involving one or more USE flags
newtype UseDependency
    = UseDependency { unUseDependency :: NonEmpty UseDep }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Parsable UseDependency st e where
    parserName = "portage USE dependency"
    parser = do
        _ <- $( char '[' )
        ds <- listP
        pure $ UseDependency $ NE.fromList ds
      where
        listP :: ParserT st e [UseDep]
        listP = parser >>= \d ->
            choice
                [ $( char ',' ) *> ((d:) <$> listP)
                , $( char ']' ) *> pure [d]
                ]

instance Printable UseDependency where
    toString (UseDependency ne) =
        "[" ++ L.intercalate "," (toString <$> NE.toList ne)  ++ "]"

instance IsList UseDependency where
    type instance Item UseDependency = UseDep
    fromList = UseDependency . NE.fromList
    toList = NE.toList . unUseDependency

-- | A single piece of a USE dependency
data UseDep
    = UseDepEnabled UseFlag (Maybe UseDepDefault)
    | UseDepMatching UseFlag (Maybe UseDepDefault)
    | UseDepNegated UseFlag (Maybe UseDepDefault)
    | UseDepMatchIfEnabled UseFlag (Maybe UseDepDefault)
    | UseDepMatchIfDisabled UseFlag (Maybe UseDepDefault)
    | UseDepDisabled UseFlag (Maybe UseDepDefault)
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Parsable UseDep st e where
    parserName = "portage USE dependency piece"
    parser = choice
        [ do
            _ <- $( char '!' )
            u <- parser
            d <- optional parser
            choice
                [ $( char '=' ) *> pure (UseDepNegated u d)
                , $( char '?' ) *> pure (UseDepMatchIfDisabled u d)
                ]
        , $( char '-' ) *> (UseDepDisabled <$> parser <*> optional parser)
        , do
            u <- parser
            d <- optional parser
            choice
                [ $( char '=' ) *> pure (UseDepMatching u d)
                , $( char '?' ) *> pure (UseDepMatchIfEnabled u d)
                , pure (UseDepEnabled u d)
                ]
        ]

instance Printable UseDep where
    toString = \case
        UseDepEnabled u d ->
            udString u d
        UseDepMatching u d ->
            udString u d ++ "="
        UseDepNegated u d ->
            "!" ++ udString u d ++ "="
        UseDepMatchIfEnabled u d ->
            udString u d ++ "?"
        UseDepMatchIfDisabled u d ->
            "!" ++ udString u d ++ "?"
        UseDepDisabled u d ->
            "-" ++ udString u d
        where udString u d = toString u ++ foldMap toString d

data UseDepDefault
    = UseDefaultEnabled
    | UseDefaultDisabled
    deriving stock (Show, Eq, Ord, Bounded, Enum, Data, Generic)

instance Parsable UseDepDefault st e where
    parserName = "portage USE dependency default"
    parser = choice
        [ try $ UseDefaultEnabled <$ $( string "(+)" )
        , UseDefaultDisabled <$ $( string "(-)" )
        ]

instance Printable UseDepDefault where
    toString = \case
        UseDefaultEnabled -> "(+)"
        UseDefaultDisabled -> "(-)"

newtype UseFlag = UseFlag
    { unUseFlag :: String }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype (IsString, Printable)

instance Parsable UseFlag st e where
    parserName = "portage USE flag"
    parser = UseFlag <$> wordAllowed wordStart wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            ]
        wordRest = wordStart ++
            [ (== '+')
            , (== '_')
            , (== '@')
            , (== '-')
            ]
