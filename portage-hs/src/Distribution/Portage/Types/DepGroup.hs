{-|
Module      : Distribution.Portage.Types.DepGroup

Group elements in the Gentoo Package Manager Specification. (Specifically,
those that are valid in dependency specifications such as @DEPEND@.)

Also, blocks of 'DepSpec's or 'DepGroup's that make up the entirity of
dependency class strings, such as what is defined as @RDEPEND@.
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Distribution.Portage.Types.DepGroup
(   -- * Dependency groups
      DepGroup(..)
    , DepBlock(..)
    ) where

import Data.Data (Data)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)

import Data.Parsable

import Distribution.Portage.Types.UseDep
import Distribution.Portage.Types.DepSpec

-- | Groups of dependencies
data DepGroup
    -- | An AND dependency group, e.g.
    --
    --   @( ... )@
    = AndGroup
        { depGroupChildren :: NonEmpty (Either DepGroup DepSpec) }
    -- | An OR dependency group, e.g.
    --
    --   @|| ( ... )@
    | OrGroup
        { depGroupChildren :: NonEmpty (Either DepGroup DepSpec) }
    -- | A USE-specific dependency group, e.g.
    --
    --   @foo? ( ... )@
    | UseGroup
        { depGroupChildren :: NonEmpty (Either DepGroup DepSpec)
        , depGroupUseFlag :: UseFlag
        }
    -- | A negated USE-specific dependency group, e.g.
    --
    --   @!foo? ( ... )@
    | NotUseGroup
        { depGroupChildren :: NonEmpty (Either DepGroup DepSpec)
        , depGroupUseFlag :: UseFlag
        }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Parsable DepGroup st String where
    parserName = "portage dependency group"
    parser = choice
        [ $( string "|| (" ) *> spaces1 *> (OrGroup . NE.fromList <$> go)
        , do _ <- $( char '!' )
             u <- parser
             _ <- $( string "? (" )
             _ <- spaces1
             ds <- go
             pure $ NotUseGroup (NE.fromList ds) u
        , do u <- parser
             _ <- $( string "? (" )
             _ <- spaces1
             ds <- go
             pure $ UseGroup (NE.fromList ds) u
        , $( char '(' ) *> spaces1 *> (AndGroup . NE.fromList <$> go)
        ]
      where
        go :: ParserT st String [Either DepGroup DepSpec]
        go = choice
            [ do ds <- parser
                 _ <- spaces1
                 rest <- go
                 pure $ Right ds : rest
            , do dg <- parser
                 _ <- spaces1
                 rest <- go
                 pure $ Left dg : rest
            , $( char ')' ) *> pure []
            ]

instance Printable DepGroup where
    toString (AndGroup gs) = groupToString "" gs
    toString (OrGroup gs) = groupToString "|| " gs
    toString (UseGroup gs u) = groupToString (toString u ++ "? ") gs
    toString (NotUseGroup gs u) = groupToString ("!" ++ toString u ++ "? ") gs

-- | Top-level blocks of zero or more 'DepGroup's and 'DepSpec's that make
--   up the entirity of dependency class strings, such as what is defined as
--   @RDEPEND@.
newtype DepBlock = DepBlock { unDepBlock :: [Either DepGroup DepSpec] }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance IsList DepBlock where
    type instance Item DepBlock = Either DepGroup DepSpec
    fromList = DepBlock
    toList = unDepBlock

instance Parsable DepBlock st String where
    parserName = "portage top-level dependency block"
    parser = DepBlock <$> go
      where
        go :: ParserT st String [Either DepGroup DepSpec]
        go = choice
            [ parser >>= \dg -> choice
                [ spaces1 *> ((Left dg :) <$> go)
                , pure [Left dg]
                ]
            , parser >>= \ds -> choice
                [ spaces1 *> ((Right ds :) <$> go)
                , pure [Right ds]
                ]
            , pure []
            ]

instance Printable DepBlock where
    toString
        = L.intercalate " "
        . fmap (either toString toString)
        . unDepBlock

---        ---
-- Internal --
---        ---

-- | All the different groups share a similar implementation for toString
groupToString :: String -> NonEmpty (Either DepGroup DepSpec) -> String
groupToString start ne
    =  start
    ++ "( "
    ++ L.intercalate " " (either toString toString <$> NE.toList ne)
    ++ " )"

-- | Skip zero or more whitespace characters including tabs and line breaks
spaces :: ParserT st e ()
spaces = skipSatisfy $ \c ->
       generalCategory c == Space
    || generalCategory c == LineSeparator
    || generalCategory c == ParagraphSeparator

-- | Skip one or more whitespace characters including tabs and line breaks
spaces1 :: ParserT st e ()
spaces1 = void $ some spaces
