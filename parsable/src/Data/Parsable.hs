{-|
Module      : Data.Parsable

This module contains two simple classes, 'Parsable' and 'Printable'.

There is an implicit "soft isomorphism" between 'parser' and 'toString'.
(Successfully parsing a string and then running 'toString' on the result
should result in the original string.)

=== Language extensions

Because 'parser' does not take any arguments, it may be necessary
to explicitly declare the type of @t@ for these functions.

It may be helpful to enable and use the @TypeApplications@ and possibly
@ScopedTypeVariables@ extensions..

Look at the @Language Extensions@ section of the GHC documentation for
instructions on how to use these extensions.
-}

{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveTraversable #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language QuantifiedConstraints #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Data.Parsable
    (
    -- * Parsing
      Parsable(..)
    , ParserName(..)
    , runParsableIO
    , runParsableST
    , runParsable
    , extractResult
    , (<?>)
    , choice
    -- ** Wrappers
    , NaturalParsable(..)
    -- * Parsing functions
    , satisfyAny
    , wordAllowed
    , readParsec
    -- * Printing
    , Printable(..)
    , toText
    -- ** Wrappers
    , ShowPrintable(..)
    -- * Re-exports
    , module Control.Monad
    , module Control.Monad.Trans.Class
    , module Data.Char
    , module Data.Functor.Identity
    , module Data.String
    , module FlatParse.Basic
    , module FlatParse.Basic.Text
    , IOMode
    , PureMode
    , STMode
    ) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class
import Data.ByteString (ByteString)
import Data.Char hiding (isDigit)
import Data.Data
import Data.Functor.Identity
import Data.String
import Data.Text (Text, unpack)
import FlatParse.Basic
import FlatParse.Basic.Text
import FlatParse.Common.Parser (IOMode, PureMode, STMode)
import GHC.Exts (ZeroBitType)
import GHC.Generics
import Text.Read (readMaybe)

newtype ParserName a (st :: ZeroBitType) e = ParserName { getParserName :: String }
    deriving stock (Show, Eq, Ord)
    deriving newtype IsString

-- | Represents types that have a valid Parsec parser.
class Parsable a (st :: ZeroBitType) e where
    parser :: ParserT st e a
    parserName :: ParserName a st e
    {-# Minimal parser, parserName #-}

newtype NaturalParsable a = NaturalParsable
    { unwrapNaturalParsable :: a }

instance (Read a, Typeable a)
    => Parsable (NaturalParsable a) st String where
    parserName = "natural number"
    parser = (<?> "natural number") $ do
        ds <- some (satisfy isDigit)
        case readMaybe ds of
            Just i -> pure $ NaturalParsable i
            Nothing -> err $ "Could not parse as " ++ t ++ ": " ++ show ds
      where
        t = show $ typeRep $ Proxy @a

infix 0 <?>
(<?>) :: ParserT st String a -> String -> ParserT st String a
(<?>) p e = withError p (\e' -> err (unlines [e,e']))

-- | Similar to 'asum', but adds 'try' to all choices except the last.
choice :: [ParserT st e a] -> ParserT st e a
choice [p] = p
choice (p:ps) = try p <|> choice ps
choice [] = empty

-- | Convenience function to run a 'Parsable' parser.
runParsableIO :: forall a. (Parsable a IOMode String)
    => ByteString -> IO (Either (Maybe String) a)
runParsableIO bs = extractResult <$> runParserIO (parser <?> n) bs
    where n = getParserName $ parserName @a @IOMode @String

runParsableST :: forall a s. (Parsable a (STMode s) String)
    => ByteString -> ST s (Either (Maybe String) a)
runParsableST bs = extractResult <$> runParserST (parser <?> n) bs
    where n = getParserName $ parserName @a @(STMode s) @String

runParsable :: forall a. (Parsable a PureMode String)
    => ByteString -> Either (Maybe String) a
runParsable = extractResult . runParser (parser <?> n)
    where n = getParserName $ parserName @a @PureMode @String

extractResult :: Result e a -> Either (Maybe e) a
extractResult (OK a _) = Right a
extractResult (Err e)  = Left (Just e)
extractResult Fail     = Left Nothing

-- | Pass a previously-parsed string to this function in order to attempt
--   using 'read'. Produces proper error messages on failure.
readParsec :: forall a st. (Typeable a, Read a) => String -> ParserT st String a
readParsec s = (<?> typeName ++ " (Read instance)") $
    case readMaybe s of
        Just x -> pure x
        Nothing -> err
            $  "unable to parse using "
            ++ typeName
            ++ " Read instance: "
            ++ show s
    where typeName = show $ typeRep $ Proxy @a

-- | Parse a token that satisfies any of the given predicates
satisfyAny :: [Char -> Bool] -> ParserT st e Char
satisfyAny fs = satisfy $ \c -> or [f c | f <- fs]

-- | Parsing of "words" which require a list of predicates for the first
--   token, and a list of predicates for any remaining tokens. This always
--   parses at least one token.
wordAllowed
    :: [Char -> Bool] -- ^ Tokens that start the word
    -> [Char -> Bool] -- ^ Any subsequent tokens
    -> ParserT st e [Char]
wordAllowed beg rest = (:) <$> satisfyAny beg <*> many (satisfyAny rest)

-- | Types that can be converted back to a @String@.
class Printable t where
    toString :: t -> String

    default toString :: Show t => t -> String
    toString = show
    {-# Minimal toString #-}

instance Printable String where
    toString = id

instance Printable Text where
    toString = unpack

-- | Convenience function that will turn a 'Printable' to any 'IsString'.
toText :: (Printable t, IsString s) => t -> s
toText = fromString . toString

-- | Wrapper for types that inherit 'toString' directly from their 'Show' instance.
--
--   It is convenient to use the @DerivingVia@ language extension with this.
--
--   > {-# Language DerivingVia #-}
--   >
--   > newtype MyNum Int
--   >     deriving Printable via (ShowPrintable Int) -- Uses Show instance of Int
newtype ShowPrintable a = ShowPrintable
    { unwrapShowPrintable :: a }
    deriving stock
        ( Read, Show, Eq, Ord, Bounded, Functor, Foldable, Traversable, Generic
        , Generic1, Data
        )
    deriving newtype (Enum, Num, Fractional, Floating, IsString, Semigroup, Monoid)
    deriving (Applicative, Monad) via Identity

-- | Uses 'show' after unwrapping the contents.
instance Show a => Printable (ShowPrintable a) where
    toString = show . unwrapShowPrintable
