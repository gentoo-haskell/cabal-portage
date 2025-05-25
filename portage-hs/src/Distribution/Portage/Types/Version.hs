{-|
Module      : Distribution.Portage.Types

Types for package versions as defined in the Gentoo Package Manager
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

module Distribution.Portage.Types.Version
(     -- * Versions
      Version(..)
    , VersionNum(..)
    , VersionLetter(..)
    , VersionSuffix(..)
    , VersionSuffixNum(..)
    , VersionRevision(..)
      -- * Internal
    , FauxVersion(..)
    , FauxVersionNum(..)
    ) where

import Data.Data (Data)
import Data.Function (on)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)
import GHC.Natural (Natural)

import Data.Parsable

data Version = Version
    { getVersionNum :: VersionNum
    , getVersionLetter :: Maybe VersionLetter
    , getVersionSuffixes :: [(VersionSuffix, Maybe VersionSuffixNum)]
    , getVersionRevision :: Maybe VersionRevision
    } deriving stock (Show, Eq, Data, Generic)

-- See section 3.3 "Version Comparison" of the Package Manager Specification
instance Ord Version where
    Version n1 l1 s1 r1 `compare` Version n2 l2 s2 r2
        =  n1 `compare` n2
        <> l1 `compare` l2
        <> s1 `compareSuffixes` s2
        <> r1 `compareRevisions` r2
      where
        -- Algorithm 3.5
        compareSuffixes [] [] = EQ
        compareSuffixes ((SuffixP,_):_) [] = GT
        compareSuffixes ((_,_):_) [] = LT
        compareSuffixes [] ((SuffixP,_):_) = LT
        compareSuffixes [] ((_,_):_) = GT
        compareSuffixes (u1:us1) (u2:us2) =
            (compare `on` fmap toNat) u1 u2
                <> us1 `compareSuffixes` us2
          where
            toNat :: Maybe VersionSuffixNum -> Natural
            toNat = maybe (0 :: Natural) (read . toList . unwrapVersionSuffixNum)

        compareRevisions = compare `on`
            maybe (0 :: Natural) (read . NE.toList . unwrapVersionRevision)

instance Printable Version where
    toString (Version n l s r)
        = toString n
        ++ foldMap toString l
        ++ (s >>= suffixString)
        ++ foldMap (\x -> "-" ++ toString x) r
      where
        suffixString (ss,sn) = "_" ++ toString ss ++ foldMap toString sn

instance Parsable Version st e where
    parserName = "portage version"
    parser = versionParser parser

newtype VersionNum = VersionNum
    { unwrapVersionNum :: NonEmpty (NonEmpty Char) }
    deriving stock (Show, Eq, Data, Generic)

-- See section 3.3 "Version Comparison" of the Package Manager Specification
instance Ord VersionNum where
    VersionNum (s1 :| ss1) `compare` VersionNum (s2 :| ss2)
        = toNat s1 `compare` toNat s2 -- Algorithm 3.2
            <> mconcat ((zipWith compRest `on` fmap trim) ss1 ss2) -- Algorithm 3.3
            <> length ss1 `compare` length ss2
      where
        compRest :: (Bool, NonEmpty Char) -> (Bool, NonEmpty Char) -> Ordering
        -- Neither side had a leading zero so we use numeric comparison
        compRest (False, c1) (False, c2) = toNat c1 `compare` toNat c2
        -- At least one side a had leading zero so we use string comparison
        compRest (_    , c1) (_    , c2) =       c1 `compare`       c2

        -- If a component starts with a '0', mark it True and remove any
        -- trailing 0s
        trim :: NonEmpty Char -> (Bool, NonEmpty Char)
        trim = \case
            ('0' :| xs) -> (True, '0' :| foldr skipTrailing0s [] xs)
            xs -> (False, xs)
          where
            skipTrailing0s '0' [] = []
            skipTrailing0s c cs = c:cs

        toNat :: NonEmpty Char -> Natural
        toNat = read . toList

instance IsList VersionNum where
    type instance Item VersionNum = NonEmpty Char
    fromList = VersionNum . fromList
    toList = toList . unwrapVersionNum

instance Printable VersionNum where
    toString = L.intercalate "." . NE.toList . fmap NE.toList . unwrapVersionNum

instance Parsable VersionNum st e where
    parserName = "portage version number"
    parser = versionNumParser (`sepBy1` ($(char '.'))) (NE.fromList <$>)

newtype VersionLetter = VersionLetter
    { unwrapVersionLetter :: Char }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable VersionLetter where
    toString (VersionLetter l) = [l]

instance Parsable VersionLetter st e where
    parserName = "portage version letter"
    parser = VersionLetter <$> satisfy isAsciiLower

data VersionSuffix
    = SuffixAlpha
    | SuffixBeta
    | SuffixPre
    | SuffixRC
    | SuffixP
    deriving stock (Show, Eq, Ord, Data, Generic, Bounded, Enum)

instance Printable VersionSuffix where
    toString = \case
        SuffixAlpha -> "alpha"
        SuffixBeta  -> "beta"
        SuffixPre   -> "pre"
        SuffixRC    -> "rc"
        SuffixP     -> "p"

instance Parsable VersionSuffix st e where
    parserName = "portage version suffix"
    parser = $(switch
        [| case _ of
            "alpha" -> pure SuffixAlpha
            "beta"  -> pure SuffixBeta
            "pre"   -> pure SuffixPre
            "rc"    -> pure SuffixRC
            "p"     -> pure SuffixP
        |])

newtype VersionSuffixNum = VersionSuffixNum
    { unwrapVersionSuffixNum :: NonEmpty Char }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable VersionSuffixNum where
    toString = NE.toList . unwrapVersionSuffixNum

instance Parsable VersionSuffixNum st e where
    parserName = "portage version suffix number"
    parser = VersionSuffixNum . NE.fromList <$> some (satisfy isDigit)

newtype VersionRevision = VersionRevision
    { unwrapVersionRevision :: NonEmpty Char }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable VersionRevision where
    toString (VersionRevision r) = "r" ++ NE.toList r

instance Parsable VersionRevision st e where
    parserName = "portage version revision"
    parser = do
        _ <- $(char 'r')
        VersionRevision . NE.fromList <$> some (satisfy isDigit)

------
-- Internal
------

-- | A type of Version that also forms valid PkgName and Repository strings:
--
--   "[A PkgName] must not end in a hyphen followed by anything matching the [Version]
--   syntax"
--   <https://projects.gentoo.org/pms/8/pms.html#x1-180003.1.2>
newtype FauxVersion = FauxVersion
    { unwrapFauxVersion :: Version }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype Printable

instance Parsable FauxVersion st e where
    parserName = "faux portage version"
    parser = FauxVersion <$> versionParser (unwrapFauxVersionNum <$> parser)

-- | A type of VersionNum which always has one digit. This avoids having '.' in
--   the resulting string, as this is an invalid character for both PkgName and
--   Category.
newtype FauxVersionNum = FauxVersionNum
    { unwrapFauxVersionNum :: VersionNum }
    deriving stock (Show, Eq, Ord, Data, Generic)
    deriving newtype Printable

instance Parsable FauxVersionNum st e where
    parserName = "faux portage version number"
    parser = FauxVersionNum <$> versionNumParser id pure

versionParser :: ParserT st e VersionNum -> ParserT st e Version
versionParser pn = do
        n <- pn
        l <- optional parser
        s <- many $ do
            _ <- $(char '_')
            ss <- parser
            sn <- optional parser
            pure (ss, sn)
        r <- optional $ $(string "-") *> parser
        pure $ Version n l s r

sepBy1 :: ParserT st e a -> ParserT st e sep -> ParserT st e [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  pure (x : xs)

versionNumParser ::
    (ParserT st e [Char] -> ParserT st e [a])
    -> (NonEmpty a -> NonEmpty (NonEmpty Char))
    -> ParserT st e VersionNum
versionNumParser f g = do
    ns <- f $ some $ satisfy isDigit
    pure $ VersionNum $ g $ NE.fromList ns
