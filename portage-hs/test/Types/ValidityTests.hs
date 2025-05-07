{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

{-# Options_GHC -Wno-orphans #-}

module Types.ValidityTests (validityTests) where

import           Data.ByteString.Char8               (pack)
import Data.Either (isLeft)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import GHC.Natural
import Test.QuickCheck
import Test.Tasty

import Data.Parsable
import Test.Parsable
import Internal.Distribution.Portage.Types


validityTests :: STM TestTree
validityTests =
    testGroup "parsable validity tests" <$> sequenceA
        [ parsableQuickCheck (Proxy @Category)
        , parsableQuickCheck (Proxy @PkgName)
        , parsableQuickCheck (Proxy @VersionNum)
        , parsableQuickCheck (Proxy @VersionLetter)
        , parsableQuickCheck (Proxy @VersionSuffix)
        , parsableQuickCheck (Proxy @VersionSuffixNum)
        , parsableQuickCheck (Proxy @VersionRevision)
        , parsableQuickCheck (Proxy @Version)
--         , parsableQuickCheck (Proxy @Operator)
        , parsableQuickCheck (Proxy @ConstrainedDep)
        , parsableQuickCheck (Proxy @Slot)
        , parsableQuickCheck (Proxy @SubSlot)
        , parsableQuickCheck (Proxy @Repository)
        , parsableQuickCheck (Proxy @Package)
--         , parsableQuickCheck (Proxy @EBuildFileName)
        , parsableQuickCheck (Proxy @FauxVersion)
        , parsableQuickCheck (Proxy @FauxVersionNum)
        ]

-- Arbitrary instances    --
-- Based off Gentoo PMS 8 --

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural

-- > 3.1.1 Category names
-- > A category name may contain any of the characters [A-Za-z0-9+_.-].
-- > It must not begin with a hyphen, a dot or a plus sign.
instance Arbitrary Category where
    arbitrary = Category <$> wordGen wordStart wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordRest = (== '+') : (== '-') : (== '.') : wordStart

-- > 3.1.2 Package names
-- > A package name may contain any of the characters [A-Za-z0-9+_-]. It must
-- > not begin with a hyphen or a plus sign, and must not end in a hyphen
-- > followed by anything matching the version syntax described in section 3.2.
-- >
-- > Note: A package name does not include the category. The term qualified
-- > package name is used where a category/package pair is meant.
instance Arbitrary PkgName where
    arbitrary = PkgName <$> pkgGen wordStart wordRest
      where
        wordStart = [isAsciiUpper, isAsciiLower, isDigit, (== '_')]
        wordRest  = (== '+') : (== '-') : wordStart

-- > 3.1.3 Slot names
-- > A slot name may contain any of the characters [A-Za-z0-9+_.-].
-- > It must not begin with a hyphen, a dot or a plus sign.
instance Arbitrary Slot where
    arbitrary = Slot
        <$> slotGen
        <*> liftArbitrary arbitrary

instance Arbitrary SubSlot where
    arbitrary = SubSlot <$> slotGen

slotGen :: Gen String
slotGen = wordGen wordStart wordRest
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


-- TODO
-- > 3.1.4 USE flag names
-- > A USE flag name may contain any of the characters [A-Za-z0-9+_@-].
-- > It must begin with an alphanumeric character. Underscores should be
-- > considered reserved for USE_EXPAND, as described in section 11.1.1.
-- >
-- > Note: Usage of the at-sign is deprecated. It was previously required for
-- > LINGUAS.


-- > 3.1.5 Repository names
-- > A repository name may contain any of the characters [A-Za-z0-9_-].
-- > It must not begin with a hyphen. In addition, every repository
-- > name must also be a valid package name.
instance Arbitrary Repository where
    arbitrary = Repository <$> pkgGen wordStart wordRest
      where
        wordStart =
            [ isAsciiUpper
            , isAsciiLower
            , isDigit
            , (== '_')
            ]
        wordRest = (== '-') : wordStart


-- TODO
-- > 3.1.6 License names
-- > A license name may contain any of the characters [A-Za-z0-9+_.-].
-- > It must not begin with a hyphen, a dot or a plus sign.


-- TODO
-- > 3.1.7 Keyword names
-- > A keyword name may contain any of the characters [A-Za-z0-9_-].
-- > It must not begin with a hyphen. In contexts where it makes sense to do so,
-- > a keyword name may be prefixed by a tilde or a hyphen.
-- > In KEYWORDS, -* is also acceptable as a keyword.


-- TODO
-- > 3.1.8 EAPI names
-- > An EAPI name may contain any of the characters [A-Za-z0-9+_.-].
-- > It must not begin with a hyphen, a dot or a plus sign.


-- > 3.2 Version Specifications
-- > The package manager must neither impose fixed limits upon the number of
-- > version components, nor upon the length of any component. Package managers
-- > should indicate or reject any version that is invalid according to the
-- > rules below.

-- > A version starts with the number part, which is in the form
-- > [0-9]+(\.[0-9]+)* (an unsigned integer, followed by zero or more
-- > dot-prefixed unsigned integers).
instance Arbitrary VersionNum where
    arbitrary = VersionNum <$> nonEmptyGen nonEmptyDigitGen

-- > This may optionally be followed by one of [a-z] (a lowercase letter).
instance Arbitrary VersionLetter where
    arbitrary = VersionLetter <$> arbitrary `suchThat` isAsciiLower

-- > This may be followed by zero or more of the suffixes
-- > _alpha, _beta, _pre, _rc or _p [...]
instance Arbitrary VersionSuffix where
    arbitrary = arbitraryBoundedEnum

-- > [...] each of which may optionally be followed by an unsigned integer.
-- > Suffix and integer count as separate version components.
instance Arbitrary VersionSuffixNum where
    arbitrary = VersionSuffixNum <$> nonEmptyDigitGen

-- > This may optionally be followed by the suffix -r followed immediately by an
-- > unsigned integer (the “revision number”). If this suffix is not present, it
-- > is assumed to be -r0.
instance Arbitrary VersionRevision where
    arbitrary = VersionRevision <$> nonEmptyDigitGen

-- The composite of an entire version string
instance Arbitrary Version where
    arbitrary = Version
        <$> arbitrary
        <*> liftArbitrary arbitrary
        <*> liftArbitrary (liftArbitrary arbitrary)
        <*> liftArbitrary arbitrary

instance Arbitrary Operator where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary ConstrainedDep where
    arbitrary = ConstrainedDep
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> liftArbitrary arbitrary
        <*> liftArbitrary arbitrary

-- The composite of an entire package atom
instance Arbitrary Package where
    arbitrary = Package
        <$> arbitrary
        <*> arbitrary
        <*> liftArbitrary arbitrary
        <*> liftArbitrary arbitrary
        <*> liftArbitrary arbitrary

-- instance Arbitrary EBuildFileName where
--     arbitrary = EBuildFileName
--         <$> arbitrary
--         <*> arbitrary

-- Generates version numbers with exactly one component. For use with
-- FauxVersion.
instance Arbitrary FauxVersionNum where
    arbitrary = FauxVersionNum . VersionNum . pure <$> nonEmptyDigitGen

-- Generates Versions that are both valid Versions and also valid
-- parts of PkgNames. This helps test the critera from "3.1.2 Package names":
--
-- > It [...] must not end in a hyphen followed by anything matching the
-- > version syntax described in section 3.2.
instance Arbitrary FauxVersion where
    arbitrary = fmap FauxVersion $ Version
        <$> (unwrapFauxVersionNum <$> arbitrary)
        <*> liftArbitrary arbitrary
        <*> liftArbitrary (liftArbitrary arbitrary)
        <*> liftArbitrary arbitrary

nonEmptyGen :: Gen a -> Gen (NE.NonEmpty a)
nonEmptyGen = fmap NE.fromList . listOf1

nonEmptyDigitGen :: Gen (NE.NonEmpty Char)
nonEmptyDigitGen = nonEmptyGen $ arbitrary `suchThat` isDigit

-- | PkgName and Repository share a similar generator
pkgGen :: [Char -> Bool] -> [Char -> Bool] -> Gen String
pkgGen
    (removeHyphens -> wordStart)
    (removeHyphens -> wordRest ) = do
        ls <- oneof
            [ do
                s <- oneof startChoices
                -- This generator can get out of hand if we don't limit the size
                -- of the list
                k <- choose (0,5)
                rest <- vectorOf k $ oneof restChoices
                pure $ s : rest
            , pure []
            ]
        end <- nonVersion
        pure $ L.intercalate "-" $ ls ++ [end]
  where
    nonVersion =
        wordGen wordStart wordRest
            `suchThat` (isLeft . runParsable @Version . pack) 

    -- Don't start with @pure ""@ or it will end up creating a string that
    -- starts with @'-'@ (an invalid Package/Repository string)
    startChoices =
        [ nonVersion
--         Currently not behaving correctly; we're getting invalid package/repository names
--         , toString . unwrapFauxVersion <$> arbitrary
        ]

    restChoices = pure "" : startChoices

-- Sometimes hyphens are handled specially and should not be generated with
-- 'wordGen'. This removes hyphens which would normally be generated when
-- following specifcations.
removeHyphens :: [Char -> Bool] -> [Char -> Bool]
removeHyphens = map $ \f -> \case
    '-' -> False
    c   -> f c
