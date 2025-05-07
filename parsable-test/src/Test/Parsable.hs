{-|
Module      : Test.Parsable

Test functions for 'Parsable' and 'Printable'.
-}

{-# Language CPP #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language ScopedTypeVariables #-}
{-# Language TupleSections #-}
{-# Language TypeApplications #-}

module Test.Parsable
    (
      -- * Partial parses
      ParseCoverage(..)
      -- * QuickCheck
    , parsableQuickCheck
    , parsableProp
      -- ** Generators
    , wordGen
      -- * HUnit
    , parsableHUnit
    , printableHUnit
    , parsableAssertion
    , printableAssertion
      -- * Generic functions
    , runCheckParsable
    , checkParsable
    , checkCoverage
      -- * Re-exports
    , module Control.Monad.STM
    , module Data.Void
    ) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Function (fix)
import Data.Semigroup (Last(..))
import Data.Typeable
import Data.Void
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (checkCoverage)

import Data.Parsable

-- | If a parse succeeds for the beginning of the input, but then fails, we
--   choose the 'PartialParse' constructor. If the entire parse was successful,
--   we choose 'CompleteParse'.
--
--   This is mostly useful for testing parsers where we need to test them
--   individually (must end with 'CompleteParse') and also composed together
--   (individual parsers may end with 'PartialParse' if they do not conclude
--   the larger parse).
data ParseCoverage
    = PartialParse String
    | CompleteParse
    deriving stock (Show, Eq, Ord)
    deriving Semigroup via Last ParseCoverage

-- | QuickCheck tests for any 'Parsable' type. Currently this only checks
--   'parsableProp'.
parsableQuickCheck :: forall proxy a.
    ( Parsable a PureMode String
    , Printable a
    , Arbitrary a
    , Eq a
    , Show a
    , Typeable a
    ) => proxy a -> STM TestTree
parsableQuickCheck p = do
    c <- newTChan
    pure $ testGroup (show (typeRep p))
        [ testProperty "parsableProp" (parsableProp @a c)
        ]

-- | QuickCheck property that verifies the "round-trip law" of any type
--   which is both 'Parsable' and 'Printable'.
parsableProp :: forall a.
    ( Parsable a PureMode String
    , Printable a
    , Eq a
    , Show a
    ) => TChan String -> a -> Property
parsableProp c x = whenFail (printErrorTChan c) $ idempotentIOProperty $ do
    let s = toString x
    let r = runCheckParsable @a (pack s)
    _ <- either (atomically . writeTChan c) (const (pure ())) r
#if defined(VERBOSE_TESTS)
    pure $ label s $ r === Right (CompleteParse, x)
#else
    pure $ r === Right (CompleteParse, x)
#endif

-- | HUnit tests for a string which should be parsed as a 'Parsable'
--
--   Currently this checks:
--
--   * 'parsableAssertion'
parsableHUnit :: forall proxy a.
    ( Parsable a PureMode String
    , Printable a
    , Show a
    ) => proxy a -> String -> TestTree
parsableHUnit p str = testCase (show str) $ parsableAssertion p str

-- | HUnit tests for a 'Parsable'/'Printable' value
--
--   Currently this checks:
--
--   * 'parsableAssertion'
--   * 'printableAssertion'
printableHUnit :: forall a.
    ( Parsable a PureMode String
    , Printable a
    , Show a
    , Eq a) => a -> TestTree
printableHUnit x = testCase (show str) $ do
    parsableAssertion (Proxy @a) str
    printableAssertion x
    where str = toString x

-- | The string is parsed as a 'Parsable'/'Printable' value, which is then
--   converted back to a string. This HUnit assertion verifies that the
--   resulting string is the same as the original.
parsableAssertion :: forall proxy a.
    ( Parsable a PureMode String
    , Printable a
    , Show a
    ) => proxy a -> String -> Assertion
parsableAssertion _ s = case runCheckParsable @a (pack s) of
    Left e ->
        assertFailure $
            "Could not parse: " ++ s ++ "\n"
            ++ "Error:\n" ++ show e
    Right p@(PartialParse _, _) ->
        assertFailure $
            "Got a PartialParse when it should be a CompleteParse:\n"
            ++ "Output: " ++ show p ++ "\n"
    Right (CompleteParse, x) ->
        assertEqual "resulting string equals original" (toString x) s

-- | The 'Parsable'/'Printable' value is converted to a string, which is then
--   parsed. This HUnit assertion verifies that the parse result is the same as
--   the original value.
printableAssertion :: forall a.
    ( Parsable a PureMode String
    , Printable a
    , Show a
    , Eq a
    ) => a -> Assertion
printableAssertion x = ppAssertion f $ runCheckParsable (pack . toString $ x)
    where f = assertEqual "parse result equals original" x

ppAssertion ::
    ( Show a
    ) => (a -> Assertion)
    -> Either String (ParseCoverage, a)
    -> Assertion
ppAssertion f = \case
    Left e ->
        assertFailure $
            "Could not parse input string:\n"
            ++ "Error:\n" ++ show e
    Right p@(PartialParse _, _) ->
        assertFailure $
            "Got a PartialParse when it should be a CompleteParse:\n"
            ++ "Output: " ++ show p ++ "\n"
    Right (CompleteParse, x) -> f x

extract :: Result String a -> Either String a
extract (OK a _) = Right a
extract (Err e) = Left e
extract Fail = Left "Parsable Tests recoverable failure"

-- | Parses a string as the given 'Parsable' value
runCheckParsable
    :: Parsable a PureMode String
    => ByteString
    -> Either String (ParseCoverage, a)
runCheckParsable = extract . runParser checkParsable

-- | Convenience function that runs 'checkCoverage' on a 'Parsable' parser.
checkParsable
    :: Parsable a st e => ParserT st e (ParseCoverage, a)
checkParsable = checkCoverage parser

-- | Run the specified parser, then return 'CompleteParse' if we are at
--   'eof', otherwise 'PartialParse'.
checkCoverage ::
    ParserT st e a
    -> ParserT st e (ParseCoverage, a)
checkCoverage p = do
    x <- p
    (,x) <$>
      (CompleteParse <$ eof) <|>
      (PartialParse  <$> traceRest)

-- | Generator which takes two lists of predicates which specify valid
--   characters.
--
--   * The first list is specifically for characters that start
--     the string.
--   * The second list is for any subsequent characters.
--
--   This generator always creates a non-empty string.
wordGen :: [Char -> Bool] -> [Char -> Bool] -> Gen String
wordGen wordStart wordRest = do
    c  <-          arbitrary `suchThat` anySat wordStart
    cs <- listOf $ arbitrary `suchThat` anySat wordRest
    pure $ c : cs
  where
    anySat :: [Char -> Bool] -> Char -> Bool
    anySat l x = or [f x | f <- l]


printErrorTChan :: TChan String -> IO ()
printErrorTChan c = fix $ \loop -> do
    me <- atomically $ tryReadTChan c
    case me of
        Just e  -> print e *> loop
        Nothing -> pure ()
