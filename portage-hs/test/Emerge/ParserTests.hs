{-# Language LambdaCase #-}

module Emerge.ParserTests
    ( parserTests
    ) where

import Conduit
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import Data.Parsable
import Distribution.Portage.Types
import Distribution.Portage.Emerge
import Distribution.Portage.Emerge.Parser
import Test.Parsable

import Paths_portage_hs (getDataFileName)

parserTests :: IO TestTree
parserTests = do
    stdouts <- emergeStdOuts
    pure $ testGroup "test against recorded emerge output" $
        map (uncurry stdoutTest) stdouts

stdoutTest :: FilePath -> StdOut -> TestTree
stdoutTest d o =
    case ecis of
        Left e ->
            testCase d $ assertFailure $
                "Could not parse recorded emerge ouptut:\n"
                ++ "Error:\n" ++ show e
        Right (c, (i,ps)) -> testGroup d
            [ testCase "complete coverage" (checkPC c)
            , testCase "package count" (checkCount i ps)
            , testGroup "round-trip on extracted packages" $
                printableHUnit <$> S.toList ps
            ]
  where
    checkPC :: ParseCoverage -> Assertion
    checkPC = \case
        c@(PartialParse _) ->
            assertFailure $
                "Got a PartialParse when it should be a CompleteParse:\n"
                ++ show c ++ "\n"
        _ -> pure ()

    checkCount :: Integer -> Set Package -> Assertion
    checkCount i ps = assertEqual "reported package count should equal set size" (fromInteger i) (S.size ps)

    ecis :: Either String (ParseCoverage, (Integer, Set Package))
    ecis = extract . runParser (checkCoverage emergeParser) . encodeUtf8 $ o
    
    extract (OK a _) = Right a
    extract (Err e) = Left e
    extract Fail = Left "Parser Tests recoverable failure"

emergeStdOuts :: IO [(FilePath, StdOut)]
emergeStdOuts = do
    dataDir <- getDataFileName ("test" </> "data") >>= canonicalizePath
    runConduitRes
        $  sourceDirectoryDeep False dataDir
        .| filterC (\f -> takeFileName f == validFile)
        .| (await >>= mapM_ readFileC)
        .| sinkList
  where
    validFile = "emerge-world-test" <.> "stdout"

    readFileC :: FilePath -> ConduitT i (FilePath, StdOut) (ResourceT IO) ()
    readFileC f = do
        o <- liftIO $ T.readFile f
        yield (f, o)
