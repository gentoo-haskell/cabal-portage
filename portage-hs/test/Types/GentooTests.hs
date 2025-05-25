{-|
Module: Types.GentooTests

Tests that require a running Gentoo system to test against.
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}

module Types.GentooTests (gentooTests) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Data (Data)
import Data.Proxy
import GHC.Generics (Generic)
import ListT (ListT, fromFoldable, toList)
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import Distribution.Portage.Types
import Data.Parsable
import Test.Parsable

gentooTests :: IO TestTree
gentooTests = testGroup "strings from live Gentoo system" <$> sequenceA
    [ do
        hPutStrLn stderr "Gathering info for /var/db/repos parse tests..."
        gentooParseTests "/var/db/repos parse tests" <$> repoTree
    , do
        hPutStrLn stderr "Gathering info for /var/db/pkg parse tests..."
        gentooParseTests "/var/db/pkg parse tests" <$> pkgTree
    , do
        hPutStrLn stderr "Gathering info for pquery dump parse tests..."
        dump <- getPqueryDump
        hPutStrLn stderr $ "Dump size: " ++ show (length dump) ++ " lines"
        pure $ pqueryParseTests dump
    ]

-- | Check all package atoms and make sure they are parsed successfully and
--   pass the "roundtrip" test.
gentooParseTests :: TestName -> [String] -> TestTree
gentooParseTests n pkgStrings = testGroup n $
    parsableHUnit (Proxy @DepSpec) <$> pkgStrings

pqueryParseTests :: [ByteString] -> TestTree
pqueryParseTests bStrings = testGroup "pquery output tests" $
    flatparseHUnit <$> zip [1..] bStrings
  where
    flatparseHUnit :: (Int, ByteString) -> TestTree
    flatparseHUnit (i,bs) = testCaseSteps ("test #" ++ show i) $ \step -> do
        step "Parsing pquery line"

        let pErr s = assertFailure $ unlines
                [ "Error when parsing pquery line: " ++ s
                , "input: " ++ utf8ToStr bs ]

        case runParser (parser @PqueryTestEntry) bs of
            Fail -> pErr "parser returned an uncaught failure"
            Err e -> pErr $ "Parsing entry retuned error: " ++ e
            OK (PqueryTestEntry ds dep rdep bdep pdep idep) rest
                | not (BS.null rest) -> pErr
                    $  "Parser did not fully consume input. Remainder: "
                    ++ utf8ToStr rest
                | otherwise -> do
                    step $ "Good: " ++ toString ds

                    testStep step "DEPEND" dep
                    testStep step "RDEPEND" rdep
                    testStep step "BDEPEND" bdep
                    testStep step "PDEPEND" pdep
                    testStep step "IDEPEND" idep

    testStep :: (Parsable a PureMode String, Printable a, Show a, Eq a)
        => (String -> IO ()) -> String -> a -> IO ()
    testStep step name item = do
        step $ "\nChecking " ++ name
        printableAssertion item
        step $ "Good: " ++ toString item

-- | Scan the entire @/var/db/repos@ tree for ebuilds and form these into
--  pacakge atoms.
repoTree :: IO [String]
repoTree = toList $ do
    let reposDir = "/var/db/repos"
    checkDir reposDir $ do

        repo <- listDir reposDir
        cat  <- listDir $ reposDir </> repo
        n    <- listDir $ reposDir </> repo </> cat
        eb   <- listDir $ reposDir </> repo </> cat </> n

        let (nv,ext) = splitExtension eb

        guard $ ext == ".ebuild"

        pure $ "=" ++ cat ++ "/" ++ nv

-- | Scan the entire @/var/db/pkg@ tree for package atoms in the form
--   @<category>/<pkg-name>-<version>@. This is inherently part of the directory
--   structure inside @/var/db/pkg@.
pkgTree :: IO [String]
pkgTree = toList $ do
    let portagePkgDir = "/var/db/pkg"
    checkDir portagePkgDir $ do

        cat <- listDir portagePkgDir           -- Scan for categories
        nv  <- listDir $ portagePkgDir </> cat -- Scan for names/versions

        case nv of
            '.' : _ -> empty
            '-' : _ -> empty
            []      -> empty
            _       -> pure $ "=" ++ cat ++ "/" ++ nv

listDir :: FilePath -> ListT IO FilePath
listDir d = do
    lift (doesDirectoryExist d) >>= guard
    lift (listDirectory d) >>= fromFoldable

checkDir :: FilePath -> ListT IO a -> ListT IO a
checkDir d rest = do
    b <- lift $ doesDirectoryExist d
    if b
        then rest
        else lift $ assertFailure $
                show d ++ " is not a directory. Is this really a Gentoo system?"

data PqueryTestEntry = PqueryTestEntry
    { pkgDepsDepSpec :: DepSpec
    , pkgDepsDepend :: DepBlock
    , pkgDepsRDepend :: DepBlock
    , pkgDepsBDepend :: DepBlock
    , pkgDepsPDepend :: DepBlock
    , pkgDepsIDepend :: DepBlock
    }
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Parsable PqueryTestEntry st String where
    parserName = "pquery output test entry"
    parser = PqueryTestEntry
        <$> parser
        <*> ( $( string " depend=\""    ) *> parser )
        <*> ( $( string "\" rdepend=\"" ) *> parser )
        <*> ( $( string "\" bdepend=\"" ) *> parser )
        <*> ( $( string "\" pdepend=\"" ) *> parser )
        <*> ( $( string "\" idepend=\"" ) *> parser <* $( char '"' ) )

getPqueryDump :: IO [ByteString]
getPqueryDump = do
    fp <- pqueryPath
    (Nothing, Just outH, Just errH, ph) <- createProcess (cp fp)
    waitForProcess ph >>= \case
        ExitFailure ec -> do
            outBS <- hGetContents outH
            errBS <- hGetContents errH
            fail $ unlines
                [ "Failure when running pquery: Got exit code " ++ show ec
                , "stdout: " ++ show outBS
                , "stdout: " ++ show errBS
                ]
        ExitSuccess -> BS.lines <$> BS.hGetContents outH

  where
    -- | Why @llvm-core/*@ ? It takes a long time for pquery to dump the whole
    --   repo. @llvm-core/*@ is relatively small with a complex list of
    --   dependencies which are ideal to test against.
    args =
        [ "--repo", "gentoo"
        , "--atom"
        , "--cpv"
        , "--slot"
        , "--attr", "depend"
        , "--attr", "rdepend"
        , "--attr", "bdepend"
        , "--attr", "pdepend"
        , "--attr", "idepend"
        , "llvm-core/*"
        ]

    cp fp = (proc fp args)
        { std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

pqueryPath :: IO FilePath
pqueryPath = findExecutable "pquery" >>= \case
    Just fp -> pure fp
    Nothing -> fail
        "Could not find pquery executable in $PATH. Install sys-apps/pkgcore first."
