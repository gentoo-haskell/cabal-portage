{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module Types.GentooTests (gentooTests) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Proxy
import ListT (ListT, fromFoldable, toList)
import System.Directory
import System.FilePath.Posix
import Test.Tasty
import Test.Tasty.HUnit

import Internal.Distribution.Portage.Types
import Data.Parsable
import Test.Parsable

gentooTests :: IO TestTree
gentooTests = testGroup "strings from live Gentoo system" <$> sequenceA
    [ gentooParseTests "/var/db/repos parse tests" <$> repoTree
    , gentooParseTests "/var/db/pkg parse tests" <$> pkgTree
    ]

-- | Check all package atoms and make sure they are parsed successfully and
--   pass the "roundtrip" test.
gentooParseTests :: TestName -> [String] -> TestTree
gentooParseTests n pkgStrings = testGroup n $
    parsableHUnit (Proxy @Package) <$> pkgStrings

-- | Scan the entire @/var/db/repos@ tree for ebuilds and form these into
--  pacakge atoms.
repoTree :: IO [String]
repoTree = toListT $ do
    let reposDir = "/var/db/repos"
    checkDir reposDir $ do

        repo <- listDir reposDir
        cat  <- listDir $ reposDir </> repo
        n    <- listDir $ reposDir </> repo </> cat
        eb   <- listDir $ reposDir </> repo </> cat </> n

        let (nv,ext) = splitExtension eb

        guard $ ext == ".ebuild"

        pure $ cat ++ "/" ++ nv ++ "::" ++ repo

-- | Scan the entire @/var/db/pkg@ tree for package atoms in the form
--   @<category>/<pkg-name>-<version>@. This is inherently part of the directory
--   structure inside @/var/db/pkg@.
pkgTree :: IO [String]
pkgTree = toListT $ do
    let portagePkgDir = "/var/db/pkg"
    checkDir portagePkgDir $ do

        cat <- listDir portagePkgDir           -- Scan for categories
        nv  <- listDir $ portagePkgDir </> cat -- Scan for names/versions

        case nv of
            '.' : _ -> empty
            '-' : _ -> empty
            []      -> empty
            _       -> pure $ cat ++ "/" ++ nv

listDir :: FilePath -> ListT IO FilePath
listDir d = do
    lift (doesDirectoryExist d) >>= guard
    listDirectory d >>= fromFoldable

checkDir :: FilePath -> ListT IO a -> ListT IO a
checkDir d rest = do
    b <- lift $ doesDirectoryExist d
    if b
        then rest
        else lift $ assertFailure $
                show d ++ " is not a directory. Is this really a Gentoo system?"
