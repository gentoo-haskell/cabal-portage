{-|
Module      : Distribution.Portage.Types.VersionedPkg

Types for Portage packages with specific version constraints
-}

{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language EmptyCase #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language InstanceSigs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}

module Distribution.Portage.Types.VersionedPkg
(   -- * Versioned packages
      VersionedPkg(..)
    , matchVersionedPackage
    ) where

import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)

import Data.Parsable

import Distribution.Portage.Types.Package
import Distribution.Portage.Types.Version

-- | One constructor per operator as defined in section 8.3.1 in the PMS
data VersionedPkg
    = -- | @<@
      VPkgLT { vPkgPackage :: Package, vPkgVersion :: Version }
      -- | @<=@
    | VPkgLE { vPkgPackage :: Package, vPkgVersion :: Version }
      -- | @>@
    | VPkgGT { vPkgPackage :: Package, vPkgVersion :: Version }
      -- | @>=@
    | VPkgGE { vPkgPackage :: Package, vPkgVersion :: Version }
      -- | @=@
    | VPkgEq { vPkgPackage :: Package, vPkgVersion :: Version }
      -- | @~@
    | VPkgEqIgnoreRev { vPkgPackage :: Package, vPkgVersion :: Version }
    -- | "Special exception: if the version specified has an asterisk
    --   immediately following it..." Acts as a version wildcard.
    --
    --   e.g. @=app-misc/blah-1.2*@
    | VPkgEqWildcard Package Version
    deriving stock (Show, Eq, Ord, Data, Generic)

instance Printable VersionedPkg where
    toString (VPkgLT p v) = defVersionedPkgStr "<" p v
    toString (VPkgGT p v) = defVersionedPkgStr ">" p v
    toString (VPkgLE p v) = defVersionedPkgStr "<=" p v
    toString (VPkgGE p v) = defVersionedPkgStr ">=" p v
    toString (VPkgEq p v) = defVersionedPkgStr "=" p v
    toString (VPkgEqIgnoreRev p v) = defVersionedPkgStr "~" p v
    toString (VPkgEqWildcard p v)
        =  "="
        ++ toString p
        ++ "-"
        ++ toString v
        ++ "*"

instance Parsable VersionedPkg st String where
    parserName = "portage versioned package"
    parser :: ParserT st String VersionedPkg
    parser = cut prs e
      where
        e :: String
        e = "Unmatched case when parsing "
            ++ getParserName (parserName @VersionedPkg @st @String)

        prs :: ParserT st String VersionedPkg
        prs = $(switch
            [| case _ of
                "<=" -> uncurry VPkgLE <$> defVersionedPkgParse
                ">=" -> uncurry VPkgGE <$> defVersionedPkgParse
                "<"  -> uncurry VPkgLT <$> defVersionedPkgParse
                ">"  -> uncurry VPkgGT <$> defVersionedPkgParse
                "~"  -> uncurry VPkgEqIgnoreRev <$> defVersionedPkgParse
                "="  -> do
                    (p,v) <- defVersionedPkgParse
                    choice
                        [ $( char '*' ) *> pure (VPkgEqWildcard p v)
                        , pure (VPkgEq p v)
                        ]
            |])

defVersionedPkgStr :: String -> Package -> Version -> String
defVersionedPkgStr s p v =  s ++ toString p ++ "-" ++ toString v

defVersionedPkgParse :: Parsable Package st e => ParserT st e (Package, Version)
defVersionedPkgParse = do
    p <- parser
    _ <- $( char '-' )
    v <- parser
    pure (p,v)

matchVersionedPackage :: VersionedPkg -> Package -> Version -> Bool
matchVersionedPackage vp pkg ver =
    case vp of
         VPkgLT p v -> pkg == p && ver < v
         VPkgGT p v -> pkg == p && ver > v
         VPkgLE p v -> pkg == p && ver <= v
         VPkgGE p v -> pkg == p && ver >= v
         VPkgEq p v -> pkg == p && ver == v
         VPkgEqIgnoreRev p v ->
            pkg == p &&
                   ver { getVersionRevision = Nothing }
                == v { getVersionRevision = Nothing }
         VPkgEqWildcard p v ->
            let getVNL = unwrapVersionNum . getVersionNum
            in pkg == p
                -- do both version lists match (up to the length of v)?
                -- e.g. 1.2* matches 1.2.3.4
                && and (NE.zipWith (==) (getVNL ver) (getVNL v))
