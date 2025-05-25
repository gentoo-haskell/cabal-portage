{-|
Module      : Distribution.Portage.Types

Types for Portage atoms, etc.
-}

{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingVia #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Distribution.Portage.Types
    (
    -- * Full Dependency spec
      DepSpec(..)
    -- * Types
    , Package(..)
    , Category(..)
    , PkgName(..)
    , Slot(..)
    , SubSlot(..)
    , Repository(..)
    -- ** Versions
    , Version(..)
    , VersionNum(..)
    , VersionLetter(..)
    , VersionSuffix(..)
    , VersionSuffixNum(..)
    , VersionRevision(..)
    -- ** Versioned packages
    , VersionedPkg(..)
    , matchVersionedPackage
    -- * USE dependencies
    , UseDependency(..)
    , UseDep(..)
    , UseDepDefault(..)
    , UseFlag(..)
    -- * Dependency groups
    , DepGroup(..)
    , DepBlock(..)
    -- * Internal
    , FauxVersion(..)
    , FauxVersionNum(..)
    ) where

import Distribution.Portage.Types.Package
import Distribution.Portage.Types.Misc
import Distribution.Portage.Types.UseDep
import Distribution.Portage.Types.Version
import Distribution.Portage.Types.VersionedPkg
import Distribution.Portage.Types.DepSpec
import Distribution.Portage.Types.DepGroup
