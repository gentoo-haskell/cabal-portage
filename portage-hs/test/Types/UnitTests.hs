{-|
Module: Types.UnitTests

Custom-written unit tests for testing basic functionality or
input that has caused problems in the past.
-}

{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module Types.UnitTests (unitTests) where

import Data.List.NonEmpty (NonEmpty(..))
import Test.Tasty
import Test.Tasty.HUnit
import Data.ByteString.Char8 (pack)

import Distribution.Portage.Types
import Data.Parsable
import Test.Parsable

unitTests :: TestTree
unitTests = testGroup "unit tests"
    [ testGroup "basic tests"
        [ "dev-python/nose" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
        , "=dev-python/nose-1" `parserTest`
            VPkgEq
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Version
                    (VersionNum (('1':|[]):|[]))
                    Nothing
                    []
                    Nothing
                )
        , "=dev-python/nose-1.23a-r123" `parserTest`
            VPkgEq
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    (Just (VersionLetter 'a'))
                    []
                    (Just (VersionRevision ('1':|"23")))
                )
        , "dev-python/nose:1337" `parserTest`
            UnversionedDepSpec
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                Nothing
                (Just (Slot "1337" Nothing))
                Nothing
        , "dev-python/nose::uberRepo" `parserTest`
            UnversionedDepSpec
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                Nothing
                Nothing
                (Just (Repository "uberRepo"))
        ,  "=dev-python/nose-1.23a-r123:1337::uberRepo" `parserTest`
            VersionedDepSpec
                (VPkgEq
                    (Package
                        (Category "dev-python")
                        (PkgName "nose")
                    )
                    (Version
                        (VersionNum (('1':|[]):|['2':|"3"]))
                        (Just (VersionLetter 'a'))
                        []
                        (Just (VersionRevision ('1':|"23")))
                    )
                )
                Nothing
                (Just (Slot "1337" Nothing))
                (Just (Repository "uberRepo"))
        , "=dev-python/nose-1.23*" `parserTest`
            VPkgEqWildcard
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    Nothing
                    []
                    Nothing
                )
        , "=dev-python/nose-1.23*:*" `parserTest`
            VersionedDepSpec
                (VPkgEqWildcard
                    (Package
                        (Category "dev-python")
                        (PkgName "nose")
                    )
                    (Version
                        (VersionNum (('1':|[]):|['2':|"3"]))
                        Nothing
                        []
                        Nothing
                    )
                )
                Nothing
                (Just AnySlot)
                Nothing
        , "[profile?,-test]" `parserTest`
            UseDependency
                (UseDepMatchIfEnabled "profile" Nothing
                :| [UseDepDisabled "test" Nothing]
                )
        , "[!profile(+)?,test(-)]" `parserTest`
            UseDependency
                (UseDepMatchIfDisabled "profile" (Just UseDefaultEnabled)
                :| [UseDepEnabled "test" (Just UseDefaultDisabled)]
                )
        ]
    , testGroup "tricky tests"
        [ "=dev-python/nose-1.3.7_p20211111_p1" `parserTest`
            VPkgEq
                (Package
                    (Category "dev-python")
                    (PkgName "nose")
                )
                (Version
                    (VersionNum
                        (('1':|[]) :| ['3':|[],'7':|[]])
                    )
                    Nothing
                    [ (SuffixP, Just (VersionSuffixNum ('2' :| "0211111")))
                    , (SuffixP, Just (VersionSuffixNum ('1' :| [])))
                    ]
                    Nothing
                )
        , "=app-text/xdvik-22.87.03-r3" `parserTest`
            VPkgEq
                (Package
                    (Category "app-text")
                    (PkgName "xdvik")
                )
                (Version
                    (VersionNum
                        (('2':|"2") :|
                            ['8':|"7"
                            ,'0':|"3"
                            ]
                        )
                    )
                    Nothing
                    []
                    (Just (VersionRevision ('3':|[])))
                )
        , "=dev-java/log4j-12-api-2.17.2" `parserTest`
            VPkgEq
                (Package
                    (Category "dev-java")
                    (PkgName "log4j-12-api")
                )
                (Version
                    (VersionNum
                        (('2':|[]) :|
                            ['1':|"7"
                            ,'2':|[]
                            ]
                        )
                    )
                    Nothing
                    []
                    Nothing
                )
        , "=sec-keys/openpgp-keys-gawk-20220407" `parserTest`
            VPkgEq
                (Package
                    (Category "sec-keys")
                    (PkgName "openpgp-keys-gawk")
                )
                (Version
                    (VersionNum (('2':|"0220407"):|[]))
                    Nothing
                    []
                    Nothing
                )
        , "=app-derp/f--98---54.321x_pre1:gawk--::goo" `parserTest`
            VersionedDepSpec
                (VPkgEq
                    (Package
                        (Category "app-derp")
                        (PkgName "f--98--")
                    )
                    (Version
                        (VersionNum (('5':|"4") :| ['3':|"21"]))
                        (Just (VersionLetter 'x'))
                        [ (SuffixPre, Just (VersionSuffixNum ('1' :| []))) ]
                        Nothing
                    )
                )
                Nothing
                (Just (Slot "gawk--" Nothing))
                (Just (Repository "goo"))
        , "=app-office/hledger-web-1.26.1[profile(+)?,-test]:0/1.26.1=::haskell" `parserTest`
            VersionedDepSpec
                (VPkgEq
                    (Package
                        (Category "app-office")
                        (PkgName "hledger-web")
                    )
                    (Version
                        (VersionNum (('1':|[]) :| ['2':|"6", '1':|[]]))
                        Nothing
                        []
                        Nothing
                    )
                )
                (Just (UseDependency
                    (UseDepMatchIfEnabled "profile" (Just UseDefaultEnabled)
                        :| [UseDepDisabled "test" Nothing]
                    )
                ))
                (Just (SlotBreakable "0" (Just (SubSlot "1.26.1"))))
                (Just (Repository "haskell"))
        ]
    , testGroup "failed QuickCheck tests"
        [ "7-r2b" `parserTest` Repository "7-r2b"
        , "7-r2b" `parserTest` PkgName "7-r2b"
        , "1.3.7_p20211111_p1" `parserTest` Version
                    (VersionNum
                        (('1':|[]) :| ['3':|[],'7':|[]])
                    )
                    Nothing
                    [ (SuffixP, Just (VersionSuffixNum ('2' :| "0211111")))
                    , (SuffixP, Just (VersionSuffixNum ('1' :| [])))
                    ]
                    Nothing
        , "p" `parserTest` SuffixP
        , "F/z-tNk[s(+)?]::v" `parserTest`
            UnversionedDepSpec
                (Package
                    (Category "F")
                    (PkgName "z-tNk")
                )
                (Just (UseDependency
                    (UseDepMatchIfEnabled (UseFlag "s") (Just UseDefaultEnabled)
                    :| []
                    )
                ))
                Nothing
                (Just (Repository "v"))
        ]
    ]

-- Takes a string and an expected result, and creates a TestTree from them
parserTest :: forall a.
    ( Parsable a PureMode String
    , Eq a
    , Show a
    ) => String
    -> a
    -> TestTree
parserTest s x = testCase (show s) $
    Right (CompleteParse, x) @=? extractResult (runParser (checkParsable @a @PureMode @String) (pack s))

