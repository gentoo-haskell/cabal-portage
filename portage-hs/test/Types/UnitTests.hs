{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}

module Types.UnitTests (unitTests) where

import Data.List.NonEmpty (NonEmpty(..))
import Test.Tasty
import Test.Tasty.HUnit
import Data.ByteString.Char8 (pack)

import Internal.Distribution.Portage.Types
import Data.Parsable
import Test.Parsable

unitTests :: TestTree
unitTests = testGroup "unit tests"
    [ testGroup "basic tests"
        [ "dev-python/nose" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                Nothing
                Nothing
                Nothing
        , "dev-python/nose-1" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                (Just (Version
                    (VersionNum (('1':|[]):|[]))
                    Nothing
                    []
                    Nothing
                ))
                Nothing
                Nothing
        , "dev-python/nose-1.23a-r123" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                (Just (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    (Just (VersionLetter 'a'))
                    []
                    (Just (VersionRevision ('1':|"23")))
                ))
                Nothing
                Nothing
        , "dev-python/nose:1337" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                Nothing
                (Just (Slot "1337" Nothing))
                Nothing
        , "dev-python/nose::uberRepo" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                Nothing
                Nothing
                (Just (Repository "uberRepo"))
        ,  "dev-python/nose-1.23a-r123:1337::uberRepo" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                (Just (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    (Just (VersionLetter 'a'))
                    []
                    (Just (VersionRevision ('1':|"23")))
                ))
                (Just (Slot "1337" Nothing))
                (Just (Repository "uberRepo"))
        , "=dev-python/nose-1.23*:1337::uberRepo" `parserTest`
            ConstrainedDep
                EqualAsterisk
                (Category "dev-python")
                (PkgName "nose")
                (Version
                    (VersionNum (('1':|[]):|['2':|"3"]))
                    Nothing
                    []
                    Nothing
                )
                (Just (Slot "1337" Nothing))
                (Just (Repository "uberRepo"))
        ]
    , testGroup "tricky tests"
        [ "dev-python/nose-1.3.7_p20211111_p1" `parserTest`
            Package
                (Category "dev-python")
                (PkgName "nose")
                (Just (Version
                    (VersionNum
                        (('1':|[]) :| ['3':|[],'7':|[]])
                    )
                    Nothing
                    [ (SuffixP, Just (VersionSuffixNum ('2' :| "0211111")))
                    , (SuffixP, Just (VersionSuffixNum ('1' :| [])))
                    ]
                    Nothing
                ))
                Nothing
                Nothing
        , "app-text/xdvik-22.87.03-r3" `parserTest`
            Package
                (Category "app-text")
                (PkgName "xdvik")
                (Just (Version
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
                ))
                Nothing
                Nothing
        , "dev-java/log4j-12-api-2.17.2" `parserTest`
            Package
                (Category "dev-java")
                (PkgName "log4j-12-api")
                (Just (Version
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
                ))
                Nothing
                Nothing
        , "sec-keys/openpgp-keys-gawk-20220407" `parserTest`
            Package
                (Category "sec-keys")
                (PkgName "openpgp-keys-gawk")
                (Just (Version
                    (VersionNum (('2':|"0220407"):|[]))
                    Nothing
                    []
                    Nothing
                ))
                Nothing
                Nothing
        , "app-derp/f--98---54.321x_pre1:gawk--::goo" `parserTest`
            Package
                (Category "app-derp")
                (PkgName "f--98--")
                (Just (Version
                    (VersionNum (('5':|"4") :| ['3':|"21"]))
                    (Just (VersionLetter 'x'))
                    [ (SuffixPre, Just (VersionSuffixNum ('1' :| []))) ]
                    Nothing
                ))
                (Just (Slot "gawk--" Nothing))
                (Just (Repository "goo"))
        , "app-office/hledger-web-1.26.1:0/1.26.1::haskell" `parserTest`
            Package
                (Category "app-office")
                (PkgName "hledger-web")
                (Just (Version
                    (VersionNum (('1':|[]) :| ['2':|"6", '1':|[]]))
                    Nothing
                    []
                    Nothing
                ))
                (Just (Slot "0" (Just (SubSlot "1.26.1"))))
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
--         , "7-r4-8y-r3-o-7-r0-a-4.ebuild" `parserTest` EBuildFileName
--                     (PkgName "7-r4-8y-r3-o-7-r0-a")
--                     (Version
--                         (VersionNum
--                             (('4' :| "") :| [])
--                         )
--                         Nothing
--                         []
--                         Nothing
--                     )
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
    Right (CompleteParse, x) @=? extract (runParser (checkParsable @a) (pack s))
      where
        extract :: Result String (ParseCoverage, a) -> Either String (ParseCoverage, a)
        extract (OK a _) = Right a
        extract (Err e) = Left e
        extract Fail = Left "Unit Tests recoverable failure"

