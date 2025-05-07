{-# Language ApplicativeDo #-}
{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Portage.Emerge.Parser
    ( emergeParser
    ) where

-- import Control.Monad.Combinators
import Data.Coerce
import Data.Monoid (Last(..))
-- import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Set (Set)
-- import Text.Parsec.Language (haskell)
-- import Text.Parsec.Token (GenTokenParser(decimal))
import Text.Read (readMaybe)

import Data.Parsable
import Distribution.Portage.Types

sepEndBy :: ParserT st e a -> ParserT st e sep -> ParserT st e [a]
sepEndBy p sep = (do 
  x <- p
  xs <- many (sep >> p)
  optional_ sep
  pure (x:xs)) <|> pure []

-- | Returns a set of packages and the count as reported by the emerge output
emergeParser :: Parsable Package st String 
    => ParserT st String (Integer, Set Package)
emergeParser = do
    r <- sepEndBy lineParser ($(char '\n'))
    case coerce $ foldMap go r of
        (Nothing, _) -> err e
        (Just i , s) -> pure (i, s)
  where
    go :: EmergeLine -> (Last Integer, Set Package)
    go = coerce . \case
        PackageLine p -> (Nothing, S.singleton p)
        TotalLine   i -> (Just i , S.empty      )
        OtherLine     -> (Nothing, S.empty      )

    e :: String
    e = "emerge output contains a package total"

data EmergeLine
    = PackageLine Package
    | TotalLine Integer
    | OtherLine
    deriving (Show, Eq, Ord)


lineParser :: Parsable Package st String 
    => ParserT st String EmergeLine
lineParser =  
  (PackageLine <$> packageLine) <|> (TotalLine <$> totalLine) <|> (OtherLine <$ skipRest)

packageLine :: Parsable Package st e => ParserT st e Package
packageLine = do
    _ <- space
    $(char '[')
    _ <- many $ satisfy (\c -> c /= ']' && c /= '\n')
    $(char ']')
    _ <- space
    pkg <- parser
    _ <- space
    _ <- skipRest
    pure pkg

totalLine :: ParserT st String Integer
totalLine = do
    $(string "Total:")
    _ <- space
    ds <- some (satisfy isDigit)
    _ <- space
    _ <- $(string "packages")
    _ <- skipRest
    case readMaybe ds of
        Just n  -> pure n
        Nothing -> err $ "Could not parse as Integer: " ++ show ds

skipRest :: ParserT st e ()
skipRest = void $ many $ satisfy (/= '\n')

space :: ParserT st e ()
space = void $ many $ satisfy (\c -> c == ' ' || c == '\t')
