-- | Module      : PSBT.SemVer.Version
--   Description : SemVer version parsing

module PSBT.SemVer.Version (
    Identifier(..)
    , Version(..)
    , displayIdentifier
    , displayVersion
    , emptyVersion
    , version
) where

import Control.Applicative ((<|>), many, optional, some)
import Data.List (intercalate)
import Text.Megaparsec (alphaNumChar, between, choice, digitChar, eof,
                        letterChar, try, sepBy1, spaceChar)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String

import PSBT.SemVer.Util

data Identifier = Num Int
           | AlphaNum String
           deriving (Eq, Show)

-- | Datatype for a SemVer version
data Version = Version {
    major          :: Integer
    , minor          :: Integer
    , patch          :: Integer
    , prereleaseTags :: [Identifier]
    , buildTags      :: [String]
    } deriving (Eq, Show)

instance Ord Identifier where
    compare (Num n) (Num m) = compare n m
    compare (Num n) (AlphaNum xs) = LT
    compare (AlphaNum xs) (Num n) = GT
    compare (AlphaNum xs) (AlphaNum ys) = compare xs ys

instance Ord Version where
    compare v1 v2
      | major v1 > major v2 = GT
      | major v1 < major v2 = LT
      | minor v1 > minor v2 = GT
      | minor v1 < minor v2 = LT
      | patch v1 > patch v2 = GT
      | patch v1 < patch v2 = LT
      | null (prereleaseTags v1) = GT
      | null (prereleaseTags v2) = LT
      | otherwise = compare (prereleaseTags v1) (prereleaseTags v2)

-- | The empty version
emptyVersion :: Version
emptyVersion = Version 0 0 0 [] []

numericId :: Parser String
numericId = try case1 <|> try case2 <|> case3
  where
    case1 = (:) <$> positiveDigit <*> some digitChar
    case2 = return <$> positiveDigit
    case3 = return <$> char '0'

nonDigit :: Parser Char
nonDigit = letterChar <|> char '-'

identifier :: Parser Char
identifier = digitChar <|> nonDigit

alphanumId :: Parser String
alphanumId = try case1 <|> try case2 <|> try case3 <|> case4
  where
    case1 = (++) <$> some identifier <*> (return <$> nonDigit)
    case2 = (:) <$> nonDigit <*> some identifier
    case3 = (++) <$> some identifier <*> case2
    case4 = return <$> nonDigit

prereleaseId :: Parser Identifier
prereleaseId = try (Num . read <$> numericId) <|> (AlphaNum <$> alphanumId)

prerelease :: Parser [Identifier]
prerelease = sepBy1 prereleaseId (char '.')

versionCore :: Parser Version
versionCore = do
    maj <- read <$> numericId
    char '.'
    min <- read <$> numericId
    char '.'
    pat <- read <$> numericId
    return emptyVersion { major = maj, minor = min, patch = pat }

buildId :: Parser String
buildId = alphanumId <|> some digitChar

build :: Parser [String]
build = sepBy1 buildId (char '.')

withBuild :: Parser Version -> Parser Version
withBuild pver = do
    ver <- pver
    char '+'
    bs <- build
    return $ ver { buildTags = bs }

verPrerelease :: Parser Version
verPrerelease = do
    ver <- versionCore
    char '-'
    pres <- prerelease
    return $ ver { prereleaseTags = pres }

verPreAndBuild :: Parser Version
verPreAndBuild = withBuild verPrerelease

verBuild :: Parser Version
verBuild = withBuild versionCore

-- | Parser for a SemVer version
version :: Parser Version
version = try verPreAndBuild
    <|> try verPrerelease
    <|> try verBuild 
    <|> versionCore

displayIdentifier :: Identifier -> String
displayIdentifier (Num n) = show n
displayIdentifier (AlphaNum xs) = xs

-- | Gets the textual representation of a Version
displayVersion :: Version -> String
displayVersion (Version maj min pat pre build) = 
    show maj ++ "." ++
    show min ++ "." ++ 
    show pat ++
    (if null pre then "" else '-' : intercalate "." (map displayIdentifier pre)) ++
    (if null build then "" else '+' : intercalate "." build)
