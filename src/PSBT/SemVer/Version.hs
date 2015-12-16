-- | Module      : PSBT.SemVer.Version
--   Description : SemVer version parsing

module PSBT.SemVer.Version (
    Version(..)
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

-- | Datatype for a SemVer version
data Version = Version {
    major          :: Integer
    , minor          :: Integer
    , patch          :: Integer
    , prereleaseTags :: [String]
    , buildTags      :: [String]
    } deriving (Eq, Show)

-- | The empty version
emptyVersion :: Version
emptyVersion = Version False 0 0 0 [] []

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

prereleaseId :: Parser String
prereleaseId = try numericId <|> alphanumId

prerelease :: Parser [String]
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

-- | Gets the textual representation of a Version
displayVersion :: Version -> String
displayVersion (Version maj min pat pre build) = 
    show maj ++ "." ++
    show min ++ "." ++ 
    show pat ++
    (if null pre then "" else '-' : intercalate "." pre) ++
    (if null build then "" else '+' : intercalate "." build)
