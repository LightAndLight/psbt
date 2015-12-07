module PSBT.SemVer (
    Range(..)
    , Version(..)
    , displaySemVer
    , displayRange
    , emptyVersion
    , semVer
    , semVerRange
) where

import Control.Applicative ((<|>), many, optional, some)
import Data.List (intercalate)
import Text.Megaparsec (alphaNumChar, between, choice, digitChar, eof,
                        letterChar, try, sepBy1, spaceChar)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String

data Version = Version {
    useLatest        :: Bool
    , major          :: Integer
    , minor          :: Integer
    , patch          :: Integer
    , prereleaseTags :: [String]
    , buildTags      :: [String]
    } deriving (Eq, Show)

emptyVersion :: Version
emptyVersion = Version False 0 0 0 [] []

data Range = Lt Version
           | Lte Version
           | Gt Version
           | Gte Version
           | E Version
           | And Range Range
           | Or Range Range
           deriving (Eq, Show)

lt :: Parser Range
lt = char '<' *> (Lt <$> semVer)

lte :: Parser Range
lte = string "<=" *> (Lte <$> semVer)

gt :: Parser Range
gt = char '>' *> (Gt <$> semVer)

gte :: Parser Range
gte = string ">=" *> (Gte <$> semVer)

e :: Parser Range
e = char '=' *> (E <$> semVer)

operator :: Parser Range
operator = try lte <|> try gte <|> try lt <|> try gt <|> try e <|> (E <$> semVer)

andRange :: Parser Range
andRange = do
    r1 <- try (between (char '(') (char ')') orRange)
        <|> try operator <|> wildcard
    some spaceChar
    r2 <- try (between (char '(') (char ')') orRange)
        <|> try andRange <|> try operator <|> wildcard
    return $ And r1 r2

orRange :: Parser Range
orRange = do
    r1 <- try andRange <|> try operator <|> wildcard
    between (some spaceChar) (some spaceChar) (string "||")
    r2 <- try orRange <|> try andRange <|> try operator <|> wildcard
    return $ Or r1 r2

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

positiveDigit :: Parser Char
positiveDigit = char '1'
            <|> char '2'
            <|> char '3' 
            <|> char '4' 
            <|> char '5' 
            <|> char '6' 
            <|> char '7' 
            <|> char '8' 
            <|> char '9' 

numericId :: Parser String
numericId = try case1 <|> try case2 <|> case3
  where
    case1 = (:) <$> positiveDigit <*> some digitChar
    case2 = return <$> positiveDigit
    case3 = return <$> char '0'

prereleaseId :: Parser String
prereleaseId = try numericId <|> alphanumId

prerelease :: Parser [String]
prerelease = sepBy1 prereleaseId (char '.')

-- 1, 2 etc.
withMajor :: Parser Version
withMajor = do
    maj <- read <$> numericId
    return $ Version False maj 0 0 [] []

-- 1.1, 3.0 etc.
withMinor :: Version -> Parser Version
withMinor v = do
    char '.'
    min <- read <$> numericId
    return $ v { minor = min }

withPatch :: Version -> Parser Version
withPatch v = do
    char '.'
    pat <- read <$> numericId
    return $ v { patch = pat }

wildcardChar :: Parser Char
wildcardChar = char 'x' <|> char 'X' <|> char '*'

-- x, *, X
majorX :: Parser Range
majorX = do
    optional wildcardChar
    eof
    return . Gte $ Version False 0 0 0 [] []

-- 1.x, 1, 2 etc.
minorX :: Parser Range
minorX = do
    ver <- withMajor
    optional $ do
        char '.'
        wildcardChar
    return $ And (Gte ver) (Lt ver { major = major ver + 1})

-- 1.1.x, 1.3 etc.
patchX :: Parser Range
patchX = do
    ver <- withMinor =<< withMajor
    optional $ do
        char '.'
        wildcardChar
    return $ And (Gte ver) (Lt ver { minor = minor ver + 1})

withMinorPatchWildcards :: Version -> Parser Version
withMinorPatchWildcards ver = do
    optional $ do
        char '.'
        wildcardChar
        optional $ do
            char '.'
            wildcardChar
    return $ ver { minor = 0, patch = 0 }

withPatchWildcard :: Version -> Parser Version
withPatchWildcard ver = do
    optional $ do
        char '.'
        wildcardChar
    return $ ver { patch = 0 }

wildcard :: Parser Range
wildcard = try patchX <|> try minorX <|> majorX

createCaretRange :: Version -> Range
createCaretRange ver
  | major ver == 0
    && minor ver == 0 = And (Gte ver) (Lt ver { patch = patch ver + 1})
  | major ver == 0 = And (Gte ver) (Lt ver { minor = minor ver + 1
                                           , patch = 0})
  | otherwise = And (Gte ver) (Lt ver { major = major ver + 1
                                      , minor = 0
                                      , patch = 0})
    
caretRange :: Parser Range
caretRange = do
    char '^'
    ver <- try versionCore <|> (do
        ver' <- withMajor
        try (withMinorPatchWildcards ver')
            <|> (withMinor ver' >>= withPatchWildcard))
    return . createCaretRange $ ver

createTildeRange :: Version -> Range
createTildeRange ver
  | minor ver == 0 = And (Gte ver) (Lt ver { major = major ver + 1, patch = 0 })
  | otherwise =  And (Gte ver) (Lt ver { minor = minor ver + 1, patch = 0 })

tildeRange :: Parser Range
tildeRange = do
    char '~'
    ver <- try versionCore
        <|> try (withMajor >>= withMinor >>= withPatchWildcard)
        <|> (withMajor >>= withMinorPatchWildcards)
    return . createTildeRange $ ver 

versionCore :: Parser Version
versionCore = withMajor >>= withMinor >>= withPatch

patchWildcard :: Parser Version
patchWildcard = withMajor >>= withMinor >>= withPatchWildcard

minorPatchWildcard :: Parser Version
minorPatchWildcard = withMajor >>= withMinorPatchWildcards

buildId :: Parser String
buildId = alphanumId <|> some digitChar

build :: Parser [String]
build = sepBy1 buildId (char '.')

verPrerelease :: Parser Version
verPrerelease = do
    ver <- versionCore
    char '-'
    pres <- prerelease
    return $ ver { prereleaseTags = pres }

withBuild :: Parser Version -> Parser Version
withBuild pver = do
    ver <- pver
    char '+'
    bs <- build
    return $ ver { buildTags = bs }

verPreAndBuild :: Parser Version
verPreAndBuild = withBuild verPrerelease

verBuild :: Parser Version
verBuild = withBuild versionCore

semVer :: Parser Version
semVer = try verPreAndBuild
    <|> try verPrerelease
    <|> try verBuild 
    <|> versionCore

explicitRange :: Parser Range
explicitRange = do
    v1 <- try versionCore
        <|> try (withMajor >>= withMinor)
        <|> withMajor
    between (some spaceChar) (some spaceChar) (char '-')
    let createRange v = return $ And (Gte v1) (Lte v)
    (try versionCore >>= createRange)
        <|> (do
            v2 <- try (withMajor >>= withMinor)
            createRange v2 { minor = minor v2 + 1 })
        <|> (do
            v2 <- withMajor
            createRange v2 { major = major v2 + 1 })

semVerRange :: Parser Range
semVerRange = try tildeRange
    <|> try caretRange
    <|> try explicitRange
    <|> try orRange
    <|> try andRange
    <|> try wildcard
    <|> try operator
    <|> fmap E semVer

displaySemVer :: Version -> String
displaySemVer (Version True _ _ _ _ _) = "latest"
displaySemVer (Version _ maj min pat pre build) = 
    show maj ++ "." ++
    show min ++ "." ++ 
    show pat ++
    (if null pre then "" else '-' : intercalate "." pre) ++
    (if null build then "" else '+' : intercalate "." build)

bracketedOr :: Range -> String
bracketedOr range = case range of
    Or _ _ -> "(" ++ displayRange range ++ ")"
    _      -> displayRange range

displayRange :: Range -> String
displayRange (Lt v) = "<" ++ displaySemVer v
displayRange (Lte v) = "<=" ++ displaySemVer v
displayRange (Gt v) = ">" ++ displaySemVer v
displayRange (Gte v) = ">=" ++ displaySemVer v
displayRange (E v) = "=" ++ displaySemVer v
displayRange (And r1 r2) = bracketedOr r1 ++ " " ++ bracketedOr r2
displayRange (Or r1 r2) = displayRange r1 ++ " || " ++ displayRange r2
