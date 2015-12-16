module PSBT.SemVer.Range (
    Range(..)
    , displayRange
    , range
) where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe)
import Text.Megaparsec ((<|>), (<?>), between, choice, eof, many, optional, satisfy, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, string)
import Text.Megaparsec.String (Parser)

import PSBT.SemVer.Util
import PSBT.SemVer.Version

data Range = Lt Version
           | Lte Version
           | Gt Version
           | Gte Version
           | E Version
           | And Range Range
           | Or Range Range
           deriving (Eq, Show)

data Partial = Zero
             | One Integer
             | Two Integer Integer
             | Three Version
             deriving Show

range :: Parser Range
range = try (Or <$> simpleRange <* logicalOr <*> range) <|> simpleRange

logicalOr :: Parser String
logicalOr = between whitespace whitespace (string "||")
  where
    whitespace = many $ char ' '

andRanges :: Parser Range
andRanges = try (And <$> simple <* char ' ' <*> andRanges) <|> simple

simpleRange :: Parser Range
simpleRange = try hyphen <|> try andRanges <|> (eof *> pure (Gte emptyVersion))

partialHyphenUpper :: Partial -> Range
partialHyphenUpper (One n) = Lt emptyVersion { major = n + 1 }
partialHyphenUpper (Two n m) = Lt emptyVersion { major = n, minor = m + 1 }
partialHyphenUpper (Three ver) = Lte ver

hyphen :: Parser Range
hyphen = (And . Gte . partialVersion <$> partial) <* string " - " <*> (partialHyphenUpper <$> partial)

simple :: Parser Range
simple = try primitive <|> try (partialRange <$> partial) <|> try tilde <|> caret

primitive :: Parser Range
primitive = (try (string "<=" *> pure Lte)
             <|> try (string ">=" *> pure Gte)
             <|> try (char '>' *> pure Gt)
             <|> try (char '<' *> pure Lt)
             <|> try (char '=' *> pure E)
             <|> pure E)
             <*> version

incPatch :: Version -> Version
incPatch ver = ver { patch = patch ver + 1 }

incMinor :: Version -> Version
incMinor ver = ver { minor = minor ver + 1, patch = 0 }

incMajor :: Version -> Version
incMajor ver = ver { major = major ver + 1, minor = 0, patch = 0 }

partialVersion :: Partial -> Version
partialVersion Zero = emptyVersion
partialVersion (One n) = emptyVersion { major = n }
partialVersion (Two n m) = emptyVersion { major = n, minor = m}
partialVersion (Three ver) = ver

partialFunc :: (Version -> Version)
            -> (Version -> Version)
            -> (Version -> Version)
            -> (Partial -> Range)
partialFunc _   _   _  Zero = Gte emptyVersion
partialFunc one two three p = And (Gte ver) . Lt $ case p of
    One _ -> one ver
    Two _ _ -> two ver
    Three _ -> three ver
  where
    ver = partialVersion p

partialRange :: Partial -> Range
partialRange = partialFunc incMajor incMinor incPatch

partialTilde :: Partial -> Range
partialTilde = partialFunc incMajor incMinor incMinor

partialCaret :: Partial -> Range
partialCaret = partialFunc incMajor caretTwo caretThree
  where
    caretTwo ver
      | major ver == 0 = incMinor ver
      | otherwise = incMajor ver
    caretThree ver
      | major ver == 0 && minor ver == 0 = incPatch ver
      | major ver == 0 = incMinor ver
      | otherwise = incMajor ver

wildcards :: Parser Char
wildcards = char 'x' <|> char 'X' <|> char '*'

partial :: Parser Partial
partial = try (Three <$> version)
      <|> try (Two <$> nr <* char '.' <*> nr <* optional (char '.' *> wildcards))
      <|> try (One <$> nr <* optional (char '.' *> wildcards *> optional (char '.' *> wildcards)))
      <|> wildcards *> pure Zero

nr :: Parser Integer
nr = read <$> (try (string "0") <|> nonZeroDigits)
  where
    nonZeroDigits = (:) <$> positiveDigit <*> many digitChar

tilde :: Parser Range
tilde = char '~' *> (partialTilde <$> partial)

caret :: Parser Range
caret = char '^' *> (partialCaret <$> partial)

qualifier :: ([String] -> [String] -> t) -> Parser t
qualifier f = f <$> (try (char '-' *> pre) <|> pure [])
    <*> ((char '+' *> build) <|> pure [])

pre :: Parser [String]
pre = parts

build :: Parser [String]
build = parts

parts :: Parser [String]
parts = (:) <$> part <*> many (char '.' *> part)

part :: Parser String
part = try (show <$> nr) <|> some (try (char '-') <|> alphaNumChar)

displayRange :: Range -> String
displayRange (Lt v) = "<" ++ displayVersion v
displayRange (Lte v) = "<=" ++ displayVersion v
displayRange (Gt v) = ">" ++ displayVersion v
displayRange (Gte v) = ">=" ++ displayVersion v
displayRange (E v) = "=" ++ displayVersion v
displayRange (And r1 r2) = displayRange r1 ++ " " ++ displayRange r2
displayRange (Or r1 r2) = displayRange r1 ++ " || " ++ displayRange r2
