-- | Module      : PSBT.SemVer.Range
--   Description : SemVer range parsing

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

-- | Datatype for a SemVer range
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

partialVersion :: Partial -> Version
partialVersion Zero = emptyVersion
partialVersion (One n) = emptyVersion { major = n }
partialVersion (Two n m) = emptyVersion { major = n, minor = m}
partialVersion (Three ver) = ver

nr :: Parser Integer
nr = read <$> (try (string "0") <|> nonZeroDigits)
  where
    nonZeroDigits = (:) <$> positiveDigit <*> many digitChar

wildcards :: Parser Char
wildcards = char 'x' <|> char 'X' <|> char '*'

partial :: Parser Partial
partial = try (Three <$> version)
      <|> try (Two <$> nr <* char '.' <*> nr <* optional (char '.' *> wildcards))
      <|> try (One <$> nr <* optional (char '.' *> wildcards *> optional (char '.' *> wildcards)))
      <|> wildcards *> pure Zero

partialHyphenUpper :: Partial -> Range
partialHyphenUpper Zero = error "The upper bound of a hyphen range cannot be a single wildcard"
partialHyphenUpper (One n) = Lt emptyVersion { major = n + 1 }
partialHyphenUpper (Two n m) = Lt emptyVersion { major = n, minor = m + 1 }
partialHyphenUpper (Three ver) = Lte ver

hyphen :: Parser Range
hyphen = (And . Gte . partialVersion <$> partial) <* string " - " <*> (partialHyphenUpper <$> partial)

primitive :: Parser Range
primitive = (try (string "<=" *> pure Lte)
             <|> try (string ">=" *> pure Gte)
             <|> try (char '>' *> pure Gt)
             <|> try (char '<' *> pure Lt)
             <|> try (char '=' *> pure E)
             <|> pure E)
             <*> version

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

incMajor :: Version -> Version
incMajor ver = ver { major = major ver + 1, minor = 0, patch = 0 }

incMinor :: Version -> Version
incMinor ver = ver { minor = minor ver + 1, patch = 0 }

incPatch :: Version -> Version
incPatch ver = ver { patch = patch ver + 1 }

partialRange :: Partial -> Range
partialRange = partialFunc incMajor incMinor incPatch

partialTilde :: Partial -> Range
partialTilde = partialFunc incMajor incMinor incMinor

tilde :: Parser Range
tilde = char '~' *> (partialTilde <$> partial)

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

caret :: Parser Range
caret = char '^' *> (partialCaret <$> partial)

simple :: Parser Range
simple = try primitive
     <|> try (partialRange <$> partial)
     <|> try tilde
     <|> caret

andRanges :: Parser Range
andRanges = try (And <$> simple <* char ' ' <*> andRanges) <|> simple

simpleRange :: Parser Range
simpleRange = try hyphen
          <|> try andRanges
          <|> (eof *> pure (Gte emptyVersion))

logicalOr :: Parser String
logicalOr = whitespace *> string "||" <* whitespace
  where
    whitespace = many $ char ' '

-- | Parser for SemVer ranges
--
-- Accepts all formats specified in the node-semver docs
-- <https://github.com/npm/node-semver#ranges>
range :: Parser Range
range = try (Or <$> simpleRange <* logicalOr <*> range)
    <|> simpleRange

-- | Gets a textual representation of a Range
--
-- Parsing the result of displayRange does not necessarily give you the same
-- tree back, but it will definitely give you an equivalent tree.
--
-- Similarly, parsing a string then calling displayRange on the result may not
-- yield the original string, but it will be equivalent.
displayRange :: Range -> String
displayRange (Lt v) = "<" ++ displayVersion v
displayRange (Lte v) = "<=" ++ displayVersion v
displayRange (Gt v) = ">" ++ displayVersion v
displayRange (Gte v) = ">=" ++ displayVersion v
displayRange (E v) = "=" ++ displayVersion v
displayRange (And r1 r2) = displayRange r1 ++ " " ++ displayRange r2
displayRange (Or r1 r2) = displayRange r1 ++ " || " ++ displayRange r2
