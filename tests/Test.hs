import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements, oneof, sized)
import Test.QuickCheck.Modifiers (NonNegative(..), getNonNegative)
import Text.Megaparsec (ParseError, parse)

import PSBT.SemVer

instance Arbitrary Range where
    arbitrary = oneof [
        elements [Lt, Lte, Gt, Gte, E] <*> arbitrary
        , elements [And, Or] <*> arbitrary <*> arbitrary
        ]

nonNegative :: Gen Integer
nonNegative = getNonNegative <$> arbitrary

parseVersion :: String -> Either ParseError Version
parseVersion = parse version "QuickCheck"

parseRange :: String -> Either ParseError Range
parseRange = parse range "QuickCheck"

-------------------------------------------------------------------------------

instance Arbitrary Version where
    arbitrary = Version <$>
        (getNonNegative <$> arbitrary) <*>
        (getNonNegative <$> arbitrary) <*>
        (getNonNegative <$> arbitrary) <*>
        pure [] <*>
        pure []

displayVersionIso1 :: Version -> Bool
displayVersionIso1 ver =
    parseVersion (displayVersion ver) == Right ver

newtype VersionString = VersionString { getVersionString :: String }
    deriving (Eq, Show)

instance Arbitrary VersionString where
    arbitrary = VersionString <$> do
        maj <- nonNegative
        min <- nonNegative
        pat <- nonNegative
        return $ show maj ++ "." ++ show min ++ "." ++ show pat

displayVersionIso2 :: VersionString -> Bool
displayVersionIso2 (VersionString str) = 
    fmap displayVersion (parseVersion str) == Right str

justVersionIsEquals :: VersionString -> Bool
justVersionIsEquals (VersionString str) =
    parseRange str == parseRange ('=' : str)

-------------------------------------------------------------------------------

newtype RangeString = RangeString String deriving (Eq, Show)

rangeStringOfSize :: Int -> Gen RangeString
rangeStringOfSize 0 = rangeStringOfSize 1 -- not sure how to handle this case
rangeStringOfSize 1 = fmap RangeString $
    (++) <$> elements ["<","<=",">",">=","="]
    <*> (getVersionString <$> arbitrary)
rangeStringOfSize n = oneof $ map createSubtrees [1..n-1]
  where
    createSubtrees x = do
        (RangeString left) <- rangeStringOfSize x 
        sep <- elements [" || ", " "]
        (RangeString right) <- rangeStringOfSize (n-x)
        return . RangeString $ left ++ sep ++ right

instance Arbitrary RangeString where
    arbitrary = sized rangeStringOfSize

displayRangeIso :: RangeString -> Bool
displayRangeIso (RangeString str) =
    fmap displayRange (parseRange str) == Right str

-------------------------------------------------------------------------------

test1 :: Bool
test1 = parseRange "1.2.3 - 2.3.4" == Right (And v1 v2)
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 3 }
    v2 = Lte emptyVersion { major = 2, minor = 3, patch = 4 }

test2 :: Bool
test2 = parseRange "1.2 - 2.3.4"== res
     && parseRange "1.2.* - 2.3.4" == res
     && parseRange "1.2.x - 2.3.4" == res
     && parseRange "1.2.X - 2.3.4" == res
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 0 }
    v2 = Lte emptyVersion { major = 2, minor = 3, patch = 4 }
    res = Right (And v1 v2)

test3 :: Bool
test3 = parseRange "1 - 2.3.4" == res
     && parseRange "1.* - 2.3.4" == res
     && parseRange "1.*.* - 2.3.4" == res
     && parseRange "1.*.x - 2.3.4" == res
     && parseRange "1.*.X - 2.3.4" == res
     && parseRange "1.x.* - 2.3.4" == res
     && parseRange "1.x.x - 2.3.4" == res
     && parseRange "1.x.X - 2.3.4" == res
     && parseRange "1.X.* - 2.3.4" == res
     && parseRange "1.X.x - 2.3.4" == res
     && parseRange "1.X.X - 2.3.4" == res
  where
    v1 = Gte emptyVersion { major = 1, minor = 0, patch = 0 }
    v2 = Lte emptyVersion { major = 2, minor = 3, patch = 4 }
    res = Right (And v1 v2)

test4 :: Bool
test4 = parseRange "1.2.3 - 2.3" == Right (And v1 v2)
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 3 }
    v2 = Lt emptyVersion { major = 2, minor = 4, patch = 0 }

test5 :: Bool
test5 = parseRange "1.2.3 - 2" == Right (And v1 v2)
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 3 }
    v2 = Lt emptyVersion { major = 3, minor = 0, patch = 0 }

test6 :: Bool
test6 = parseRange "" == res
     && parseRange "*" == res
     && parseRange "x" == res
     && parseRange "X" == res
  where
    res = Right (Gte emptyVersion)

test7 :: Bool
test7 = parseRange "1" == res
     && parseRange "1.*" == res
     && parseRange "1.*.*" == res
     && parseRange "1.*.x" == res
     && parseRange "1.*.X" == res
     && parseRange "1.x.*" == res
     && parseRange "1.x.x" == res
     && parseRange "1.x.X" == res
     && parseRange "1.X.*" == res
     && parseRange "1.X.x" == res
     && parseRange "1.X.X" == res
  where
    v1 = Gte emptyVersion { major = 1, minor = 0, patch = 0 }
    v2 = Lt emptyVersion { major = 2, minor = 0, patch = 0 }
    res = Right (And v1 v2)

test8 :: Bool
test8 = parseRange "1.2" == res
     && parseRange "1.2.*" == res
     && parseRange "1.2.x" == res
     && parseRange "1.2.X" == res
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 0 }
    v2 = Lt emptyVersion { major = 1, minor = 3, patch = 0 }
    res = Right (And v1 v2)

test9 :: Bool
test9 = parseRange "~1.2.3" == Right (And v1 v2)
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 3 }
    v2 = Lt emptyVersion { major = 1, minor = 3, patch = 0 }

test10 :: Bool
test10 = parseRange "~1.2" == res
      && parseRange "~1.2.*" == res
      && parseRange "~1.2.x" == res
      && parseRange "~1.2.X" == res
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 0 }
    v2 = Lt emptyVersion { major = 1, minor = 3, patch = 0 }
    res = Right (And v1 v2)

test11 :: Bool
test11 = parseRange "~1" == res
     && parseRange "~1.*" == res
     && parseRange "~1.*.*" == res
     && parseRange "~1.*.x" == res
     && parseRange "~1.*.X" == res
     && parseRange "~1.x.*" == res
     && parseRange "~1.x.x" == res
     && parseRange "~1.x.X" == res
     && parseRange "~1.X.*" == res
     && parseRange "~1.X.x" == res
     && parseRange "~1.X.X" == res
  where
    v1 = Gte emptyVersion { major = 1, minor = 0, patch = 0 }
    v2 = Lt emptyVersion { major = 2, minor = 0, patch = 0 }
    res = Right (And v1 v2)

test12 :: Bool
test12 = parseRange "^1.2.3" == Right (And v1 v2)
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 3 }
    v2 = Lt emptyVersion { major = 2, minor = 0, patch = 0 }

test13 :: Bool
test13 = parseRange "^0.2.3" == Right (And v1 v2)
  where
    v1 = Gte emptyVersion { major = 0, minor = 2, patch = 3 }
    v2 = Lt emptyVersion { major = 0, minor = 3, patch = 0 }

test14 :: Bool
test14 = parseRange "^0.0.3" == Right (And v1 v2)
  where
    v1 = Gte emptyVersion { major = 0, minor = 0, patch = 3 }
    v2 = Lt emptyVersion { major = 0, minor = 0, patch = 4 }

test15 :: Bool
test15 = parseRange "^1.2" == res
      && parseRange "^1.2.*" == res
      && parseRange "^1.2.x" == res
      && parseRange "^1.2.X" == res
  where
    v1 = Gte emptyVersion { major = 1, minor = 2, patch = 0 }
    v2 = Lt emptyVersion { major = 2, minor = 0, patch = 0 }
    res = Right (And v1 v2)

test16 :: Bool
test16 = parseRange "^0.2" == res
      && parseRange "^0.2.*" == res
      && parseRange "^0.2.x" == res
      && parseRange "^0.2.X" == res
  where
    v1 = Gte emptyVersion { major = 0, minor = 2, patch = 0 }
    v2 = Lt emptyVersion { major = 0, minor = 3, patch = 0 }
    res = Right (And v1 v2)

test17 :: Bool
test17 = parseRange "^1" == res
     && parseRange "^1.*" == res
     && parseRange "^1.*.*" == res
     && parseRange "^1.*.x" == res
     && parseRange "^1.*.X" == res
     && parseRange "^1.x.*" == res
     && parseRange "^1.x.x" == res
     && parseRange "^1.x.X" == res
     && parseRange "^1.X.*" == res
     && parseRange "^1.X.x" == res
     && parseRange "^1.X.X" == res
  where
    v1 = Gte emptyVersion { major = 1, minor = 0, patch = 0 }
    v2 = Lt emptyVersion { major = 2, minor = 0, patch = 0 }
    res = Right (And v1 v2)

test18 :: Bool
test18 = ver1 < ver2
      && ver2 < ver3
      && ver3 < ver4
      && ver4 < ver5
      && ver5 < ver6
      && ver6 < ver7
      && ver7 < ver8
  where
    ver1 = Version 1 0 0 [AlphaNum "alpha"] []
    ver2 = Version 1 0 0 [AlphaNum "alpha",Num 1] []
    ver3 = Version 1 0 0 [AlphaNum "alpha",AlphaNum "beta"] []
    ver4 = Version 1 0 0 [AlphaNum "beta"] []
    ver5 = Version 1 0 0 [AlphaNum "beta",Num 2] []
    ver6 = Version 1 0 0 [AlphaNum "beta",Num 11] []
    ver7 = Version 1 0 0 [AlphaNum "rc",Num 1] []
    ver8 = Version 1 0 0 [] []

tests :: [Test]
tests = [
    testProperty "displayVersion isomorphism - Version to String to Version" displayVersionIso1
    , testProperty "Normal hyphen range" displayVersionIso2
    , testProperty "displayRangeisomorphism - String to Range to String" displayRangeIso
    , testProperty "Normal hyphen range" test1
    , testProperty "Major and minor on left" test2
    , testProperty "Major on left" test3
    , testProperty "Major and minor on right" test4
    , testProperty "Major on right" test5
    , testProperty "Empty version" test6
    , testProperty "Only major" test7
    , testProperty "Manjor and minor" test8
    , testProperty "Tilde range" test9
    , testProperty "Tilde major and minor" test10
    , testProperty "Tilde major" test11
    , testProperty "Caret range" test12
    , testProperty "Caret range - zero major" test13
    , testProperty "Caret range - zero major and minor" test14
    , testProperty "Caret range - only major and minor, no zeroes" test15
    , testProperty "Caret range - only major and minor, zero major" test16
    , testProperty "Caret range - only major" test17
    , testProperty "Version ordering test" test18
    ]

main = defaultMain tests
