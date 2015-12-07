{-# LANGUAGE OverloadedStrings #-}

module PSBT.Bower (
    Bower(..)
    , BowerError(..)
    , Dependency(..)
    , readBowerFile
) where

import Control.Applicative ((<|>), empty, optional)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.Aeson (FromJSON, Value(..), parseJSON)
import Data.Aeson.BetterErrors (Parse, ParseError, asBool, asObject, asString, displayError, eachInArray,
                                eachInObject, key, keyMay, keyOrDefault, parse, throwCustomError)
import Data.Aeson.BetterErrors.Internal (ParseT(..), liftParse)
import Data.Aeson.Types (Object, Parser)
import qualified Data.ByteString.Lazy as B (readFile)
import Data.HashMap.Lazy (HashMap, toList)
import Data.Text (Text)
import qualified Data.Text as T (unlines)
import System.Directory (doesFileExist)
import Text.Megaparsec (parseMaybe)

import PSBT.SemVer

data Dependency = Dependency {
    packageName :: Text
    , version :: Version
    } deriving Show

data Bower = Bower {
    name              :: String
    , description     :: Maybe String
    , mainModule      :: Maybe [String]
    , dependencies    :: Maybe [Dependency]
    , devDependencies :: Maybe [Dependency]
    , resolutions     :: Maybe [Dependency]
    } deriving Show

data BowerError = JSONError Text
                | FileNotFound FilePath
                deriving Show

asDependencies :: Parse Text [Dependency]
asDependencies = do
    depList <- eachInObject asString
    traverse getDependency depList
  where
    getDependency (name,versionStr)
      | versionStr == "latest" = return
          (Dependency name $ Version True 0 0 0 [] [])
      | otherwise = case parseMaybe semVer versionStr of
            Just version -> return $ Dependency name version
            Nothing      -> throwCustomError "Invalid version format"

asBower :: Parse Text Bower
asBower = Bower <$>
    key "name" asString <*>
    keyMay "description" asString <*>
    keyMay "main" (eachInArray asString) <*>
    keyMay "dependencies" asDependencies <*>
    keyMay "devDependencies" asDependencies <*>
    keyMay "resolutions" asDependencies

readBowerFile :: FilePath -> ExceptT BowerError IO Bower
readBowerFile fp = do
        exists <- liftIO $ doesFileExist fp
        unless exists (throwE $ FileNotFound fp)
        bower <- liftIO $ B.readFile fp
        case parse asBower bower of
            Left e  -> throwE (JSONError . T.unlines $ displayError id e)
            Right b -> return b

