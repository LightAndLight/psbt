{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), catchE, runExceptT, throwE)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (traverse_)
import Data.HashMap.Lazy (HashMap, keys)
import Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import Network.HTTP
import qualified Network.Stream as S (Result)
import Network.URI (parseURI)
import Options.Applicative
import System.Directory
import System.IO (openFile, hClose, IOMode(ReadWriteMode))
import System.Process (readProcess)

import PSBT

data Command = Init String
             | Build (Maybe String)

initParser :: Parser Command
initParser = Init <$> strArgument
    (metavar "DIRECTORY")

initInfo :: ParserInfo Command
initInfo = info initParser
    (progDesc "Initialize a PureScript project directory")

buildParser :: Parser Command
buildParser = Build <$> optional (strOption
    (long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "Output a JavaScript file suitable for a browser"))

buildInfo :: ParserInfo Command
buildInfo = info buildParser
    (progDesc "Download missing dependencies and build commonJS modules")

argsParser :: Parser Command
argsParser = subparser
    (command "init" initInfo
  <> command "build" buildInfo)

argsInfo :: ParserInfo Command
argsInfo = info argsParser
    (progDesc "A package manager for the PureScript programming language")

parserPrefs :: ParserPrefs
parserPrefs = prefs showHelpOnError

runInit :: String -> IO ()
runInit str = do
    createDirectory str
    setCurrentDirectory str
    createDirectory "src"
    createDirectory "test"
    h <- openFile "bower.json" ReadWriteMode
    hClose h
    putStrLn $ "Project created in " ++ str

data PackageListing = PackageListing {
    pkgName :: String
    , url :: String
    } deriving Show

instance FromJSON PackageListing where
    parseJSON (Object v) = PackageListing <$>
                           v .: "name" <*>
                           v .: "url"
    parseJSON _          = empty

registry :: String
registry = "http://bower.herokuapp.com/packages/search/" 

data BuildError = ListingNotFound String
                  | MultipleListingsFound [PackageListing]
                  | ConnectionError
                  | JSONParseError
                  | BowerBuildError BowerError
                  deriving Show

getListing :: String -> ExceptT BuildError IO PackageListing
getListing pkg = do
    res <- liftIO $ fmap (fmap parseResponse) response
    case res of
        Left _              -> throwE ConnectionError
        Right Nothing       -> throwE JSONParseError
        Right (Just [])     -> throwE $ ListingNotFound pkg
        Right (Just [ps])   -> return ps
        Right (Just (p:ps)) -> if pkgName p == pkg
                                    then return p
                                    else throwE $ MultipleListingsFound (p:ps)
  where 
    request = defaultGETRequest_ . fromJust . parseURI $ registry ++ pkg
    response = simpleHTTP request :: IO (S.Result (Response ByteString))
    parseResponse = decode . rspBody :: Response ByteString -> Maybe [PackageListing]

download :: String -> ExceptT BuildError IO ()
download pkg = do
    liftIO $ createDirectoryIfMissing False "dependencies"
    b <- liftIO $ doesDirectoryExist $ "dependencies/" ++ pkg
    unless b $ do
        (PackageListing pkgName url) <- getListing pkg
        let gitArgs = ["clone", url, "dependencies/" ++ pkgName]
        liftIO $ readProcess "git" gitArgs "" >>= putStrLn
    downloadBowerDeps ("dependencies/" ++ pkg ++ "/bower.json")

downloadBowerDeps :: FilePath -> ExceptT BuildError IO ()
downloadBowerDeps fp = do
    bower <- catchE (readBowerFile fp) (throwE . BowerBuildError)
    case dependencies bower of
        Just deps -> traverse_ (download . T.unpack . packageName) deps
        Nothing -> return ()

pscArgs :: [String]
pscArgs = ["src/Main.purs"
    , "dependencies/purescript-*/src/**/*.purs"
    , "--ffi"
    , "dependencies/purescript-*/src/**/*.js"
    ]

pscBundleArgs :: String -> [String]
pscBundleArgs str = ["output/*/index.js"
    , "output/*/foreign.js"
    , "--module", "Main"
    , "--main", "Main"
    , "-o", str
    ]

runBuild :: Maybe String -> ExceptT BuildError IO ()
runBuild out = do
    downloadBowerDeps "bower.json"
    liftIO $ do
        readProcess "psc" pscArgs "" >>= putStrLn
        case out of
            Just s -> readProcess "psc-bundle" (pscBundleArgs s) "" >>= putStrLn
            Nothing -> return ()
        putStrLn "Build complete."

data AppError = BuildAppError BuildError

runCommand :: Command -> ExceptT AppError IO ()
runCommand (Init dir) = liftIO $ runInit dir
runCommand (Build out) = catchE (runBuild out) (throwE . BuildAppError) 

buildErrorMessage :: BuildError -> String
buildErrorMessage (ListingNotFound str) = "Package " ++ str ++ " not found."
buildErrorMessage (MultipleListingsFound pkgs) =
    "Multiple packages found: \n" ++ unlines (map pkgName pkgs)
buildErrorMessage ConnectionError = "There was a problem with the connection"
buildErrorMessage JSONParseError = "Malformed JSON received"
buildErrorMessage (BowerBuildError e) = case e of
    JSONError txt -> T.unpack txt
    FileNotFound str -> "File " ++ str ++ " not found"

handleErrors :: Either AppError () -> IO ()
handleErrors (Right v) = return v
handleErrors (Left e) = putStrLn $ "Error: " ++ case e of
    BuildAppError e' -> buildErrorMessage e'

main :: IO ()
main = do
    command <- customExecParser parserPrefs argsInfo
    errors <- runExceptT $ runCommand command
    handleErrors errors
