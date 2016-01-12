{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Exception          (Exception, SomeException,
                                             toException)
import           Control.Monad              (unless)
import           Control.Monad.Catch        (Handler (..), MonadThrow, catches,
                                             throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Aeson
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as B
import           Data.Either                (isLeft)
import           Data.Foldable              (maximum, traverse_)
import           Data.HashMap.Lazy          (HashMap, keys)
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromJust, mapMaybe)
import qualified Data.Text                  as T (unpack)
import           Network.HTTP
import qualified Network.Stream             as S (Result)
import           Network.URI                (parseURI)
import           Options.Applicative
import           System.Directory
import           System.Exit                (ExitCode)
import           System.IO                  (IOMode (ReadWriteMode), hClose,
                                             hGetContents, openFile)
import           System.Process             (readProcess, runInteractiveCommand,
                                             waitForProcess)
import           Text.Megaparsec            (parseMaybe)

import           PSBT.Bower
import qualified PSBT.SemVer                as S

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

runInit :: MonadIO m => String -> m ()
runInit str = liftIO $ do
    createDirectory str
    setCurrentDirectory str
    createDirectory "src"
    createDirectory "test"
    h <- openFile "bower.json" ReadWriteMode
    hClose h
    putStrLn $ "Project created in " ++ str

data PackageListing = PackageListing {
    pkgName :: String
    , url   :: String
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
                  deriving Show

instance Exception BuildError where

getListing :: (MonadIO m, MonadThrow m) => String -> m PackageListing
getListing pkg = do
    res <- liftIO $ fmap (fmap parseResponse) response
    case res of
        Left _              -> throwM ConnectionError
        Right Nothing       -> throwM JSONParseError
        Right (Just [])     -> throwM $ ListingNotFound pkg
        Right (Just [ps])   -> return ps
        Right (Just (p:ps)) -> if pkgName p == pkg
                                    then return p
                                    else throwM $ MultipleListingsFound (p:ps)
  where
    request = defaultGETRequest_ . fromJust . parseURI $ registry ++ pkg
    response = simpleHTTP request :: IO (S.Result (Response ByteString))
    parseResponse = decode . rspBody :: Response ByteString -> Maybe [PackageListing]

runSilently :: FilePath -> [String] -> IO ()
runSilently fp args = do
    (stdin, stdout, stderr, ph) <- runInteractiveCommand (unwords $ fp : args)
    hClose stdin
    hClose stdout
    waitForProcess ph
    hGetContents stderr >>= putStrLn
    hClose stderr

checkoutCorrectVersion :: MonadIO m => Dependency -> m ()
checkoutCorrectVersion dep = liftIO $ do
    setCurrentDirectory $ "dependencies/" ++ T.unpack (packageName dep)
    tags <- readProcess "git" ["tag"] ""
    let versions = mapMaybe (parseMaybe S.version) . lines $ tags
    let max = case version dep of
            Nothing  -> maximum versions
            Just range -> foldr (maxInRange range) S.emptyVersion versions
    let versionString = 'v' : S.displayVersion max
    putStrLn $ "Checking out " ++ versionString ++ "..."
    runSilently "git" ["checkout", versionString]
    setCurrentDirectory "../../"
  where
    maxInRange range ver currentMax
      | ver > currentMax && S.inRange ver range = ver
      | otherwise = currentMax

download :: (MonadIO m, MonadThrow m) => Dependency -> m [Dependency]
download dep = do
    let pkg = T.unpack . packageName $ dep
    b <- liftIO $ do
      createDirectoryIfMissing False "dependencies"
      doesDirectoryExist $ "dependencies/" ++ pkg
    unless b $ do
        (PackageListing pkgName url) <- getListing pkg
        let gitArgs = ["clone", url, "dependencies/" ++ pkgName]
        liftIO $ do
            putStrLn $ "Downloading " ++ pkgName ++ "..."
            runSilently "git" gitArgs
    checkoutCorrectVersion dep
    downloadBowerDeps ("dependencies/" ++ pkg ++ "/bower.json")

downloadBowerDeps :: (MonadIO m, MonadThrow m) => FilePath -> m [Dependency]
downloadBowerDeps fp = do
    bower <- readBowerFile fp
    case dependencies bower of
        Just deps -> do
            traverse_ download deps
            return deps
        Nothing -> return []

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

runBuild :: (MonadIO m, MonadThrow m) => Maybe String -> m ()
runBuild out = do
    deps <- downloadBowerDeps "bower.json"
    liftIO $ do
        putStrLn ""
        readProcess "psc" pscArgs "" >>= putStrLn
        case out of
            Just s -> readProcess "psc-bundle" (pscBundleArgs s) "" >>= putStrLn
            Nothing -> return ()
        putStrLn "Build complete."

runCommand :: (MonadIO m, MonadThrow m) => Command -> m ()
runCommand (Init dir) = runInit dir
runCommand (Build out) = runBuild out

buildErrorMessage :: BuildError -> String
buildErrorMessage (ListingNotFound str) = "Package " ++ str ++ " not found."
buildErrorMessage (MultipleListingsFound pkgs) = "Multiple packages found: \n" ++ unlines (map pkgName pkgs)
buildErrorMessage ConnectionError = "There was a problem with the connection"
buildErrorMessage JSONParseError = "Malformed JSON received"

buildErrorHandler :: MonadIO m => Handler m ()
buildErrorHandler = Handler $ liftIO . putStrLn . buildErrorMessage

main = do
    command <- liftIO $ customExecParser parserPrefs argsInfo
    catches (runCommand command) [bowerErrorHandler, buildErrorHandler]
