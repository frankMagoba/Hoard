{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hoard.Config
    ( Config(..)
    , Hoard(..)
    , AbsoluteDir(..)
    , RelativeDir(..)
    , AbsoluteFile(..)
    , RelativeFile(..)
    , isEmptyDir
    , getConfig
    , saveConfig
    ) where

import           Data.Aeson
import           Data.Yaml
import           GHC.Generics
import           Path
import qualified Path.IO      as Path

type AbsoluteDir = Path Abs Dir
type RelativeDir = Path Rel Dir
type AbsoluteFile = Path Abs File
type RelativeFile = Path Rel File

isEmptyDir :: Path a Dir -> IO Bool 
isEmptyDir dir = Path.listDir dir >>= \(dirs, files) -> return $ null dirs && null files

data Hoard =
    Hoard
        { hoardName     :: String
        , hoardLocation :: AbsoluteDir
        }
    deriving (Generic, Show)

data Config =
    Config
        { configHoards       :: [Hoard]
        , configDefaultHoard :: String
        }
    deriving (Generic, Show)

instance FromJSON Hoard where
    parseJSON =
        withObject "Hoard" $ \v -> Hoard <$> v .: "name" <*> v .: "location"

instance ToJSON Hoard where
    toJSON Hoard {..} =
        object ["name" .= hoardName, "location" .= hoardLocation]

instance FromJSON Config where
    parseJSON =
        withObject "Config" $ \v ->
            Config <$> v .: "hoards" <*> v .: "defaultHoard"

instance ToJSON Config where
    toJSON Config {..} =
        object ["hoards" .= configHoards, "defaultHoard" .= configDefaultHoard]

defaultConfig :: Config
defaultConfig = Config {configHoards = [], configDefaultHoard = ""}

configFileName :: FilePath
configFileName = ".hoard.yaml"

configFile :: IO AbsoluteFile
configFile = do
    home <- Path.getHomeDir
    Path.resolveFile home configFileName

-- | Read config from a config file, or return 'defaultConfig'.
getConfig :: IO Config
getConfig = do
    file       <- configFile
    fileExists <- Path.doesFileExist file
    if fileExists
        then do
            result <- decodeFileEither (toFilePath file)
            case result of
                Left  e      -> error (show e)
                Right config -> return config
        else return defaultConfig

-- | Save config to a config file.
saveConfig :: Config -> IO ()
saveConfig conf = do
    file <- configFile
    Data.Yaml.encodeFile (toFilePath file) conf
