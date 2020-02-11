{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hoard.Add
    ( hoardAdd
    , hoardAnnex
    ) where

import qualified Codec.Audio.FLAC.Metadata         as Flac
import qualified Codec.Audio.FLAC.Metadata.Picture as Flac
import           Codec.Picture
    ( DynamicImage (..)
    , convertRGB8
    , decodeImage
    , decodePng
    , readJpeg
    )
import           Control.Applicative               ( (<|>) )
import           Control.Monad                     ( forM, forM_, when )
import           Control.Monad.IO.Class            ( MonadIO (..) )
import           Data.Aeson
import qualified Data.ByteString.UTF8              as BS ( toString )
import           Data.Default.Class
import           Data.Either.Combinators           ( rightToMaybe )
import           Data.FileEmbed                    ( embedFile )
import           Data.List                         ( sort, sortBy )
import           Data.Maybe
    ( catMaybes
    , fromMaybe
    , isJust
    , mapMaybe
    , maybe
    )
import           Data.Text                         ( Text )
import qualified Data.Text                         as Text
import           Data.Yaml
import           Data.Yaml.Pretty.Extras
import           GHC.Generics
import           Hoard.Config
import           Lib
import           Path                              ( (</>) )
import qualified Path
import qualified Path.IO                           as Path
import           System.Directory                  ( canonicalizePath )
import           System.IO                         ( hPutStrLn, stderr )
import qualified Text.Editor                       as Editor
import           Text.Read                         ( readMaybe )
import           Text.Regex                        ( matchRegex, mkRegex )

data MusicAlbumSource =
    MusicAlbumSource
        { sourceLocation :: AbsoluteDir
        , targetLocation :: AbsoluteDir
        , albumName      :: Maybe Text
        , artistName     :: Maybe Text
        , albumYear      :: Maybe Int
        , coverImage     :: Maybe AbsoluteFile
        , genres         :: [Text]
        , tracks         :: [MusicTrackSource]
        }
    deriving (Generic, Show, FromJSON, ToJSON)

data MusicTrackSource =
    MusicTrackSource
        { sourceFilename :: RelativeFile
        , targetFilename :: RelativeFile
        , trackNumber    :: Int
        , discNumber     :: Maybe Int
        , title          :: Maybe Text
        }
    deriving (Generic, Show)

instance ToJSON MusicTrackSource where
    toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromJSON MusicTrackSource where
    parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance ToPrettyYaml [MusicAlbumSource] where
    fieldOrder =
        const
            [ "sourceLocation"
            , "targetLocation"
            , "albumName"
            , "artistName"
            , "albumYear"
            , "coverImage"
            , "genres"
            , "tracks"
            , "sourceFilename"
            , "targetFilename"
            , "discNumber"
            , "trackNumber"
            , "name"
            ]

instance Default Flac.MetaSettings where
    def =
        Flac.MetaSettings
            { Flac.metaAutoVacuum = True
            , Flac.metaSortPadding = True
            , Flac.metaUsePadding = True
            , Flac.metaPreserveFileStats = True
            }

readTextMaybe :: Read a => Text -> Maybe a
readTextMaybe = readMaybe . Text.unpack

readMaybeTextMaybe :: Read a => Maybe Text -> Maybe a
readMaybeTextMaybe = (>>= readTextMaybe)

tshow :: Show a => a -> Text
tshow = Text.pack . show

getSource :: [String] -> Hoard -> FilePath -> IO (Maybe MusicAlbumSource)
getSource args Hoard {..} path = do
    absPath <- canonicalizePath path >>= Path.parseAbsDir
    files   <- sort <$> Path.walkDirAccum Nothing (\_ _ -> pure) absPath
    let flacFiles = filter (\f -> Path.fileExtension f == ".flac") files
    let jpgFiles  = filter (\f -> Path.fileExtension f == ".jpg") files
    -- mapM_ print flacFiles
    if null flacFiles
        then hPutStrLn stderr ("No FLAC files in " <> path) >> return Nothing
        else continueWithFiles absPath flacFiles jpgFiles
  where
    continueWithFiles absPath flacFiles jpgFiles = do
        let firstFlacFile = Path.toFilePath $ head flacFiles
            -- putStrLn $ "Extracting info from " <> show (Path.filename $ head flacFiles)
        src :: MusicAlbumSource <- Flac.runFlacMeta def firstFlacFile $ do
            maybeName :: Maybe Text     <- Flac.retrieve (Flac.VorbisComment Flac.Album)
            maybeArtist :: Maybe Text   <- Flac.retrieve (Flac.VorbisComment Flac.Artist)
            maybeGenre :: Maybe Text    <- Flac.retrieve (Flac.VorbisComment Flac.Genre)
            maybeMetaYear :: Maybe Text <- Flac.retrieve (Flac.VorbisComment Flac.Date)
            let maybeNameYear :: Maybe Text =
                    Text.pack . head <$> matchRegex (mkRegex "([0-9]{4})") (Path.toFilePath $ Path.dirname absPath)
            liftIO $ print $ Path.toFilePath $ Path.dirname absPath
            liftIO $ putStrLn $ "meta year: " <> show maybeMetaYear
            liftIO $ putStrLn $ "name year: " <> show maybeNameYear
            musicFolder  <- Path.parseRelDir "Music"
            artistFolder <- Path.parseRelDir $ maybe "[ARTIST]" Text.unpack maybeArtist
            albumFolder  <- Path.parseRelDir $ maybe "[ALBUM]" Text.unpack maybeName
            return MusicAlbumSource
                { sourceLocation = absPath
                , targetLocation = hoardLocation </> musicFolder </> artistFolder </> albumFolder
                , albumName      = maybeName
                , artistName     = maybeArtist
                , albumYear      = readMaybeTextMaybe maybeMetaYear <|> readMaybeTextMaybe maybeNameYear
                , coverImage     = if null jpgFiles then Nothing else Just $ head jpgFiles
                , genres         = maybe [] (filter (not . Text.null) . Text.splitOn ",") maybeGenre
                , tracks         = []
                }
        tracks <-
            mapM
                    ( \(i, file) -> do
                        (maybeDiscNumber, maybeTrackNumber, maybeTitle) <-
                            Flac.runFlacMeta def (Path.toFilePath file) $ do
                                discNumber :: Maybe Text  <- Flac.retrieve $ Flac.VorbisComment Flac.DiscNumber
                                trackNumber :: Maybe Text <- Flac.retrieve $ Flac.VorbisComment Flac.TrackNumber
                                title :: Maybe Text       <- Flac.retrieve $ Flac.VorbisComment Flac.Title
                                return (discNumber, trackNumber, title)
                        relFile <- Path.stripProperPrefix absPath file
                        let trackNumber = fromMaybe i $ maybeTrackNumber >>= readTextMaybe
                        let targetFilename = fromMaybe (Path.fromRelFile relFile) $ maybeTitle >>= \title ->
                                Just $ Text.unpack $ tshow trackNumber <> " - " <> title <> ".flac"
                        targetFile <- Path.parseRelFile targetFilename
                        return MusicTrackSource
                            { sourceFilename = relFile
                            , targetFilename = targetFile
                            , trackNumber    = trackNumber
                            , discNumber     = maybeDiscNumber >>= readTextMaybe
                            , title          = maybeTitle
                            }
                    )
                $ zip [1 ..] flacFiles
        let needDiscFolders = any (> 1) $ mapMaybe discNumber tracks
        discTracks <- forM tracks $ \track@MusicTrackSource {..} -> do
            discFolder <- Path.parseRelDir $ "Disc " <> maybe "UNK" show discNumber
            return $ track { targetFilename = discFolder </> targetFilename }
        return $ Just $ src { tracks = sortBy compareTracks (if needDiscFolders then discTracks else tracks) }
    compareTracks t1 t2 = do
        let disc1   = fromMaybe 0 $ discNumber t1
        let disc2   = fromMaybe 0 $ discNumber t2
        let number1 = trackNumber t1
        let number2 = trackNumber t2
        case compare disc1 disc2 of
            EQ -> compare number1 number2
            x  -> x

updateMetadata :: MusicAlbumSource -> IO ()
updateMetadata MusicAlbumSource {..} = forM_ tracks $ \MusicTrackSource {..} -> do
    let file = targetLocation </> targetFilename
    let fish = convertRGB8 <$> rightToMaybe (decodePng $(embedFile "src/Hoard/koi.png"))
    putStrLn $ "    Update " <> show file
    maybeCover <- rightToMaybe <$> case Path.toFilePath <$> coverImage of
        Just fn -> readJpeg fn
        Nothing -> pure $ Left "No cover image specified!"
    Flac.runFlacMeta def (Path.fromAbsFile file) $ do
        Flac.VorbisComment Flac.Artist Flac.=-> artistName
        Flac.VorbisComment Flac.Album Flac.=-> albumName
        Flac.VorbisComment Flac.Title Flac.=-> title
        Flac.VorbisComment Flac.Date Flac.=-> tshow <$> albumYear
        Flac.VorbisComment Flac.Genre Flac.=-> maybeGenres
        Flac.VorbisComment Flac.TrackNumber Flac.=-> Just (tshow trackNumber)
        Flac.VorbisComment Flac.DiscNumber Flac.=-> tshow <$> discNumber
        Flac.VorbisComment Flac.DiscTotal Flac.=-> tshow <$> maxDiscNum
        forM_ fish $ Flac.writePngPicture Flac.PictureFish
        forM_ maybeCover $ \(ImageYCbCr8 im) -> Flac.writeJpegPicture Flac.PictureFrontCover 90 im
  where
    discNums    = catMaybes $ discNumber <$> tracks
    maxDiscNum  = if null discNums then Nothing else Just $ maximum discNums
    maybeGenres = let gs = Text.intercalate "," genres in if Text.null gs then Nothing else Just gs

data AddAction
    = Copy
    | Move
    deriving (Eq, Show)

performAdd :: AddAction -> MusicAlbumSource -> IO ()
performAdd action source@MusicAlbumSource {..} = do
    putStrLn $ "Now adding " <> show sourceLocation
    Path.ensureDir targetLocation
    case coverImage of
        Nothing              -> return ()
        Just fullSourceImage -> do
            fullTargetImage <- Path.parseRelFile "cover.jpg" >>= \fn -> pure $ targetLocation </> fn
            perform fullSourceImage fullTargetImage
    forM_ tracks $ \track@MusicTrackSource {..} -> do
        let fullSourceFile = sourceLocation </> sourceFilename
        let fullTargetFile = targetLocation </> targetFilename
        Path.ensureDir $ Path.parent fullTargetFile
        perform fullSourceFile fullTargetFile
    updateMetadata source
    isEmptySource <- isEmptyDir sourceLocation
    when (action == Move && isEmptySource) $ Path.removeDir sourceLocation
  where
    perform a b = do
        putStrLn $ "    " <> show action <> " " <> show a <> "  ->  " <> show b
        case action of
            Copy -> Path.copyFile a b
            Move -> Path.renameFile a b

hoardAdd :: [String] -> IO ()
hoardAdd args = do
    config <- getConfig
    let hoard = hoardFromArgsOrDefault config args
    let annex = isFlagInArgs args "annex"
    srcs  <- catMaybes <$> mapM (getSource args hoard) (deflagArgs args ["artist", "genres"])
    srcs' <- if isFlagInArgs args "noninteractive"
        then pure srcs
        else Editor.runUserEditorDWIM ".hoard.yml" (toPrettyYaml srcs) >>= Data.Yaml.decodeThrow
    putStrLn $ BS.toString $ toPrettyYaml srcs'
    mapM_ (performAdd $ if annex then Move else Copy) srcs'

hoardAnnex :: [String] -> IO ()
hoardAnnex args = hoardAdd $ "--annex" : args
