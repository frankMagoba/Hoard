{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Hoard.Database
    ( createDb
    ) where

import           Data.Text                       ( Text )
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite            ( Sqlite )
import qualified Database.Beam.Sqlite            as Sqlite
import qualified Database.Beam.Sqlite.Connection as Sqlite
import qualified Database.Beam.Sqlite.Migrate    as Sqlite
import qualified Database.SQLite.Simple          as Sqlite

data MusicDb f =
    MusicDb
        { _musicArtists    :: f (TableEntity MusicArtistT)
        , _musicGenres     :: f (TableEntity MusicGenreT)
        , _musicAlbums     :: f (TableEntity MusicAlbumT)
        , _musicAlbumGenre :: f (TableEntity MusicAlbumGenreT)
        }
    deriving (Generic, Database be)

data MusicArtistT f =
    Artist
        { _artistId      :: Columnar f Int
        , _artistName    :: Columnar f Text
        , _artistPicture :: Columnar f (Maybe Text)
        }
    deriving (Generic, Beamable)

instance Table MusicArtistT where
    data PrimaryKey MusicArtistT f = ArtistId (Columnar f Int)
                                   deriving (Generic, Beamable)
    primaryKey = ArtistId . _artistId

data MusicGenreT f =
    Genre
        { _genreId      :: Columnar f Int
        , _genreName    :: Columnar f Text
        , _genrePicture :: Columnar f (Maybe Text)
        }
    deriving (Generic, Beamable)

instance Table MusicGenreT where
    data PrimaryKey MusicGenreT f = GenreId (Columnar f Int)
                                  deriving (Generic, Beamable)
    primaryKey = GenreId . _genreId

data MusicAlbumT f =
    Album
        { _albumId      :: Columnar f Int
        , _albumArtist  :: PrimaryKey MusicArtistT f
        , _albumName    :: Columnar f Text
        , _albumYear    :: Columnar f Int
        , _albumPicture :: Columnar f (Maybe Text)
        }
    deriving (Generic, Beamable)

instance Table MusicAlbumT where
    data PrimaryKey MusicAlbumT f = AlbumId (Columnar f Int)
                                  deriving (Generic, Beamable)
    primaryKey = AlbumId . _albumId

data MusicAlbumGenreT f =
    AlbumGenre
        { _albumId' :: PrimaryKey MusicAlbumT f
        , _genreId' :: PrimaryKey MusicGenreT f
        }
    deriving (Generic, Beamable)

instance Table MusicAlbumGenreT where
    data PrimaryKey MusicAlbumGenreT f = AlbumGenreId (PrimaryKey
                                                     MusicAlbumT
                                                     f)
                                                  (PrimaryKey MusicGenreT f)
                                       deriving (Generic, Beamable)
    primaryKey = AlbumGenreId <$> _albumId' <*> _genreId'

createDb :: FilePath -> IO ()
createDb path = do
    conn <- Sqlite.open path
    let lenientHooks = defaultUpToDateHooks { runIrreversibleHook = pure True }
    Sqlite.withTransaction conn $ Sqlite.runBeamSqlite conn $ do
        res <- bringUpToDateWithHooks lenientHooks
                                      Sqlite.migrationBackend
                                      initialSetupStep
        case res of
            Nothing -> error "did not run migration"
            Just _  -> return ()
    Sqlite.close conn

initialSetupStep
    :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite MusicDb)
initialSetupStep = migrationStep "initial_setup" (const initialSetup)

initialSetup :: Migration Sqlite (CheckedDatabaseSettings Sqlite MusicDb)
initialSetup =
    MusicDb
        <$> ( createTable "MusicArtists" $ Artist
                { _artistId      = field "id" int notNull unique
                , _artistName    = field "name"
                                         (nationalVarchar Nothing)
                                         notNull
                                         unique
                , _artistPicture = field
                    "picture"
                    (maybeType $ nationalVarchar Nothing)
                }
            )
        <*> ( createTable "MusicGenres" $ Genre
                { _genreId      = field "id" int notNull unique
                , _genreName    = field "name"
                                        (nationalVarchar Nothing)
                                        notNull
                                        unique
                , _genrePicture = field
                    "picture"
                    (maybeType $ nationalVarchar (Just 20))
                }
            )
        <*> ( createTable "MusicAlbums" $ Album
                { _albumId      = field "id" int notNull unique
                , _albumArtist  = ArtistId $ field "artistId" int notNull
                , _albumName    = field "name"
                                        (nationalVarchar Nothing)
                                        notNull
                                        unique
                , _albumYear    = field "year" int notNull
                , _albumPicture = field
                    "picture"
                    (maybeType $ nationalVarchar Nothing)
                }
            )
        <*> ( createTable "MusicAlbumGenres" $ AlbumGenre
                { _albumId' = AlbumId $ field "albumId" int notNull
                , _genreId' = GenreId $ field "genreId" int notNull
                }
            )
