module Hoard.Create
    ( hoardCreate
    ) where

import Control.Monad   ( when )
import Hoard.Config
import Hoard.Database
import Lib
import Path 

hoardCreate :: [String] -> IO ()
hoardCreate args = do
    name     <- valueFromArgsOrPrompt args "name" "Name:"
    location <- valueFromArgsOrPrompt args "location" "Location:" >>= parseAbsDir
    config   <- getConfig
    when (name `elem` (hoardName <$> configHoards config))
        $  errorWithoutStackTrace
        $  "Hoard with name '"
        <> name
        <> "' already exists."
    let makeDefault = isFlagInArgs args "default"
    let hoard       = Hoard {hoardName = name, hoardLocation = location}
    let defaultHoard = if makeDefault || null (configHoards config)
            then name
            else configDefaultHoard config
    let config' = config { configHoards       = configHoards config ++ [hoard]
                         , configDefaultHoard = defaultHoard
                         }
    sqlFile <- parseRelFile "hoard.sqlite"
    createDb $ show $ location </> sqlFile
    saveConfig config'
    putStrLn
        $  "Created hoard named '"
        <> name
        <> "' in location '"
        <> show location
        <> "'"
