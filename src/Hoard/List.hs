{-# LANGUAGE RecordWildCards #-}

module Hoard.List
    ( hoardList
    ) where

import Control.Monad ( forM_ )
import Hoard.Config
import Lib

padString :: Int -> String -> String
padString n s = take n $ s ++ repeat ' '

hoardList :: [String] -> IO ()
hoardList args = do
    config <- getConfig
    let namePad = padString 30
    let locPad  = padString 50
    putStrLn $ namePad "Hoard name" <> locPad "Location" <> "Default"
    forM_ (configHoards config) $ \Hoard {..} ->
        putStrLn $ namePad hoardName <> locPad (show hoardLocation) <> show
            (configDefaultHoard config == hoardName)
