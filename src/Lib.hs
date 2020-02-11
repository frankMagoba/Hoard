{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( maybeValueFromArgs
    , valueFromArgs
    , valueFromArgsOrPrompt
    , isFlagInArgs
    , yesNoPrompt
    , hoardFromArgsOrDefault
    , deflagArgs
    ) where

import Data.List    ( find )
import Data.Maybe   ( fromMaybe )
import Hoard.Config
import System.IO    ( hFlush, stdout )

valueFromArgs :: [String] -> String -> String -> String
valueFromArgs args name def = fromMaybe def $ maybeValueFromArgs args name

maybeValueFromArgs :: [String] -> String -> Maybe String
maybeValueFromArgs args name = lookup args
  where
    lookup (('-':'-':k):v:xs) | name == k = Just v
                              | otherwise = lookup (v : xs)
    lookup ['-':'-':k] | name == k = errorWithoutStackTrace $ "maybeValueFromArgs: Missing value for flag " <> name
                       | otherwise = Nothing
    lookup _ = Nothing

promptUser :: String -> IO String
promptUser p = putStr (p <> " ") >> hFlush stdout >> getLine

valueFromArgsOrPrompt :: [String] -> String -> String -> IO String
valueFromArgsOrPrompt args name prompt = case maybeValueFromArgs args name of
    Just val -> pure val
    Nothing  -> promptUser prompt

isFlagInArgs :: [String] -> String -> Bool
isFlagInArgs args name = "--" ++ name `elem` args

yesNoPrompt :: String -> IO Bool
yesNoPrompt prompt = promptUser (prompt <> " [y/N]") >>= \case
    "Y" -> pure True
    "y" -> pure True
    _   -> pure False

hoardFromArgsOrDefault :: Config -> [String] -> Hoard
hoardFromArgsOrDefault Config {..} args = case found of
    Just h -> h
    _      -> error "No hoard found"
  where
    name  = fromMaybe configDefaultHoard $ maybeValueFromArgs args "hoard"
    found = find (\h -> hoardName h == name) configHoards

deflagArgs :: [String] -> [String] -> [String]
deflagArgs ['-':'-':x] valued
    | x `elem` valued = error $ "deflagArgs: Missing value for flag " <> x
    | otherwise       = []
deflagArgs (('-':'-':x):xs) valued
    | x `elem` valued = deflagArgs (tail xs) valued
    | otherwise       = deflagArgs xs valued
deflagArgs (x:xs) valued = x : deflagArgs xs valued
deflagArgs []     _      = []
