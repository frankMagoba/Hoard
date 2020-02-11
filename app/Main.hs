module Main where

import Hoard.Add
import Hoard.Create
import Hoard.Help
import Hoard.List
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )

main :: IO ()
main = do
    args <- getArgs
    if null args
        then hoardHelp [] >> exitFailure
        else pure ()
    let (command:cargs) = args
     in case command of
            "add" -> hoardAdd cargs
            "annex" -> hoardAnnex cargs
            "help" -> hoardHelp cargs
            "create" -> hoardCreate cargs
            "list" -> hoardList cargs
            _ ->
                errorWithoutStackTrace $
                "'" <> command <> "' is not a Hoard command. See 'hoard help'."
