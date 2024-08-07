module Main (main) where

import CommandLine (parseArguments)
import Logger (initLogging, getNewLogger)
import System.Log.Logger (logL, Priority (INFO))
import Concatenator (concatFiles)
import FileManager (writeNewFile)

main :: IO ()
main = do
    initLogging
    logger <- getNewLogger "concatenator"
    logL logger INFO "--- Concatenator ---"
    args <- parseArguments
    concatted <- concatFiles args
    writeNewFile args concatted
    logL logger INFO "Done!"
    return ()
