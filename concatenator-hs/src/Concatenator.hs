module Concatenator (concatFiles) where
    
import CommandLine (CommandLineOptions(..))
import FileManager (walkFiles, defaultConcatenatorFilters, readFileToString)
import Logger (getNewLogger)
import System.Log.Logger (logL)
import System.Log (Priority(INFO))
import Control.Monad (forM)

concatFiles :: CommandLineOptions -> IO String
concatFiles opts = do
    fileContents <- getFiles opts
    return $ concat fileContents

getFiles :: CommandLineOptions -> IO [String]
getFiles opts = do
    let directoryPath = directory opts
        extn = extension opts
        excludeFldrs = excludeFolders opts
        filters = defaultConcatenatorFilters extn excludeFldrs

    logger <- getNewLogger "concatenator"
    logL logger INFO ("Concatenating files: " ++ directoryPath)

    walked <- walkFiles filters directoryPath
    contents <- forM walked readFileToString
    return $ concat contents



