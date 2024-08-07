{-# LANGUAGE ScopedTypeVariables #-}

module FileManager (walkFiles, walkFilesDefault, defaultConcatenatorFilters, readFileToString, writeNewFile) where

import Control.Monad (filterM)
import Control.Exception (IOException)
import qualified Control.Exception as E

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (splitPath, takeExtension, takeFileName)
import Logger (getNewLogger)

import System.Log.Logger (logL, Priority (INFO))
import CommandLine (CommandLineOptions, outputFolder, outputFilename)

type FileFilter = FilePath -> IO Bool

walkFiles :: FileFilter -> FilePath -> IO [FilePath]
walkFiles fileFilter path = do
    logger <- getNewLogger "file walk"
    logL logger INFO ("Listing directories for path " ++ path)
    temp <- listDirectory path
    logL logger INFO ("Listing files for path " ++ path)
    files <- filterM fileFilter temp
    directories <- filterM doesDirectoryExist temp
    internalFiles <- mapM (walkFiles fileFilter) directories
    return $ files ++ concat internalFiles

walkFilesDefault :: FilePath -> IO [FilePath]
walkFilesDefault = walkFiles defaultFileFilter

defaultFileFilter :: FileFilter
defaultFileFilter = doesFileExist

checkFileExtension :: String -> FileFilter
checkFileExtension ext path = return (takeExtension path == ext)

checkPathSegments :: [String] -> FileFilter
checkPathSegments segments path = do
  let pathSegments = splitPath path
      fileName = takeFileName path
      allSegments = pathSegments ++ [fileName]
  return $ not (any (`elem` segments) allSegments)

defaultConcatenatorFilters :: String -> [String] -> FileFilter
defaultConcatenatorFilters extension excludeFolderParts =
  let filters = [ defaultFileFilter,
                  checkFileExtension extension,
                  checkPathSegments excludeFolderParts]
  in \path -> and <$> mapM ($ path) filters

readFileToString :: FilePath -> IO [String]
readFileToString path = do
  result <- tryReadFile path
  case result of
    Left _ -> return []
    Right content -> return [content]

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile path = do
  result <- E.try $ readFile path
  case result of
    Left (e :: IOException) -> return (Left e)
    Right content -> return (Right content)

writeNewFile :: CommandLineOptions -> String -> IO ()
writeNewFile opts content = do 
  logger <- getNewLogger "writer"
  logL logger INFO ("Writing files: " ++ (outputFilename opts))
  writeFile (outputFolder opts ++ "/" ++ outputFilename opts) content