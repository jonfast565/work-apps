module PartialCsv where

import System.Environment (getArgs)
import GHC.IO ()

-- gives an array some indexes
zipArray :: (Num b, Enum b) => [a] -> [(a, b)]
zipArray ls = zip ls [0..]

-- get header, and filter file to desired lines
processLines :: Int -> Int -> String -> IO String
processLines firstLine lastLine document =
    let ls = lines document
        header = [x | (x, i) <- zipArray ls, i == 0]
        filtered = [x | (x, i) <- zipArray ls, i >= firstLine, i <= lastLine]
        content = unlines (header ++ filtered) in
    return content

-- helper to get arg position
getArgPositionString :: Int -> IO String
getArgPositionString idx = getArgs >>= \x -> return (x !! idx)

getArgPositionInt :: Int -> IO Int
getArgPositionInt idx = getArgs >>= \x -> return (read (x !! idx) :: Int)

-- get the input file
getInputFile :: IO String
getInputFile = do
    inputFilePath <- getArgPositionString 0
    putStrLn $ "Get content from " ++ inputFilePath
    readFile inputFilePath

-- process the file
processFile :: String -> IO String
processFile content = do
    inputFilePath <- getArgPositionString 0
    putStrLn $ "Process file " ++ inputFilePath
    argFirstLine <- getArgPositionInt 1
    argLastLine <- getArgPositionInt 2
    processLines argFirstLine argLastLine content

-- write output
writeOutput :: String -> IO ()
writeOutput fileLines = do
    argOutputFilePath <- getArgPositionString 3
    putStrLn $ "Write file " ++ argOutputFilePath
    writeFile argOutputFilePath fileLines

-- info, etc.
printHeader :: IO ()
printHeader = do
    putStrLn "--- CSV Partial ---"
    putStrLn "partialcsv.exe <inputFilePath> <firstLineNumber> <lastLineNumber> <outputFilePath>"

-- do stuff
main :: IO ()
main = do
    printHeader
    content <- getInputFile
    fileLines <- processFile content
    writeOutput fileLines
    putStrLn "Done!"
    return ()

