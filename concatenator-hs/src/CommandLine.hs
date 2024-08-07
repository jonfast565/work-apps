module CommandLine(parseArguments, CommandLineOptions(..)) where

import Options.Applicative

data CommandLineOptions = CommandLineOptions
  { extension :: String
  , directory :: String
  , excludeFolders :: [String]
  , outputFolder :: String
  , outputFilename :: String
  } deriving Show

commandLineParser :: Parser CommandLineOptions
commandLineParser = CommandLineOptions
  <$> strOption
      ( long "extension"
     <> short 'e'
     <> metavar "EXT"
     <> help "File extension to filter (e.g., 'txt')" )
  <*> strOption
      ( long "directory"
     <> short 'd'
     <> metavar "DIR"
     <> help "Directory path to scan" )
  <*> many (strOption
      ( long "exclude-folder"
     <> short 'x'
     <> metavar "FOLDER"
     <> help "Folder to exclude from scan (can be used multiple times)" ))
  <*> strOption
      ( long "output-folder"
     <> short 'o'
     <> metavar "OUTPUT_DIR"
     <> help "Output directory for results" )
  <*> strOption
      ( long "output-filename"
     <> short 'f'
     <> metavar "FILENAME"
     <> help "Output filename" )

optsParserInfo :: ParserInfo CommandLineOptions
optsParserInfo = info (commandLineParser <**> helper)
  ( fullDesc
 <> progDesc "Tool for filtering files by extension in a directory"
 <> header "File Filter Tool" )

parseArguments :: IO CommandLineOptions
parseArguments = execParser optsParserInfo