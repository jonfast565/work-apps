module Main where

import Options.Applicative

newtype Options = Options { string :: String }

options :: Parser Options
options = Options
      <$> strOption
          ( long "string"
         <> metavar "STRING"
         <> help "The string you want to de-quote" )

dequote :: String -> String
dequote = concatMap (\c -> if c == '"' then ['\\', '"'] else [c])

program :: Options -> IO()
program opts = do 
    let mapped = dequote $ string opts 
    putStrLn mapped

main :: IO ()
main = program =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Escapes quotes"
     <> header "--- escape quotes easily ---" )
  