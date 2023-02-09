module Main where

import Options.Applicative

data Options = Options { string :: String }

options :: Parser Options
options = Options
      <$> strOption
          ( long "string"
         <> metavar "STRING"
         <> help "The string you want to de-quote" )

dequote :: String -> String
dequote = map 

program :: Options -> IO()
program opts = do 
    mapped <- mapM 
    putStrLn "test"

main :: IO ()
main = program =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
  