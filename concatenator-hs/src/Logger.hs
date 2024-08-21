module Logger
  ( initLogging,
    getNewLogger,
  )
where

import System.Log.Logger as HsLogger

initLogging :: IO ()
initLogging = do
  updateGlobalLogger rootLoggerName (setLevel INFO)

getNewLogger :: String -> IO Logger
getNewLogger = HsLogger.getLogger