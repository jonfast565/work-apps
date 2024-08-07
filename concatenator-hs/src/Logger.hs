module Logger
  ( initLogging,
    getNewLogger,
  )
where

import System.IO (stdout)
import System.Log.Handler.Simple as HsLoggerSimple
import System.Log.Logger as HsLogger

initLogging :: IO ()
initLogging = do
  updateGlobalLogger rootLoggerName (setLevel INFO)
  logHandler <- streamHandler stdout INFO
  updateGlobalLogger rootLoggerName (addHandler logHandler)

getNewLogger :: String -> IO Logger
getNewLogger = HsLogger.getLogger