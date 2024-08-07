module Utilities
    ( andIO, andAllIO
    ) where

andIO :: IO Bool -> IO Bool -> IO Bool
andIO ioA ioB = do
  a <- ioA
  b <- ioB
  return (a && b)

andAllIO :: [IO Bool] -> IO Bool
andAllIO [] = return True
andAllIO (x:xs) = do
    result <- x
    if result
        then andAllIO xs
        else return False