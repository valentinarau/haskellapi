{-# LANGUAGE OverloadedStrings #-}

module Shortener (shortener, printTime, printConfig, printJson) where

import Web.Scotty
import Data.Time (getCurrentTime)
import Data.Aeson (encode)


shortener :: IO ()
shortener =
    scotty 3000 $
        get "/" $
        html "<h1>Welcome</h1>"

printConfig :: IO ()
printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents


printTime :: IO ()
printTime = do
  time <- getCurrentTime
  print time

numbers :: [Integer]
numbers = [1,2,3,4]

printJson :: IO ()
printJson = print (encode numbers)
-- funcion :: [Char] -> Bool
-- funcion palabra = True 