module Main (main) where

import Data.Char (toLower)
import System.Environment (getArgs)
import System.Exit (exitFailure)

parseBool :: String -> Maybe Bool
parseBool "true" = Just True
parseBool "false" = Just False
parseBool _ = Nothing

toggle :: Bool -> Bool
toggle = not

getInput :: [String] -> String
getInput (x : _) = x
getInput [] = "false"

boolToLowerStr :: Bool -> String
boolToLowerStr b = map toLower (show b)

main :: IO ()
main = getArgs >>= handleInput . getInput
  where
    handleInput input = case parseBool input of
      Just b -> putStrLn (boolToLowerStr (toggle b))
      Nothing -> putStrLn "Error: Input must be 'true' or 'false'." >> exitFailure
