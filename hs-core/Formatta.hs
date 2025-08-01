module Formatta (toggleFormatOnSave) where

import Data.Char (toLower)

parseBool :: String -> Maybe Bool
parseBool "true" = Just True
parseBool "false" = Just False
parseBool _ = Nothing

toggle :: Bool -> Bool
toggle = not

boolToLowerStr :: Bool -> String
boolToLowerStr b = map toLower (show b)

toggleFormatOnSave :: String -> String
toggleFormatOnSave input =
  case parseBool input of
    Just b -> boolToLowerStr (toggle b)
    Nothing -> "error"
