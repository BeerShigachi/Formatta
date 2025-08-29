module Commands.Help (showHelp, showInvalidCommandMessage) where

helpMessage :: String
helpMessage =
  unlines
    [ "Formatta CLI - Usage:",
      "  formatta toggle <true|false>   Toggle format on save.",
      "  formatta format <file>         Format the given file.",
      "  formatta version               Show version."
    ]

showHelp :: IO ()
showHelp = putStrLn helpMessage

showInvalidCommandMessage :: [String] -> String
showInvalidCommandMessage args =
  unlines $
    ["Error: Invalid command '" ++ unwords args ++ "'", ""] ++ lines helpMessage