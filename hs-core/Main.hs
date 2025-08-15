import Formatta (toggleFormatOnSave)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("toggle" : rest) -> putStrLn $ toggleFormatOnSave (getInputArg rest)
    ("format" : file : _) -> putStrLn $ "Format file: " ++ file -- Replace with your format logic
    ("version" : _) -> putStrLn "Formatta CLI v0.1.0.0"
    ("help" : _) -> putStrLn helpMessage
    _ -> putStrLn $ invalidCommandMessage args

invalidCommandMessage :: [String] -> String
invalidCommandMessage args =
  unlines $
    ["Error: Invalid command '" ++ unwords args ++ "'", ""] ++ lines helpMessage

helpMessage :: String
helpMessage =
  unlines
    [ "Formatta CLI - Usage:",
      "  formatta toggle <true|false>   Toggle format on save.",
      "  formatta format <file>         Format the given file.",
      "  formatta version               Show version."
    ]

getInputArg :: [String] -> String
getInputArg (x : _) = x
getInputArg [] = ""
