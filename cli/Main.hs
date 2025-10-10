import Commands.Help (showHelp, showInvalidCommandMessage)
import Commands.Toggle (toggleFormatOnSave)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("toggle" : rest) -> putStrLn $ toggleFormatOnSave (getInputArg rest)
    ("format" : file : _) -> putStrLn $ "Format file: " ++ file -- Replace with your format logic
    ("version" : _) -> putStrLn "Formatta CLI v0.1.0.0"
    ("help" : _) -> showHelp
    _ -> putStrLn $ showInvalidCommandMessage args

getInputArg :: [String] -> String
getInputArg (x : _) = x
getInputArg [] = ""
