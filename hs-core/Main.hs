import Formatta (toggleFormatOnSave)

main :: IO ()
main = do
  input <- getLine
  putStrLn $ toggleFormatOnSave input
