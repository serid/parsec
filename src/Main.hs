module Main where

import JitLib.Result
import JitLib.Json

main :: IO ()
main = do putStrLn "Hello world!"
          test "[200, null, true]"
          test "{\"a\":  100 , \"b\": [200, null, true] , }"

test :: String -> IO ()
test input = do putStrLn ""
                putStrLn ("Input: " ++ input)
                putStrLn ("Output: " ++ (show $ unwrap $
                  parseJs input))
