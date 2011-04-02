--
-- helpers
--
-- matt@cs
--
module WF.Utilities where

import System.IO.Unsafe (unsafePerformIO) 

printOut :: String -> IO ()
printOut x = putStrLn $ show x

printer :: String -> ()
printer x = unsafePerformIO (printOut x)
