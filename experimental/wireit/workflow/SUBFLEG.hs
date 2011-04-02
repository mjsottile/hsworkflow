-- Example using subflow sub


{-# OPTIONS_GHC -fglasgow-exts #-}
module SUBFLEG where
import WOOL 
import WOOLUtil 
import WOOLPrimitives 
import qualified WOOLWeb 
import qualified WOOLIO 
import BMP 
import System.FilePath 
import Control.Monad.State 
import Control.Monad.Identity 
import Control.Concurrent 
import Network.HTTP; 
import Text.Printf 
import IO 
import List 
import System.IO.HVFS.Utils 
import Data.String.Utils 
import System.Path 
import Data.Char 
import System.Random; 
import qualified Text.HTML.TagSoup as TS 
import System.Directory 

import qualified SUB as SUBSF

subflegworkflow inargs =
    let

    in do
        makeOutPipe "output" 
        makePipe "impipe8" 
        makePipe "impipe5" 
        makePipe "impipe6" 
        makePipe "impipe7" 

        makeReplicate "impipe8" ["impipe8","impipe7"]
        makeWorkflow (SUBSF.subworkflow [])["impipe7"] ["impipe5","impipe6"] 

        makeTake "impipe5" "output" 3
        makeTake "impipe6" "output" 5

        push "impipe8" (toIntToken 11)
        push "impipe8" (toIntToken 2)
        push "impipe8" (toIntToken 5)

        return ()

main =
    do
    putStrLn "Running mainflow:"
    (Just s0) <- startWorkflow (subflegworkflow [])
    ( sn, [ output ] ) <- proceedWorkflow s0 []
    putStrLn $ "output=" ++ (show output)