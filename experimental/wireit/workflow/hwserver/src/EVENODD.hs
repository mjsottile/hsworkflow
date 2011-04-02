-- evenodd example


{-# OPTIONS_GHC -fglasgow-exts #-}
module EVENODD where
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

evenoddworkflow   =
    let
        seval [] [i] = seval [toIntToken 0] [i]
        seval [p] [i] =
                   let parity = toInt p in
                   if ( parity == 0 ) then
                       ( [toIntToken 1], [ i, toMissingToken ] )
                   else
                       ( [toIntToken 0], [ toMissingToken, i ] )

    in do
        makeOutPipe "output" 
        makePipe "impipe0" 
        makePipe "impipe1" 

        makeFunctionS seval [] ["impipe0"] ["impipe1","wf3ph3seval"]
        makeCounter 1 999999999 "impipe0"

        makeTake "impipe1" "output" 5


        return ()

main =
    do
           putStrLn "EvenOdd"
           (Just s0) <- startWorkflow (evenoddworkflow)
           (sn, output) <- proceedWorkflow s0 []
           putStrLn $ "output=" ++ (show output)