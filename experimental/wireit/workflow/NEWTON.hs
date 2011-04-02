-- Newton's method


{-# OPTIONS_GHC -fglasgow-exts #-}
module NEWTON where
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

newtonworkflow [n] =
    let
           feval (dbldatum:_) =
               let 
                   f = toDouble dbldatum
                   reldiff :: Double -> Double -> Double
                   reldiff old new = abs (old - new) / old
                   
                   computeNext :: Double -> Double
                   computeNext x = x - f x / f' x
                       where f  x = sin x
                             f' x = cos x
                   f1 = computeNext f
                   result =    if (reldiff f f1) > 0.00000000000005 then
                                   [ toDoubleToken f1, toMissingToken ]
                               else
                                   [ toMissingToken, toDoubleToken f1 ]
               in
                   result
           feval _ = [] 

    in do
        makeOutPipe "OutPipe" 
        makeInPipe "impipe3" [(n)] 
        makePipe "impipe1" 

        makeFunction feval  ["impipe3"] ["impipe1","impipe3"]

        makeTake "impipe1" "OutPipe" 1


        return ()

main =
        do
          putStrLn "newton"
          (Just s0) <- startWorkflow (newtonworkflow [(toIntToken 3)])
          (sn, [ output ]) <- proceedWorkflow s0 []
          putStrLn $ "output=" ++ (show output)