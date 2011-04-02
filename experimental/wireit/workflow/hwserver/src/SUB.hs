
{-# OPTIONS_GHC -fglasgow-exts #-}
module SUB where
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

subworkflow =
    let
        feval ( [fdatum] ) =
                let f = toInt fdatum in
		       if ( f > 0 ) then
			   [ toIntDatum ( f * f ), toMissingDatum ]
		       else if ( f < 0 ) then
			   [ toMissingDatum, toIntDatum ( f * f ) ]
		       else
			   [ toIntDatum ( f * f ), toIntDatum ( f + f ) ]

    in do
        makeOutPipe "outputL" 
        makeOutPipe "outputR" 
        makePipe "impipe0" 

        makeFunction feval  ["impipe0"] ["outputL","outputR"]


        push "impipe0" (n)

        return ()

main =
    do
    putStrLn "nRunning mainflow:n"
    (Just s0) <- startWorkflow (subworkflow [(toIntDatum 11)])
    ( sn, [ output1,output2 ] ) <- runWorkflow s0
    putStrLn $ "output=" ++ (show output1)
    WOOLUtil.printWorkflowActivities sn
    WOOLUtil.printWorkflowPipes sn