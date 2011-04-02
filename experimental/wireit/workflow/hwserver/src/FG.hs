-- The f/g loop example


{-# OPTIONS_GHC -fglasgow-exts #-}
module FG where
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

fgworkflow inargs =
    let
        g_eval ( idatum:_ ) = let i = toInt idatum in [ toIntToken( i `mod` 100 )]
        tuple_activity ( idatum:_ ) = let i = toInt idatum in [ toIntToken( i + 1 ), toIntToken( i * 2 ) ]
        output st =
               let
                   (_:o:_:_) = getPipes st
                   result = getPipeData o
               in
                   toListToken result

    in do
        makeOutPipe "wfOutput" 
        makePipe "o" 
        makePipe "impipe1" 
        makePipe "impipe2" 

        makeFunction g_eval  ["impipe2"] ["impipe1"]
        makeFunction tuple_activity  ["impipe1"] ["o","impipe2"]


        makeTimeout "wfOutput" output 5
        push "impipe1" (toIntToken 1)

        return ()

main =
    do
        putStrLn "fg"
        (Just s0) <- startWorkflow (fgworkflow)
        (sn, output) <- proceedWorkflow s0 []
        putStrLn $ "output=" ++ (show output)