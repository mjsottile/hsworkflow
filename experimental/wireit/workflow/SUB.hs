-- This is a module

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

subworkflow inargs =
    let
        feval ( [fdatum] ) =
                let f = toInt fdatum in
		       if ( f > 0 ) then
			   [ toIntToken( f * f ), toMissingToken ]
		       else if ( f < 0 ) then
			   [ toMissingToken, toIntToken ( f * f ) ]
		       else
			   [ toIntToken ( f * f ), toIntToken ( f + f ) ]

    in do
        makeOutPipe "outputL" 
        makeOutPipe "outputR" 
        makeInPipe "impipe0" [] 

        makeFunction feval  ["impipe0"] ["outputL","outputR"]



        return ()