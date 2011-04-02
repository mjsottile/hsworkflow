module Main where
import System.Environment
import WOOL
import WOOLUtil
import WOOLIO
import WOOLPrimitives
import WOOLServer
import qualified EVENODD as WF

main = do
    args <- getArgs -- getArgs is from System.Environment, if you're not familiar with it
    runWOOLServer args WF.evenoddworkflow