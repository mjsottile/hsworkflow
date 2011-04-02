-- TagSlurp


{-# OPTIONS_GHC -fglasgow-exts #-}
module TAGSLURP where
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

tagslurpworkflow inargs =
    let
        fwritetags threadId ( [ nametagspair ] ) =
                       let
                           ( named, tagsd ) = toPair nametagspair
                           filename = toString named
                           tags = toList tagsd
                           writetags handle [] = return ()
                           writetags handle (t:ts) = do
                               liftIO $ hPutStr handle $ toString t
                               liftIO $ hPutStr handle "n"
                               writetags handle ts
                           newname = filename ++ ".tags"
                       in do
                           liftIO $                putStrLn $ "fwritetags: [" ++ newname ++ "] " ++ (show (length tags)) ++ " tags"
                           handle <- liftIO $      openFile newname WriteMode
                           writetags handle tags
                           liftIO $ hClose handle
                           return $ [toMissingToken]
        fwriteimg threadId ( [ nameimgpair ] ) =
                   let
                       ( named, imgd ) = toPair nameimgpair
                       filename = toString named
                       img = toString imgd
                   in do
                       rnd <- liftIO $ ( getStdRandom ( randomR (1, 10000) ) :: IO Int )
                       let newname = filename ++ ".imgs/" ++ (show rnd)
                       liftIO $                putStrLn $ "fwriteimg[" ++ newname ++ "] " ++ (show (length img)) ++ " bytes"
                       handle <- liftIO $      openFile newname WriteMode
                       liftIO $    hPutStr handle $ img
                       liftIO $ hClose handle
                       return $ [toMissingToken]
        ffixtags ( [ nametagspair ] ) =
                       let
                           ( named, tagsd ) = toPair nametagspair
                           filename = toString named
                           tags = toList tagsd
                           fixtags [] result = result
                           fixtags (td:ts) result =
                               let
                                   t = toString td
                                   x = if ( "http://" `isPrefixOf` t ) then
                                           [ toPairToken( named, toStringToken t ) ]
                                       else
                                           []
                               in
                                   fixtags ts (result ++ x)
                           fixedtags = fixtags tags []
                       in
                           [toListToken fixedtags]

    in do
        makePipe "GROUND" 
        makePipe "imgurlR" 
        makePipe "impipe2" 
        makePipe "impipe3" 
        makePipe "impipe4" 
        makePipe "impipe5" 
        makePipe "impipe6" 
        makePipe "impipe7" 
        makePipe "impipe8" 
        makePipe "impipe9" 
        makePipe "impipe10" 
        makePipe "impipe12" 
        makePipe "impipe13" 
        makePipe "impipe14" 
        makePipe "impipe16" 
        makePipe "impipe17" 

        makeFunctionM fwritetags  ["impipe6"] ["GROUND"]
        makeFunctionM fwriteimg  ["impipe12"] ["GROUND"]
        makeFunction ffixtags  ["impipe7"] ["impipe8"]
        makeReplicate "impipe5" ["impipe6","impipe7"]
        makeReplicate "impipe9" ["impipe10","imgurlR"]
        makeReplicate "impipe4" ["impipe16","impipe17"]
        makeSerializer "impipe14" "impipe2"
        WOOLIO.makeFileRead "impipe2" "impipe3"
        WOOLWeb.makeURLFetch2 "impipe3" "impipe4"
        WOOLWeb.makeGetTags "impipe16" "impipe5"
        makeSerializer "impipe8" "impipe9"
        WOOLWeb.makeURLFetch2 "impipe10" "impipe12"
        WOOLIO.makeDirListWithSuffix ".url" "impipe13" "impipe14"
        WOOLIO.makeFileWrite Nothing (Just ".page") "impipe17" "GROUND"


        push "impipe13" (toStringToken "./data/urls")

        return ()

main =
    do
           (Just s0) <- startWorkflow (tagslurpworkflow [])

           st <- runWorkflowNCycles s0 150

	   let oPipeData = getWorkflowPipeData st "imgurlR"
           putStrLn "Data for pipe imgurlR:"
           WOOLUtil.printData oPipeData
           putStrLn "nState of the entire workflow:"        

           putStrLn "All done"