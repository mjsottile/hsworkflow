-- imgdiff


{-# OPTIONS_GHC -fglasgow-exts #-}
module IMGDIFF where
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

imgdiffworkflow inargs =
    let
        buildDeltaName lpath rpath =
	    let
		lnameext = takeFileName( lpath )
		lname = dropExtension( lnameext )
		rnameext = takeFileName( rpath )
		rname = dropExtension( rnameext )
		delta = lname ++ "_" ++ rname ++ ".bmp"
	    in
		delta

        simpleDiff1 ( Color4 lr lg lb la ) ( Color4 rr rg rb ra ) =
	    let
		newr = if ( ( lr == rr ) ) then lr else 0
		newg = if ( ( lg == rg ) ) then lg else 0
		newb = if ( ( lb == rb ) ) then lb else 0
		newa = if ( ( la == ra ) ) then la else 0
	    in
		(Color4 newr newg newb newa)

        simpleDiff2 ( Color4 lr lg lb la ) ( Color4 rr rg rb ra ) =	
	    let
		newr = ( lr + rr )
		newg = ( lg + rg )
		newb = ( lb + rb )
		newa = ( la + ra )
	    in
		(Color4 newr newg newb newa)

        computeDelta :: (Monad m,MonadIO m) => OperatorID -> [Token] -> WorkflowT m [Token]
        computeDelta threadId ( [ l, r ] ) =
           if isSignal l then do
               return $ [toMissingToken]
           else
               let
                   ( lfilenamed, limgd ) = toPair l
                   lfilename = toString lfilenamed
                   limg = toString limgd
               
                   ( rfilenamed, rimgd ) = toPair r
                   rfilename = toString rfilenamed
                   rimg = toString rimgd
               in do
                   (lBMH,lBMW,lBMHeader,lBMData) <- parseBitmap limg Nothing
                   (rBMH,rBMW,rBMHeader,rBMData) <- parseBitmap rimg Nothing
                   let deltaBMData = zipWith simpleDiff1 lBMData rBMData
                   let deltaBM = (lBMH,lBMW,lBMHeader,deltaBMData)
                   let deltaName = buildDeltaName lfilename rfilename
                   delta <- formatBitmap deltaBM
                   liftIO $                putStrLn $ "computeDelta[" ++ deltaName ++ "] "
                   return $ [ toPairToken ( toStringToken deltaName, toStringToken delta ) ]
        generateIndex :: (Monad m,MonadIO m) => OperatorID -> [Token] -> WorkflowT m [Token]
        generateIndex threadId ( [s, d] ) =
           let
               sources = toList s
               deltas = toList d
               pageHeader = "<html><body>"
               pageFooter = "</body></html>"
               insertImage (nm) = "<td><img width=256 src='" ++ (toString nm) ++ "'/></td>n"
               addImages [] buf = buf
               addImages (i:is) buf = addImages is $ buf ++ (insertImage i)
               pagetop = pageHeader ++ "<h3>Sources</h3><table><tr>" ++ (addImages sources []) ++ ("</tr></table>")
               pagebottom = "<h3>Deltas</h3><table><tr><td width=128> </td>" ++ (addImages deltas []) ++ ("</tr></table>") ++ pageFooter
           in do
               return $ [ toPairToken ( toStringToken "imgdiff.html", toStringToken ( pagetop ++ pagebottom ) ) ]

    in do
        makeOutPipe "output" 
        makePipe "impipe0" 
        makePipe "impipe1" 
        makePipe "impipe2" 
        makePipe "impipe3" 
        makePipe "impipe4" 
        makePipe "impipe5" 
        makePipe "impipe8" 
        makePipe "impipe7" 
        makePipe "impipe9" 
        makePipe "impipe10" 
        makePipe "impipe11" 
        makePipe "impipe12" 

        makeFunctionM computeDelta  ["impipe8","impipe7"] ["impipe9"]
        makeFunctionM generateIndex  ["impipe2","impipe11"] ["impipe12"]
        makeReplicate "impipe1" ["impipe2","impipe3"]
        makeReplicate "impipe5" ["impipe8","impipe7"]
        WOOLIO.makeDirListWithSuffix ".bmp" "impipe0" "impipe1"
        makeSerializer "impipe3" "impipe4"
        WOOLIO.makeFileRead "impipe4" "impipe5"
        WOOLIO.makeFileWrite (Just "./data/imagedeltas/") Nothing "impipe9" "impipe10"
        WOOLIO.makeFileWrite (Just "./") Nothing "impipe12" "output"

        makeTake "impipe10" "impipe11" 4

        push "impipe0" (toStringToken "./data/images")
        push "impipe8" (toSignalToken )

        return ()

main =
    do   
       (Just s0) <- startWorkflow (imgdiffworkflow [])
       (sn, result) <- proceedWorkflow s0 []
       putStrLn $ "result=" ++ (show result)     

       putStrLn "All done"