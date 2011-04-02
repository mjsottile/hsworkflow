{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module ControllerWOOL where

import Control.Monad.State
import qualified Data.Map as M
import Happstack.Server 
import Text.StringTemplate
import System.FilePath
import Data.Char
import StateVersions.AppState1
import View

import ControllerMisc
import ControllerStressTests
import ControllerPostActions
import Happstack.Helpers
import Misc 
import Text.StringTemplate.Helpers
import Data.Monoid

import WOOL
import WOOLUtil
import WOOLIO
import FromDataInstances
import System.IO.Unsafe
import Data.IORef



globalWFState :: IORef (WFState IO)
globalWFState = unsafePerformIO $ newIORef defaultInitialState

globalStateSet st = writeIORef globalWFState st

globalStateGet = readIORef globalWFState

staticfiles :: ServerPartT IO Response
staticfiles = msum [ staticserve "static"
                     , staticserve "userdata"
                     , browsedir "projectroot" "."
                     , browsedirHS "templates" "templates"
                     , browsedirHS "src" "src" 
                     ] 
  where staticserve d = dir d (fileServe [] d)

-- main controller
controller :: STDirGroups String -> Bool -> Bool -> ServerPartT IO Response
controller tDirGroups dynamicTemplateReload allowStressTests =  
    -- staticfiles handler *has* to go first, or some content (eg images) will fail to load nondeterministically,
    -- eg http://localhost:5001/static/Html2/index.html (this loads ok when staticfiles handler goes first,
    -- but has the problem when staticfiles handler goes after tutorial handler)
    -- Also interesting: the order doesn't matter when dynamicTemplateReload is false
    -- This still feels to me like a bug: it was quite a headache to diagnose, and why should
    -- the order of the static content handler matter anyway?
    -- At the very least, fileServer should have a highly visible comment warning about this problem.
   staticfiles      
   `mappend` tutorial tDirGroups dynamicTemplateReload allowStressTests  
   `mappend` (return . toResponse $  "Quoth this server... 404.")


-- with directoryGroupsOld (lazy readFile), appkiller.sh causes crash
-- directoryGroupsHAppS is defined in happstack-helpers.  This is where
-- the assumption that all the templates lie in a subdirectory called
-- templates comes from
getTemplateGroups :: (Stringable a) => IO (M.Map FilePath (STGroup a))
getTemplateGroups = directoryGroupsHAppS "templates" 

tutorial :: STDirGroups String -> Bool -> Bool -> ServerPartT IO Response
tutorial tDirGroups' dynamicTemplateReload allowStressTests = do
  -- A map of template groups, with the key being the containing directory name 
  -- If true, Redo IO action for fetching templates (which was also done in main)
  -- so templates are loaded from templates dir for every request.
  -- which lets you change templates interactively without stop/starting the server
  -- but has a higher server disk read load. Useful for development, bad for performance under a heavy load.
  rq <- askRq
  tDirGroups <- liftIO $ if dynamicTemplateReload
    then getTemplateGroups 
    else return tDirGroups' 
  
  mbSess <- liftIO $ getmbSession rq
  tutorialCommon allowStressTests  $ RenderGlobals rq tDirGroups mbSess
  
tutorialCommon :: Bool -> RenderGlobals -> ServerPartT IO Response
tutorialCommon allowStressTests rglobs = msum 
   [ exactdir "/" (( return . tutlayoutU rglobs [] ) "home")
     , dir "tutorial" $ msum [
         dir "logout" (logoutPage rglobs) 
         , dir "changepassword" (methodSP POST $ changePasswordSP rglobs)
         , dir "step" (methodSP POST $ stepSP rglobs)
         , dir "actions" $ msum 
                 [ dir "login" (methodSP POST $ loginPage rglobs)
                   , dir "newuser" (methodSP POST $ newUserPage rglobs) 
                 ]
         , dir "initializedummydata" (spAddDummyData rglobs)
         , spJustShowTemplate rglobs
   ] ]            
  
spJustShowTemplate :: (Monad m) => RenderGlobals -> ServerPartT m Response
spJustShowTemplate rglobs = lastPathPartSp0 $ const (return . tutlayoutU rglobs []) 


{-
ServerPartTs conceptually just take a request to a response.

A Happstack webserver is fundamentally just a ServerPartT.  
This is where the Monoid instance of ServerPartT comes in, as many ServerPartTs can
be combined together with mappend.  mzero corresponds to a 404 and 
mzero `mappend` f = f, while if f is not mzero then f `mappend` g = f.
This means that when the wrapped function is run, the combination of ServerPartTs will be
tried from left to right until one does not return mzero.  If they all return mzero the
final result of the request is a 404. 

-}

-- pretty much useless little server part constructor, for demo purposes
simplematch :: String -> ServerPartT IO Response
simplematch u = do
    rq <- askRq
    let ru = rqURL rq
    if ru == ("/simplematch" ++ u)
       then ( return . toResponse ) ( "matched " ++ u) 
       else mzero



wfExtract st =
    do
        let oPipeData = getWorkflowPipeData st "Counter"
        putStrLn "Data for pipe Counter:"
        WOOLUtil.printData oPipeData
        putStrLn "\nState of the entire workflow:"
        WOOLUtil.printWorkflow st


formatPipeData st =
    let pipes = getPipes st
        prows = map perpipe pipes
                where perpipe (Pipe t d ) = [ t, (show d) ]
    in
        prows

formatActivityData st =
    let ops = getOps st
        prows = map perop ops
                where
                    perop (o) = [ (show (getOperatorID o)), (show o) ]
    in
        prows




stepSP :: RenderGlobals -> ServerPartT IO Response
stepSP rglobs = do 
                    StepInfo numticks <- getData'
                    liftIO $ putStrLn "Stepping..."
                    liftIO $ putStrLn $ "..." ++ (show numticks) ++ " ticks"

                    st <- liftIO $ globalStateGet
                    (Just st') <- lift $ stepWorkflow st
                    liftIO $ globalStateSet st'
                    liftIO $ wfExtract st'

                    let pag = Pagination { currentbar = 1
                                         , resultsPerBar = 5
                                         , currentpage = 1
                                         , resultsPerPage = 5
                                         , baselink = "step"
                                         , paginationtitle = "Step Results: "}
                        pipeCells = formatPipeData st'
                        paintPipesTable _ j p = 
                            paintTable (Just [ "<span class=\"pipeTag\">Tag</span>", "<span class=\"pipeContents\">Contents</span>" ])
                                       j
                                       (Just p)
                        pipesTable = paintPipesTable rglobs pipeCells pag

                        actCells = formatActivityData st'
                        paintActsTable _ j p = 
                            paintTable (Just [ "<span class=\"threadId\">Tag</span>", "<span class=\"activityInfo\">Contents</span>" ])
                                       j
                                       (Just p)
                        actsTable = paintActsTable rglobs actCells pag

                        attrs = [   ("statusmsgStep", "STEPPED")
                                 , ( "stateOutput",  "stateOutput" )
                                 , ( "stateDebug",  "stateDebug" )
                                 , ( "statePipes",  pipesTable )
                                 , ( "stateActivities",  actsTable )
                                ]
                    return $ tutlayoutU rglobs attrs "debug"

