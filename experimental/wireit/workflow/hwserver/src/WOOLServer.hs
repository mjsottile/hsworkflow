-- with gracious thanks to mightybyte:
-- http://softwaresimply.blogspot.com/2008_02_01_archive.html

module WOOLServer where
import Happstack.Server hiding (port)
import Happstack.State
import ControllerWOOL
import System.Environment
import StateVersions.AppState1
import Happstack.Server.Helpers
import WOOL
import WOOLUtil
import WOOLIO

runWOOLServer args wf = do
  case args of
    [port, dynamicTemplateReload', allowStressTests'] -> do
       let p = read port
           allowStressTests = read allowStressTests'
           dynamicTemplateReload = read dynamicTemplateReload'
       tDirGroups <- getTemplateGroups -- defined in Controller.hs
       {- smartserver and its cousin smartserver' are from the happstack-helpers package.
          They are essentially a combination startSystemState and simpleHTTP('), with a
          few other minor conveniences built in.

          Just like with simpleHTTP we pass in the ServerPartT, given by controller from Controller.hs,
          and just like startSystemState we pass in a Proxy in order to initialize the system state.
        -}
       
       (Just s0) <- startWorkflow wf
       globalStateSet s0
       
       -- s00 <- evilGet
       -- sn <- runWorkflowNCycles s00 10
       -- wfExtract sn
       
       smartserver (Conf p Nothing) "happs-tutorial"
                    (controller tDirGroups dynamicTemplateReload allowStressTests)
                    stateProxy
    _ -> putStrLn "usage example: happs-tutorial 5001 True True (starts the app on port 5001, \
                      \templates reload dynamically on every request, allows stress tests)"


stateProxy :: Proxy AppState
stateProxy = Proxy
