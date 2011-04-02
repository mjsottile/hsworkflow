module ControllerMisc where

import Happstack.Server



import View
import StateVersions.AppState1

import System.Directory (doesFileExist)

import Happstack.State
import Control.Monad.Reader
import Control.Monad.Error
import Happstack.Helpers
import qualified Data.ByteString.Char8 as B

-- The final value is HtmlString so that the Happstack machinery does the right thing with toMessage.
-- If the final value was left as a String, the page would display as text, not html.
-- The only difference is in the content types between the ToMessage instances.
tutlayoutU :: RenderGlobals -> [(String,String)] -> String -> Response
tutlayoutU rglobs attrs = toResponse . HtmlString . tutlayout rglobs attrs

getmbSession :: Request -> IO (Maybe SessionData)
getmbSession = maybe ( return Nothing ) ( query . GetSession ) . getMbSessKey

-- The user argument could be bytestring rather than say, UserName, to keep things generic
-- This way we could have different types of users, say UserName users and AdminUserName users.
-- But for now, keep UserName.
startsess' :: (MonadIO m) => UserName -> String -> ServerPartT m Response
startsess' user landingpage = do  
  let sd = SessionData user
  key <- update $ NewSession sd
  addCookie 3600 (mkCookie "sid" (show key)) 
  seeOther landingpage (toResponse "")

getLoggedInUserInfos :: RenderGlobals -> ErrorT String (ServerPartT IO) (UserName,UserInfos)
getLoggedInUserInfos (RenderGlobals _ _ Nothing) = fail "getLoggedInUserInfos, not logged in"
getLoggedInUserInfos (RenderGlobals _ _ (Just (SessionData uN))) = do
  loggedInUserInfos <- ErrorT . return .
                               maybe (Left $ "bad user" ++ (B.unpack . unusername $ uN)) Right
                                 =<< (query $ GetUserInfos uN)
  return (uN,loggedInUserInfos)


getMbSessKey :: Request -> Maybe SessionKey
getMbSessKey rq = runReaderT (readCookieValue "sid") (rqInputs rq,rqCookies rq)

updateuser :: (Monad m) => t -> b -> m b
updateuser _ newuser = do
  undefined
  return newuser


logoutPage :: RenderGlobals -> ServerPartT IO Response
logoutPage rglobs@(RenderGlobals origRq ts _) = do
  rq <- askRq
  newRGlobs <- maybe
               ( return rglobs )
               (\sk -> do 
                  update . DelSession $ sk
                  return (RenderGlobals origRq ts Nothing))
               (getMbSessKey rq)
  return . tutlayoutU newRGlobs [] $ "home"

avatarimage :: UserName -> IO String
avatarimage un = do
  uap <- urlavatarpath un
  return $ simpleImage (uap,(B.unpack . unusername $ un) ++ " image") ("100","100")
  where 
    urlavatarpath n = do
      let p = writeavatarpath n
      e <- doesFileExist p
      if e
        then return $ '/' : p
        else return "/static/defaultprofileimage.png"

writeavatarpath :: UserName -> String
writeavatarpath un = "userdata/" ++ ( B.unpack . unusername $ un) ++ "/" ++ "profileimage"

errW :: RenderGlobals -> String -> Response
errW rglobs msg = tutlayoutU rglobs [("errormsg", msg)] "errortemplate"
        
requireLogin :: (UserName -> RenderGlobals -> ServerPartT IO Response) 
             -> RenderGlobals -> ServerPartT IO Response
requireLogin f rglobs = do
  case mbUser rglobs of
    Nothing -> return $ errW rglobs "Not logged in"
    Just u -> f u rglobs