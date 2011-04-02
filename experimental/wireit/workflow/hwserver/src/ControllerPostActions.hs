{-# LANGUAGE ScopedTypeVariables #-}
module ControllerPostActions where

import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as B
import Control.Monad.Error


import Happstack.Server
import Happstack.State
import Happstack.Helpers

import StateVersions.AppState1
import View
import Misc
import ControllerMisc
import FromDataInstances

loginPage :: RenderGlobals -> ServerPartT IO Response
loginPage rglobs@(RenderGlobals rq _ _) = 
  -- unfortunately can't just use rqUrl rq here, because sometimes has port numbers and... it gets complicated
  case tutAppReferrer rq of
    Left e -> return $ errW rglobs e
    Right landingpage -> loginPage' authUser (const startsess') rglobs landingpage 
  where 
    authUser = authUser' getUserPassword
    getUserPassword name = return . maybe Nothing (Just . B.unpack . password) =<< query (GetUserInfos name)

-- move this to HAppSHelpers
tutAppReferrer :: Request -> Either String String
tutAppReferrer rq = do
          let approot :: Either String String
              approot = modRewriteAppUrl "" rq 
          case approot of 
            Left _ -> Left $ "tutAppReferrer, could not determine approot, rq: " ++ (show rq)
            Right ar ->
              case getHeaderVal "referer" rq of 
                  Left e -> Left $ "smartAppReferrer error, rq: " ++ e
                  -- check against logout, otherwise if you have just logged out then 
                  -- try immediately to log in again it won't let you.
                  Right rf ->  if isInfixOf "logout" rf || isInfixOf "login" rf || isInfixOf "newuser" rf
                      then Right ar
                      else Right rf

-- Use a helper function because the plan is to eventually have a similar function
-- that works for admin logins
loginPage' :: (Monad m) =>
              (UserName -> B.ByteString -> ServerPartT m Bool)
              -> (RenderGlobals -> UserName -> String -> ServerPartT m Response)
              -> RenderGlobals
              -> String
              -> ServerPartT m Response
loginPage' auth startsession rglobs landingpage = do
  UserAuthInfo user pass <- getData'
  loginOk <- auth user pass
  if loginOk
   then startsession rglobs user landingpage
   else return $ errW rglobs "Invalid user or password"

-- check if a username and password is valid. If it is, return the user as successful monadic value
-- otherwise fail monadically
authUser' :: (UserName -> ServerPartT IO (Maybe String) ) -> UserName -> B.ByteString -> ServerPartT IO Bool
authUser' getpwd name pass = do
  mbP <- getpwd name
  -- scramblepass works with lazy bytestrings, maybe that's by design. meh, leave it for now
  -- to do: we need to use a seed, there was a discussion about this on haskell cafe.
  return $ maybe False ( == scramblepass (B.unpack pass) ) mbP


changePasswordSP :: RenderGlobals -> ServerPartT IO Response
changePasswordSP rglobs = do 
    ChangePasswordInfo newpass1 newpass2 <- getData'
    if newpass1 /= newpass2 
      then return $ errw "new passwords don't match"
      else do 
        etRes <- runErrorT $ getLoggedInUserInfos rglobs
        case etRes of
          Left e -> return $ errw e
          Right (u,_) ->  do     
            update $ ChangePassword u newpass1
            return $ tutlayoutU rglobs [] "accountsettings-changed"
  where errw msg = tutlayoutU rglobs [("errormsgAccountSettings", msg)] "changepassword"


newUserPage :: RenderGlobals -> ServerPartT IO Response
newUserPage rglobs = do
  NewUserInfo user pass1 pass2 <- getData'
  rq <- askRq
  etRes <- runErrorT $ setupNewUser (NewUserInfo user (pass1 :: B.ByteString) pass2) 
  case etRes of
    Left err -> return $ errW rglobs err 
    Right () -> case modRewriteAppUrl "tutorial/registered" rq of
                  Left e -> return $ errW rglobs e
                  Right p -> startsess' user p
                         
  where
    setupNewUser :: NewUserInfo -> ErrorT String (ServerPartT IO) ()
    setupNewUser (NewUserInfo user pass1 pass2) = do
        when (B.null pass1 || B.null pass2) (throwError "blank password")
        when (pass1 /= pass2) (throwError "passwords don't match")
          --  Q: can return . Left be replaced with throwError?
          --  A: no. But you can return just plain Left with throwError.
        nameTakenHAppSState <- query $ IsUser user
        when nameTakenHAppSState (throwError "name taken")
        addUserVerifiedPass user pass1 pass2  

addUserVerifiedPass :: UserName -> B.ByteString -> B.ByteString -> ErrorT String (ServerPartT IO) ()
addUserVerifiedPass user pass1 pass2 =
  ErrorT $ newuser user pass1 pass2
  where
    newuser :: UserName -> B.ByteString -> B.ByteString -> ServerPartT IO (Either String ())
    newuser u@(UserName _) p1 p2 -- userExists
      | p1 /= p2 = return . Left $  "passwords did not match"
      | otherwise = update $ AddUser u $ B.pack $ scramblepass (B.unpack p1)
