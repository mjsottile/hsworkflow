module View where

import Misc
import Data.List
import Happstack.Server.HTTP.Types (Request)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Control.Monad.Reader
import Text.StringTemplate.Helpers

import Data.Maybe
import StateVersions.AppState1

import Happstack.Helpers

-- Notice, there are no Happstack.* imports 
-- Idea is, view is meant to be used from controller.
-- :browse View in ghci should reveal there's no IO for any of these function sigs.

{-
  RenderGlobals is essentially just a cute wrapper around a triple
  meant to pass along the set of templates, the SessionData which is stored
  in a cookie when the user has logged in, and the actual Request made.
-}
data RenderGlobals = RenderGlobals {
                            origrq :: Request
                            , templates :: STDirGroups String
                            , mbSession :: Maybe SessionData
                            }
  deriving Show

mbUser :: RenderGlobals -> Maybe UserName
mbUser = fmap sesUser . mbSession

tutlayout :: RenderGlobals -> [(String, String)] -> String -> String
tutlayout (RenderGlobals rq ts' mbSess) attrs tmpl0 = 
  let ts = getTemplateGroup "." ts'
      tmpl = cleanTemplateName tmpl0
      mbU = fmap sesUser mbSess
      rendertut :: [(String,String)] -> String -> String
      rendertut = renderTemplateGroup ts

      -- should use readM, or whatever it's called, from Data.Safe
      --readtut :: (Monad m, Read a) => String -> m a
      readtut file = (Misc.safeRead . rendertut [] . concatMap escapequote $ file)
        where escapequote char = if char=='"' then "\\\"" else [char]
      readTutTuples :: String -> [(String,String)]
      readTutTuples f = either (const [("readTutTuples error","")]) id
                        (readtut f :: Either String [(String,String)] )
      attrsL = maybe attrs ( \user -> ("loggedInUser",B.unpack . unusername $ user) : attrs ) mbU  

      content = rendertut attrsL tmpl

      header =  rendertut [("menubarMenu",menubarMenu),("userMenu",userMenu),("mainUserMenu",mainUserMenu)] "header"
        where 
          userMenu = maybe 
            ( rendertut attrsL "login" )
            ( \user -> hMenuBars rq
              [("/tutorial/logout","logout " ++ (B.unpack . unusername $ user))
               , ("/tutorial/changepassword","change password")] )
            mbU
          mainUserMenu = if isJust mbU 
                           then hMenuBars rq $ readTutTuples "mainusermenu" 
                           else "" 

          menubarMenu = hMenuBars rq $ readTutTuples "menubarmenu"
      tocArea = "" -- vMenuOL rq $ readTutTuples "toc"
  in rendertut [ ("tocarea",tocArea),
                ("contentarea",content)
                ,("headerarea",header)] "base"

cleanTemplateName :: String -> String
cleanTemplateName = filter isAlpha

paintblurb :: String -> String
paintblurb =  newlinesToHtmlLines

paintProfile :: RenderGlobals -> String -> UserProfile -> String -> String
paintProfile rglobs user cp userimagepath =
  let ts = getTemplateGroup "." . templates $ rglobs
      
      attrs = [("username",user) 
               , ("userimage", userimagepath ) 
               , ("blurb",newlinesToHtmlLines . B.unpack . blurb $ cp)
               , ("contact", newlinesToHtmlLines . B.unpack . contact $ cp)]
  in renderTemplateGroup ts attrs "consultantprofile"

