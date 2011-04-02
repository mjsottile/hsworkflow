module FromDataInstances where

import Happstack.Server
import StateVersions.AppState1
import Control.Monad.Reader
-- import Safe
import qualified Data.ByteString.Char8 as B
import Misc
import Happstack.Helpers

-- This could be a lot less verbose, and use shorter variable names,
-- but it's the tutorial instructional example for using FromData to deal with forms, so no harm.
data PaginationUrlData = PaginationUrlData { pCurrBar :: Int
                                             , pResultsPerBar :: Int
                                             , pCurrPage :: Int
                                             , pResultsPerPage :: Int }
instance FromData PaginationUrlData where
    fromData =
        let readerCurrbar, readerResultsPerBar, readerCurrpage,readerResultsPerPage
                :: ReaderT ([(String, Input)], [(String, Cookie)]) Maybe Int
            readerCurrpage = safeRead -- convert string to int
                               =<< look "currentpage" `mplus` return "1" -- get string from urlencoded get string
            readerResultsPerPage =  do n <- look "resultsPerPage" `mplus` return "200" 
                                       safeRead n
            readerCurrbar = safeRead =<< look "currentbar" `mplus` return "1"
            readerResultsPerBar = safeRead =<< look "resultsPerBar" `mplus` return "10000"

            readerPaginationUrlData :: ReaderT ([(String,Input)], [(String,Cookie)]) Maybe PaginationUrlData
            readerPaginationUrlData =
              liftM4 PaginationUrlData readerCurrbar readerResultsPerBar readerCurrpage readerResultsPerPage
        in  readerPaginationUrlData    


data UserNameUrlString = UserNameUrlString {profilename :: UserName}
instance FromData UserNameUrlString where
    fromData = liftM UserNameUrlString ( return . UserName =<< return . B.pack =<< look "user" `mplus` return "")

data UserAuthInfo = UserAuthInfo UserName B.ByteString
instance FromData UserAuthInfo where
    fromData = liftM2 UserAuthInfo ( do un <- (look "username" `mplus` return "") ; return . UserName . B.pack $ un)
                                   (return . B.pack =<< look "password" `mplus` return "")

data ChangePasswordInfo = ChangePasswordInfo B.ByteString B.ByteString
instance FromData ChangePasswordInfo where
    fromData = liftM2 ChangePasswordInfo -- ( return . B.pack =<< (look "oldpass") `mplus` (return "") )
                                     ( return . B.pack =<< look "password" `mplus` return "")
                                     ( return . B.pack =<< look "password2" `mplus` return "")

data StepInfo = StepInfo B.ByteString
instance FromData StepInfo where
    fromData = liftM StepInfo      ( return . B.pack =<< look "numticks" `mplus` return "")


-- wrapper over UserProfile data type, the difference is that the avatar is file contents
data EditUserProfileFormData = EditUserProfileFormData 
  B.ByteString -- contact, eg, "thomashartman1 at gmail, 917 915 9941"
  B.ByteString -- blurb
  Bool -- isConsultant, Bool 
  B.ByteString -- avatarcontents, filename

instance FromData EditUserProfileFormData where
    fromData = liftM4 EditUserProfileFormData
                                  ( return . B.pack =<< look "contact" `mplus` return "" )
                                  ( return . B.pack =<< look "consultantblurb" `mplus` return "")
                                  ( return True )
                                  --( do {x <- (readcheckbox "listasconsultant");return x } )
                                  --( readcheckbox "listasconsultant")  
                                  ( return . B.pack =<< look "imagecontents" `mplus` return "") 

data NewUserInfo = NewUserInfo UserName B.ByteString B.ByteString
instance FromData NewUserInfo where
    fromData = liftM3 NewUserInfo (do un <- look "username" `mplus` return ""; return (UserName . B.pack $ un) )
                                  (return . B.pack =<< look "password" `mplus` return "")
                                  (return . B.pack =<< look "password2" `mplus` return "")
