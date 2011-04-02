{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, NoMonomorphismRestriction, ScopedTypeVariables,
    TypeFamilies, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
module StateVersions.AppState1 {- (
  module SerializeableUserInfos,
  Users (..), UserName -- , add_user_job
  
) -} where

import Happstack.State
import Data.Generics
import Control.Monad (liftM)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify,put,get, MonadState)
import Data.Maybe
import Data.List

import qualified MiscMap as M
import qualified Data.ByteString.Char8 as B

import Misc

t :: String
t = let f (JobName (j :: B.ByteString)) = B.unpack j in f . JobName $ B.pack "job"

-- It might be a bit of overkill to declare things with this level of specificity
-- but I think it'll make the type signatures easier to read later on.
newtype JobName = JobName { unjobname :: B.ByteString }
  deriving (Show,Read,Ord, Eq, Typeable,Data)

instance Version JobName
$(deriveSerialize ''JobName) 

data Job = Job {jobbudget :: B.ByteString -- we allow jobs with unspecified budgets
                , jobblurb :: B.ByteString}
  deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version Job
$(deriveSerialize ''Job) 

{-
  For convenience we define a set of mutator functions for the various fields of our
  data types.  It pays off at the end of the day when writing our Updates.

  mod_field takes a mutator function
  set_field takes a value
-}
set_jobbudget :: B.ByteString -> Job -> Job
set_jobbudget = mod_jobbudget . const

mod_jobbudget :: (B.ByteString -> B.ByteString) -> Job -> Job
mod_jobbudget f j@(Job b _) = j{jobbudget=f b}

set_jobblurb :: B.ByteString -> Job -> Job
set_jobblurb = mod_jobblurb . const

mod_jobblurb :: (B.ByteString -> B.ByteString) -> Job -> Job 
mod_jobblurb f j@(Job _ b) = j{jobblurb=f b}

newtype Jobs = Jobs { unjobs :: M.Map JobName Job }
  deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version Jobs
$(deriveSerialize ''Jobs) 

data UserProfile = UserProfile {
  contact :: B.ByteString -- eg, "thomashartman1 at gmail, 917 915 9941"
  -- tell something about yourself. Edited via a text area. should replace newlines with <br> when displayed.
  , blurb :: B.ByteString
  , consultant :: Bool -- this is what actually determines whether the profile will list as a consultant or not
  , avatar :: B.ByteString -- path to an image file
} deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version UserProfile
$(deriveSerialize ''UserProfile) 

set_contact :: B.ByteString -> UserProfile -> UserProfile
set_contact = mod_contact . const

mod_contact :: (B.ByteString -> B.ByteString) -> UserProfile -> UserProfile
mod_contact f u@(UserProfile c _ _ _) = u{contact=f c}

set_blurb :: B.ByteString -> UserProfile -> UserProfile
set_blurb = mod_blurb . const

mod_blurb :: (B.ByteString -> B.ByteString) -> UserProfile -> UserProfile
mod_blurb f u@(UserProfile _ b _ _) = u{blurb=f b}

set_consultant :: Bool -> UserProfile -> UserProfile
set_consultant = mod_consultant . const

mod_consultant :: (Bool -> Bool) -> UserProfile -> UserProfile
mod_consultant f u@(UserProfile _ _ c _) = u{consultant=f c}

data UserInfos = UserInfos {   
  password :: B.ByteString
  , userprofile :: UserProfile
  , jobs :: Jobs
} deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version UserInfos
$(deriveSerialize ''UserInfos) 

set_userprofile :: UserProfile -> UserInfos -> UserInfos
set_userprofile = mod_userprofile . const

mod_userprofile :: (UserProfile -> UserProfile) -> UserInfos -> UserInfos
mod_userprofile f u@(UserInfos _ up _) = u{userprofile=f up}

add_job :: (Monad m) => JobName -> Job -> UserInfos -> m UserInfos
add_job jobname = mod_jobs . M.insertUqM jobname

del_job :: (Monad m) => JobName -> UserInfos -> m UserInfos
del_job = mod_jobs . M.deleteM

set_job :: (Monad m) => Job -> JobName -> UserInfos -> m UserInfos
set_job = mod_job . const

mod_job :: (Monad m) => (Job -> Job) -> JobName -> UserInfos -> m UserInfos
mod_job f jobname = mod_jobs $ M.adjustM jobname f 

mod_jobs :: (Monad m) => (M.Map JobName Job -> Either String (M.Map JobName Job)) 
                      -> UserInfos -> m UserInfos
mod_jobs mf (UserInfos pass up (Jobs j) ) = either (fail . ("mod_jobs: " ++) )
                                                      (\js -> return $ UserInfos pass up (Jobs js) )
                                                      (mf j)



newtype UserName = UserName { unusername :: B.ByteString }
  deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version UserName
$(deriveSerialize ''UserName)

data Users = Users { users :: M.Map UserName UserInfos }
  deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version Users
$(deriveSerialize ''Users)

-- can fail monadically if the username doesn't exist, or the job name is a duplicate
add_user_job :: (Monad m) => UserName -> JobName -> Job -> Users -> m Users
add_user_job un jn = mod_userMM un . add_job jn


-- adjust users, where the adjustment function can fail monadically

mod_userMM :: (Monad m) => UserName -> 
              (UserInfos -> Either String UserInfos) -> Users -> m Users
mod_userMM username f (Users us) = either (fail . ("mod_userMM: " ++) )
                                        (return . Users)
                                        (M.adjustMM username f us)

-- adjust users, where the adjustment function is presumed to be infallible,
-- but can still fail monadically if the username is invalid
mod_userM :: (Monad m) => UserName -> (UserInfos -> UserInfos) -> Users -> m Users
mod_userM username f (Users us) = return . Users =<< M.adjustM username f us

set_user_userprofile_contact::(Monad m)=> UserName -> B.ByteString -> Users -> m Users
set_user_userprofile_contact username = mod_userM username . mod_userprofile . set_contact

set_user_userprofile_blurb ::(Monad m) => UserName -> B.ByteString -> Users -> m Users
set_user_userprofile_blurb username = mod_userM username . mod_userprofile . set_blurb

set_user_userprofile_consultant :: (Monad m) => UserName -> Bool -> Users -> m Users
set_user_userprofile_consultant username = mod_userM username . mod_userprofile . set_consultant

add_user :: (Monad m) => UserName -> B.ByteString -> Users -> m Users
add_user username hashedpass (Users us)
    | B.null . unusername $ username = fail "blank username"
    | B.null hashedpass = fail "error: blank password"
    | not . isalphanum_S . B.unpack . unusername $ username
        = fail $ "bad username, " ++ allowedCharactersSnip
    | otherwise = either (fail . ("add_user: " ++))
                         (return . Users)
                         ( M.insertUqM username uis us )
  where uis = UserInfos hashedpass (UserProfile (B.pack "") (B.pack "")
                                                False (B.pack "") )
                                   (Jobs M.empty)

del_user :: (Monad m) => UserName -> t -> Users -> m Users
del_user username _ (Users us) = either (fail . ("del_user: " ++))
                                        (return . Users)
                                        ( M.deleteM username us )

type SessionKey = Integer

newtype SessionData = SessionData {
  sesUser :: UserName
} deriving (Read,Show,Eq,Typeable,Data,Ord)
instance Version SessionData 
$(deriveSerialize ''SessionData)


data Sessions a = Sessions {unsession::M.Map SessionKey a}
  deriving (Read,Show,Eq,Typeable,Data)
instance Version (Sessions a)
$(deriveSerialize ''Sessions)


-- Think of appdatastore as the database in a traditional web app.
-- Data there gets stored permanently
-- Data in appsessions is stored permanently too, but we don't care as much about its persistence,
-- it's just to keep track of who is logged in at a point in time.
-- appsessions field could be less complicated, just have M.Map Int SessionData
-- don't really see the advantage of declaring a wrapper over map.

-- to do: appdatastore should be :: Map UserName User
-- User :: Password ConsultantProfile Jobs
-- Jobs :: Map JobName Job
-- Job :: JobBudget JobBlurb
-- thereafter.......... 


data AppState = AppState {
  appsessions :: Sessions SessionData,  
  appdatastore :: Users
} deriving (Show,Read,Typeable,Data)           

instance Version AppState

$(deriveSerialize ''AppState) 

instance Component AppState where 
  type Dependencies AppState = End 
  initialValue = AppState { appsessions = Sessions M.empty,
                            appdatastore = Users M.empty }


askDatastore :: Query AppState Users
askDatastore = fmap appdatastore ask

askSessions :: Query AppState (Sessions SessionData)
askSessions = fmap appsessions ask

setUserProfile :: UserName -> UserProfile -> Update AppState ()
setUserProfile uname = modUserInfos uname . set_userprofile

addJob :: UserName -> JobName -> Job -> Update AppState (Either String ())
addJob uname jn = modUserInfosM uname . add_job jn

delJob :: UserName -> JobName -> Update AppState (Either String ())
delJob uname = modUserInfosM uname . del_job 

setJob :: UserName -> Job -> JobName -> Update AppState (Either String ())
setJob uname j = modUserInfosM uname . set_job j

modUserInfosM :: UserName -> (UserInfos -> Either String UserInfos) -> Update AppState (Either String ())
modUserInfosM un mf = do
  (AppState sessions (Users us)) <- get
  case M.adjustMM un mf us of
    Left err -> return . Left $ err
    Right um -> do put $ AppState sessions (Users um)
                   return . Right $ ()

modUserInfos :: UserName -> ( UserInfos -> UserInfos ) -> Update AppState ()
modUserInfos un f = do 
  (AppState sessions (Users us)) <- get
  case M.adjustM un f us of
    Left err -> fail err
    Right um -> put $ AppState sessions (Users um)


modSessions :: (Sessions SessionData -> Sessions SessionData) -> Update AppState ()
modSessions f = modify (\s -> (AppState (f $ appsessions s) (appdatastore s)))

-- yecchh.
-- the way setmap is being used seems kludgy
-- should probably either be using HAppS IndexSet, or a Map instead of Set.

isUser :: UserName -> Query AppState Bool
isUser name = do
  (Users us ) <- askDatastore
  return (isJust $ M.lookup name us)

addUser :: UserName -> B.ByteString -> Update AppState (Either String ())
addUser un hashedpass = do
  AppState s us <- get
  case ( add_user un hashedpass us :: Either String Users) of
    Left err -> if isInfixOf "duplicate key" err
                  then return . Left $ "username taken"
                  else return . Left $ "error: " ++ err
    Right newus -> do put $ AppState s newus
                      return $ Right ()


changePassword :: UserName -> B.ByteString -> Update AppState ()
changePassword un newpass = do
  AppState s us <- get
  let hashednewpass = scramblepass $ B.unpack newpass
  newUs <- set_user_password un (B.pack hashednewpass) us
  put $ AppState s newUs

set_user_password :: (Monad m) => UserName -> B.ByteString -> Users -> m Users
set_user_password username = mod_userM username . set_password

set_password :: B.ByteString -> UserInfos -> UserInfos
set_password newpass u = u{password=newpass}


-- was getUser
getUserInfos :: UserName -> Query AppState (Maybe UserInfos)
getUserInfos u = ( return . M.lookup u . users ) =<< askDatastore

getUserProfile :: UserName -> Query AppState (Maybe UserProfile)
getUserProfile u = do
  mbUI <- getUserInfos u
  case mbUI of 
    Nothing -> return Nothing
    Just (UserInfos _ profile _) -> return $ Just profile

-- list all jobs along with the username who posted each job
listAllJobs :: Query AppState [(JobName, Job, UserName)]
listAllJobs = fmap (
                  concat . M.elems
                    . M.mapWithKey g                 
                       . M.map (unjobs . jobs) . users) 
                           askDatastore 
  where g uname = map ( \(jobname,job) -> (jobname,job,uname) ) . M.toList


listUsers :: Query AppState [UserName]
listUsers = fmap (M.keys . users) askDatastore

listUsersWantingDevelopers :: Query AppState [UserName]
listUsersWantingDevelopers =  fmap (M.keys . M.filter wantingDeveloper . users) askDatastore
  where wantingDeveloper = not . M.null . unjobs . jobs

newSession :: SessionData -> Update AppState SessionKey
newSession u = do  
  AppState (Sessions ss) us <- get
  (newss,k) <- inssess u ss  
  -- check that random session key is really unique
  put $ AppState (Sessions newss) us 
  return k
  where
    inssess u' sessions = do
      key <- getRandom
      case (M.insertUqM key u' sessions) of
        Nothing -> inssess u' sessions
        Just m -> return (m,key)

delSession :: SessionKey -> Update AppState ()
delSession sk = modSessions $ Sessions . M.delete sk . unsession

getSession::SessionKey -> Query AppState (Maybe SessionData)
getSession key = fmap (M.lookup key . unsession) askSessions

numSessions :: Query AppState Int
numSessions  =  fmap (M.size . unsession) askSessions


initializeDummyData :: M.Map UserName UserInfos -> Update AppState ()
initializeDummyData dd = do
  AppState ss (Users us) <- get
  if M.null us 
    then fail "initializeDummyData, users not empty"
    else put $ AppState ss (Users dd)

-- bad performance for large unumbers of users (>1000, with 200 jobs/dummy user)
-- maybe macid doesn't like serializing large quantities of data at once

addDummyData :: M.Map UserName UserInfos -> Update AppState ()
addDummyData dd = do
  AppState ss (Users us) <- get
  put $ AppState ss (Users (M.union us dd) )

addDummyUser :: (UserName, UserInfos) -> Update AppState ()
addDummyUser (un,uis) = do
  AppState ss (Users us) <- get
  us' <- M.insertUqM un uis us
  put $ AppState ss (Users us' )


-- define types which are upper case of methods below, eg AddUser, AuthUser...
-- these types work with HApppS query/update machinery
-- in ghci, try :i AddUser
$(mkMethods ''AppState
    ['askDatastore
     , 'getUserInfos
     , 'getUserProfile
     , 'addUser
     , 'changePassword 
     , 'setUserProfile
     , 'isUser
     , 'listUsers     
     , 'listAllJobs
     , 'getSession
     , 'newSession
     , 'delSession
     , 'numSessions
     , 'initializeDummyData
     , 'addDummyData
     , 'addDummyUser
     , 'addJob
     , 'delJob
     , 'setJob ]
 )



