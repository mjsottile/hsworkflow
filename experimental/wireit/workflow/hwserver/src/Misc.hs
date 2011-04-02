module Misc where 

import Happstack.Server 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad
import Data.List
import Control.Arrow
import Data.Digest.Pure.MD5
import Text.PrettyPrint as PP
import System.FilePath (pathSeparator, takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Safe (readMay)
import System.Time
import Data.Char (isAlphaNum)
import Data.Char (toLower)

pp :: (Show a) => [t] -> [a] -> String
pp [] = PP.render . vcat . map (text . show)
pp _ = undefined

pathPartsSp pps f = do
  rq <- askRq
  if rqPaths rq == pps
    then f rq 
    else mzero

-- Do something when the request has exactly one path segment left.
-- lastPathPartSp :: (Request -> String -> WebT IO Response) -> ServerPartT IO Response
lastPathPartSp0 f = do
  rq <- askRq
  case rqPaths rq of
            [lastpart] -> f rq lastpart
            _ -> mzero

-- store passwords as md5 hash, as a security measure
scramblepass :: String -> String
scramblepass = show . md5 . L.pack

-- HStringTemplate modifications -- copy/paste/tweak from HStringTemplate.
-- same as directoryGroup, but throws an error for template names with punctuation



tFromTo = fromTo 10 20 [1..1000] == [10..20] 
fromTo fr to = take (to-(fr-1)) . drop (fr-1)


quote x = '\"' : x ++ "\"" 

-- Windows/Unix/Mac compatible 
pathstring pathparts =
  let sep :: String
      sep = [pathSeparator] 
  in intercalate sep pathparts

safeRead :: (Monad m, Read a) => [Char] -> m a
safeRead s = 
  maybe (fail $ "safeRead: " ++ s)
        return 
        (readMay s)

timeSince x = do
  currTime <- getClockTime
  return . diffClockTimes currTime $ x

-- splitList 3 [1..11]
-- [([(1,1),(2,2),(3,3)],(1,3)),([(4,4),(5,5),(6,6)],(4,6)),([(7,7),(8,8),(9,9)],(7,9)),([(10,10),(11,11)],(10,11))]
-- the result is a list of (indexed sublist,(fist index, last index))
splitList :: Int -> [b] -> [([b], (Int, Int))]
splitList n x = let
  part = splitList' n ( zip [1..] x) 
  bounded = map (map snd &&& bounds) part
  bounds [] = (0,0)
  bounds l@((_,_):_) = (fst . head $ l,fst . last $ l)
  in bounded
  where 
    splitList' :: Int -> [a] -> [[a]]
    splitList' _ [] = []
    splitList' n l@(x:xs) =
      let (a,b') = genericSplitAt n l
          b = splitList' n b'
      in  a : b


lc = map toLower

-- write a file, creating parent directories if necessary
writeFileForce fp contents = do
  createDirectoryIfMissing True (takeDirectory fp)
  B.writeFile fp contents

-- check if a string is made of alphanumeric plus _
isalphanum_S = null . filter (not . isAlphaNum_)
isAlphaNum_ c = isAlphaNum c || '_' == c || '-' == c

allowedCharactersSnip = "please use only numbers, letters, '-', and '_'."