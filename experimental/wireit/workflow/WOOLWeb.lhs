>   {-# OPTIONS_GHC -fglasgow-exts #-}

(This is literate Haskell code; suffix file with .lhs)

This WOOLIO library is not required for the WOOL engine. It is a supplemental
library of common functions that assume the IO monad. Pure (non-IO)
WOOL programs should NOT use this library.

>   module WOOLWeb where
>   import WOOL
>   import Network.HTTP;     
>   import Control.Concurrent
>   import Control.Monad.State
>   import Network.HTTP;
>   import List
>   import IO
>   import Data.Char
>   import System.Directory
>   import qualified Text.HTML.TagSoup as TS

>   getpageDelay :: Int
>   getpageDelay = 50000    -- 50000==0.05sec

makeURLFetch consumes a StringToken containing a url. This activity will
act as a Factory (fgetpagefactory), constructing a separate WOOL Thread to
manage the fetching concurrently with other WOOL operations. This separate
thread (fgetpage) will then construct an MVar and an associated IO Thread
to perform the fetch asynchronously (async) with respect to the WOOL engine.

The WOOL engine will synchronously check with the managing thread, which
will consult its MVar using tryReadMVar (not blocking). This is where the
asynchronous world of the IO monad meets the synchronous world of WOOL.

There are currently two versions of makeURLFetch.

makeURLFetch takes a url and returns content.
makeURLFetch2 takes two arguments, (context,url), and returns (context,content),
where the context is unchanged.

(This general pattern is likely to be repeated in most workflows. WOOL is
requiring that the user explicitly pass through context, for subsequent use
later in the workflow. This is handled in Orc by using the nested scoping
provided by the >x> operator. I think that WOOL needs to have a proper way
to manage bound variables from 'prior' activities.
Possible ways to do this:
- ALL activities have a Context object available which contains an ordered
  list of Frames, each of which contains the bound variables for a
  preceding activity. The slots in these frames are accessible with some syntax.
  These act like dynamically scoped variables.
- We give the programmer of WOOL workflows a convenient way to pack,
  transport, and unpack useful context variables so that they are available
  'downstream'.

>   makeURLFetch :: (Monad m,MonadIO m) => Tag -> Tag -> (WorkflowT m OperatorID)
>   makeURLFetch intag outtag =
>       let
>           fgetpagefactory _ [ u ] =
>               let
>                   async =
>                       let
>                           url = toString u
>                       in do
>                           rsp <- liftIO $ Network.HTTP.simpleHTTP ( getRequest url )
>                           content <- liftIO $ getResponseBody rsp
>                           putStrLn $ "async done: " ++ url
>                           return $ toStringToken content
>               
>                   fgetpage :: (Monad m,MonadIO m) => (MVar Token) -> OperatorID -> [Token] -> WorkflowT m [Token]
>                   fgetpage ref opId ( [] ) =
>                           do
>                               x <- liftIO $ tryTakeMVar ref
>                               case x of
>                                       Nothing ->  do
>                                                       -- liftIO $    putStrLn $ "fgetpage WAITING: " ++ url
>                                                       liftIO $    threadDelay getpageDelay
>                                                       return [toMissingToken]
>                                       Just result -> do
>                                                       -- liftIO $    putStrLn $ "fgetpage FOUND: " ++ url
>                                                       uninstallOperator opId
>                                                       return [result]
>
>                   fgetpage _ _ _ = return []
>
>               in
>                   if isString u then do
>                       let url = toString u
>                       liftIO $                putStrLn $ "fgetpagefactory: " ++ url
>                       ref <- liftIO $ newEmptyMVar
>                       liftIO $ forkIO $ ( async >>= putMVar ref )
>                       makeFunctionM   (fgetpage ref)    []  [outtag]
>                       return [toMissingToken]
>                   else do
>                       return [toMissingToken]
>
>           fgetpagefactory _ _ = return []
>
>       in do
>           makeFunctionM   fgetpagefactory    [intag]       ["GROUND"]



>   makeURLFetch2 :: (Monad m,MonadIO m) => Tag -> Tag -> (WorkflowT m OperatorID)
>   makeURLFetch2 intag outtag =
>       let
>           fgetpagefactory _ ( [ pairtoken ] ) =
>               let
>                   ( context, urld ) = toPair pairtoken
>                   url = toString urld
>               
>                   async = do
>                       rsp <- liftIO $ Network.HTTP.simpleHTTP ( getRequest url )
>                       content <- liftIO $ getResponseBody rsp
>                       putStrLn $ "RECEIVED URL: " ++ url
>                       return $ toStringToken ( content )
>               
>                   fgetpage :: (Monad m,MonadIO m) => (MVar Token) -> OperatorID -> [Token] -> WorkflowT m [Token]
>                   fgetpage ref opId ( [] ) =
>                       do
>                           x <- liftIO $ tryTakeMVar ref
>                           case x of
>                                   Nothing ->  do
>                                                   -- liftIO $    putStrLn $ "fgetpage WAITING: " ++ url
>                                                   liftIO $    threadDelay getpageDelay
>                                                   return [toMissingToken]
>                                   Just (result) -> do
>                                                   uninstallOperator opId
>                                                   return $ [ toPairToken ( context, result ) ]
>
>                   fgetpage _ _ _ = return []
>
>               in do
>                   liftIO $                putStrLn $ "fgetpagefactory: " ++ url
>                   ref <- liftIO $ newEmptyMVar
>                   liftIO $ forkIO $ ( async >>= putMVar ref )
>                   makeFunctionM   (fgetpage ref)    []  [outtag]
>                   return [toMissingToken]
>
>           fgetpagefactory _ _ = return []
>
>       in do
>           makeFunctionM   fgetpagefactory    [intag]       ["GROUND"]


>   listFilesWithSuffix :: FilePath -> String -> IO [FilePath]
>   listFilesWithSuffix path suffix =
>       let
>           endswith :: Eq a => [a] -> [a] -> Bool
>           endswith a b = (a `isSuffixOf` b)
>           joinFN :: String -> String -> FilePath
>           joinFN p1 p2 = p1 ++ "/" ++ p2
>       in do
>           files <- getDirectoryContents path
>           files1 <- return $ filter (`notElem` [".", ".."]) files
>           files2 <- return $ map (joinFN path) files1
>           files3 <- filterM doesFileExist files2
>           files4 <- return $ filter (endswith suffix) files3
>           return files4
>
>   type Image = String
>
>   getImages :: String -> [Image]
>   getImages = create . filterTags . TS.parseTags
>     where 
>       filterTags :: [TS.Tag] -> [TS.Tag]
>       filterTags = filter (\x -> TS.isTagOpenName "img" x)
>   
>       create :: [TS.Tag] -> [Image]
>       create (TS.TagOpen "img" attrs : rest) =
>           let
>               getImgSrc [] = ""
>               getImgSrc (("src",src):_) = src
>               getImgSrc (_:as) = getImgSrc as
>               
>           in
>               (getImgSrc attrs) : create rest
>       create (_:rest) = create rest
>       create [] = []               
>   

>   type SName = String
>   type SValue = String
>   
>   data Anchor = Anchor SName SValue
>        deriving Show
>   

>   trimWhiteSpace :: String -> String
>   trimWhiteSpace = dropWhile isSpace . reverse . dropWhile isSpace . reverse
>   
>   printAnchors :: [Anchor] -> IO ()
>   printAnchors [] = return ()
>   printAnchors ((Anchor t a):as) =
>       do
>           putStrLn $ "[" ++ t ++ "] " ++ a ++ "\n"
>           printAnchors as
>   

>   getAnchors :: String -> [Anchor]
>   getAnchors = create . filterTags . TS.parseTags
>     where 
>       filterTags :: [TS.Tag] -> [TS.Tag]
>       filterTags = filter (\x -> TS.isTagOpenName "a" x || TS.isTagText x)
>   
>       create :: [TS.Tag] -> [Anchor]
>       create (TS.TagOpen "a" [("href", name)] : TS.TagText text : rest) = 
>         Anchor name (trimWhiteSpace text) : create rest
>       create (_:rest) = create rest
>       create [] = []               
>   
>
>   makeStringPairList   ::  [Anchor] -> [Token] -> [Token]
>   makeStringPairList [] out = out
>   makeStringPairList ((Anchor t a):as) out =
>       do
>           -- putStrLn $ "[" ++ t ++ "] " ++ a ++ "\n"
>           let pair = toListToken [toStringToken t, toStringToken a]
>           makeStringPairList as (out ++ [pair])
>
>   getTagsFromPage :: String -> [Token]
>   getTagsFromPage page = do
>     let   anchors = getAnchors page
>     let   spl = makeStringPairList anchors []
>     spl 

>   getImageTagsFromPage :: String -> [Token]
>   getImageTagsFromPage page = do
>     let   images = getImages page
>     let   r = makeStringTokenList images
>     r 
>

>   makeGetTags :: (Monad m,MonadIO m) => Tag -> Tag -> (WorkflowT m OperatorID)
>   makeGetTags intag outtag =
>       let
>           fgettags _ ( [ namecontentpair ] ) =
>               let
>                   ( f, c ) = toPair namecontentpair
>                   filename = toString f
>                   content = toString c
>               in do
>                   liftIO $                putStrLn $ "fgettags: " ++ filename
>                   let tags = getImageTagsFromPage content
>                   return $ [ toPairToken ( toStringToken filename, toListToken tags ) ]
>           fgettags _ _ = return []
>       in do
>           makeFunctionM   fgettags       [intag]       [outtag]

>
