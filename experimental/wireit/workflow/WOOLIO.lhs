>   {-# OPTIONS_GHC -fglasgow-exts #-}

(This is literate Haskell code; suffix file with .lhs)

This WOOLIO library is not required for the WOOL engine. It is a supplemental
library of common functions that assume the IO monad. Pure (non-IO)
WOOL programs should NOT use this library.


>   module WOOLIO where

>   import WOOLTypes
>   import WOOLOperators
>   import Control.Monad.State
>   import List
>   import IO
>   import Data.Char
>   import System.Directory

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

>   makeDirListWithSuffix :: (Monad m,MonadIO m) => String -> Tag -> Tag -> (WorkflowT m OperatorID)
>   makeDirListWithSuffix suffix intag outtag =
>       let
>           fdir _ ( [ToToken d] ) =
>               let
>                   dirname = toString d
>               in do
>                   liftIO $ putStrLn $ "fdir: " ++ dirname
>                   files <- liftIO $ listFilesWithSuffix dirname suffix
>                   return $ [toListToken (makeStringTokenList files)]
>
>           fdir _ _ = return []
>
>       in do
>           makeFunctionM   fdir    [intag]       [outtag]


The FileRead activity takes a filename and returns a pair containing
the filename and the contents of the file.

WARNING: Behavior is unspecified right now when the file cannot be opened.
I don't know what will happen.

>   makeFileRead :: (Monad m,MonadIO m) => Tag -> Tag -> (WorkflowT m OperatorID)
>   makeFileRead intag outtag =
>       let
>           fread _ ( [ ToToken f ] ) =
>               let
>                   filename = toString f
>               in do
>                   liftIO $ putStrLn $ "fread: " ++ filename
>                   handle <- liftIO $      openFile filename ReadMode
>                   contents <- liftIO $    hGetContents handle
>                   let result = [ toPairToken ( toStringToken filename, toStringToken contents ) ]
>                   -- liftIO $ putStrLn $ "result: " ++ (show result) ++ "\n"
>                   return result
>
>           fread _ _ = return []
>
>       in do
>           makeFunctionM   fread       [intag]       [outtag]


>   makeFileWrite :: (Monad m,MonadIO m) => (Maybe String) -> (Maybe String) -> Tag -> Tag -> (WorkflowT m OperatorID)
>   makeFileWrite mprefix msuffix intag  outtag =
>       let
>           fwrite :: (Monad m,MonadIO m) => OperatorID -> [Token] -> WorkflowT m [Token]
>           fwrite _ ( [ nameAndPage ] ) =
>               let
>                   ( f, p ) = toPair nameAndPage
>                   filename = toString f
>                   page = toString p
>                   getm Nothing = ""
>                   getm (Just s) = s
>               in do
>                   let newname = (getm mprefix) ++ filename ++ (getm msuffix)
>                   liftIO $                putStrLn $ "fwrite[" ++ newname ++ "] " ++ (show (length page)) ++ " bytes"
>                   handle <- liftIO $      openFile newname WriteMode
>                   liftIO $    hPutStr handle $ page
>                   liftIO $ hClose handle
>                   return $ [toStringToken newname]
>
>           fwrite _ _ = return []
>       in do
>           makeFunctionM   fwrite       [intag]       [outtag]



