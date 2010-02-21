--
-- wrapper activity : intended to allow programs outside haskell to be
-- used as workflow activities.
-- 
-- matt@cs.uoregon.edu
--
import System (getArgs)
import System.IO
import System.Process
import Data.List (find)
import System.IO.Unsafe (unsafePerformIO)

hForceGetContents :: Handle -> IO String
hForceGetContents h = do
  nil <- openFile "/dev/null" WriteMode
  raw <- hGetContents h
  hPutStr nil raw
  hClose nil
  return raw

type ExtProcess = String
type Hash = String
type Token = (FilePath,Hash)
data MemoEntry = MemoEntry {procName :: ExtProcess
                           ,inputToken :: Token 
                           ,outputToken :: Token
                           }deriving (Eq, Read, Show)
type MemoTable = [MemoEntry]

loadMemoTable :: FilePath -> IO MemoTable
loadMemoTable path = do
  h <- openFile path ReadMode
  raw <- hForceGetContents h
  let hpairs = map (read :: String -> MemoEntry) (lines raw)
  hClose h
  return hpairs

saveMemoTable :: MemoTable -> FilePath -> IO ()
saveMemoTable tbl path = do
  h <- openFile path WriteMode
  let raw = unlines (map show tbl)
  hPutStr h raw
  hClose h
  return ()
  
addMemoEntry :: MemoEntry -> MemoTable -> MemoTable
addMemoEntry = (:)

lookupMemoEntry :: (ExtProcess, Token) -> MemoTable -> Maybe Token
lookupMemoEntry key tbl = 
  case find (match key) tbl of
    Just entry -> Just (outputToken entry)
    Nothing -> Nothing
  where match (p,t) entry = 
          (p == (procName entry) && t == (inputToken entry))

readToEof h = 
  do checkEOF <- hIsEOF h
     (if checkEOF
      then return []
      else do line <- hGetLine h
              lines <- readToEof h
              return $ line:lines)

stringToToken :: String -> (String,String)
stringToToken s =
  (fpath,md5)
  where
    tok = tail (dropWhile (/= '=') s)
    fpath = takeWhile (/= ',') tok
    md5 = drop 2 (dropWhile (/= ',') tok)

doCall :: ExtProcess -> FilePath -> FilePath -> FilePath -> IO Token
doCall p wrapPath dataRepo fn = do
  (Just stdIn, Just stdOut, Just stdErr, h) <- 
    createProcess (proc (wrapPath ++ "/wrapper.pl") 
                        [wrapPath ++ "/" ++ p, dataRepo])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hSetBuffering stdOut LineBuffering
  hSetBuffering stdIn LineBuffering
  hPutStrLn stdIn ("f:" ++ fn)
  hPutStrLn stdIn ""
  output <- hGetLine stdOut
  hClose stdIn
  hClose stdOut
  hClose stdErr
  terminateProcess h
  return $ stringToToken output

invokeExternal :: ExtProcess -> FilePath -> FilePath -> Token -> IO Token
invokeExternal p wrapPath dataRepo tok =
  loadMemoTable db >>= \tbl ->
  case lookupMemoEntry (p,tok) tbl of
    Just out -> do
      putStrLn "Found"
      return out
    Nothing -> do 
      putStrLn "Not found"
      out <- doCall p wrapPath dataRepo (fst tok)
      let entry = MemoEntry {procName=p,
                             inputToken=tok,
                             outputToken=out}
      let newTbl = addMemoEntry entry tbl
      saveMemoTable newTbl db
      return out
  where db = dataRepo ++ "/db.txt"
  
extAct :: ExtProcess -> FilePath -> FilePath -> (Token -> Token)
extAct p wrapPath dataRepo = 
  \t -> unsafePerformIO (invokeExternal p wrapPath dataRepo t)

{-
main :: IO ()
main = do
  args <- getArgs
  wrapperDir <- return $ head args
  dataRepo <- return $ head (tail args)
  x1 <- invokeExternal "sample1" wrapperDir dataRepo (dataRepo ++ "/test","0")
  -- x2 <- invokeExternal "p1" ("a","b")
  -- x3 <- invokeExternal "p2" ("a","b")
  putStrLn (show x1)
  x2 <- invokeExternal "sample2" wrapperDir dataRepo x1
  putStrLn (show x2)
-}
