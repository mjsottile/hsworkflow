module Main where

import Happstack.Server
import Control.Monad.State

-- flip evalStateT 0 has a type of StateT Int IO a -> IO () a
-- and is the suitable transformer to be passed as the first parameter to
-- simpleHTTP' 
main :: IO ()
main = do
  putStrLn "Now listening on port 8880"
  simpleHTTP' (flip evalStateT 0) (Conf 8880 Nothing) handler

handler :: ServerPartT (StateT Int IO) String
handler = do
  -- We can use the MonadTrans instance of ServerPartT in order to lift a StateT computation
  -- into the ServerPartT monad
  i <- lift $ do
                    liftIO $ putStrLn $ "blah"
                    modify (+1)
                    get
  return . show $ i  