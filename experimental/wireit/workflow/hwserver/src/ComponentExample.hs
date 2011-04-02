{-# LANGUAGE TemplateHaskell, DeriveDataTypeable,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  TypeSynonymInstances, TypeOperators #-}

module Main where

{- The following is a fairly simple example using
   multiple components.  It essentially follows the same structure
   as the previous example with the exception that the final
   state, State3, doesn't actually contain any data but has the
   other two states as a part of its dependency.  This means
   we can create Update and Query functions against State1 and
   State2 and use them both if we make our proxy have type
   State3.

   This allows you to make orthogonal components into separate,
   potentially reusable, modules.
-}

import Happstack.State
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader

data State1 = State1 Int
    deriving (Typeable,Show)
instance Version State1
$(deriveSerialize ''State1)

getState1 :: Query State1 Int
getState1 = do
  State1 n <- ask
  return n

setState1 :: Int -> Update State1 ()
setState1 = put . State1

$(mkMethods ''State1 ['getState1,'setState1])

instance Component State1 where
    type Dependencies State1 = End
    initialValue = State1 0

data State2 = State2 String
    deriving (Typeable,Show)
instance Version State2
$(deriveSerialize ''State2)

getState2 :: Query State2 String
getState2 = do
  State2 s <- ask
  return s

setState2 :: String -> Update State2 ()
setState2 = put . State2

$(mkMethods ''State2 ['getState2,'setState2])

instance Component State2 where
    type Dependencies State2 = End
    initialValue = State2 ""

data State3 = State3
    deriving (Typeable,Show)
instance Version State3
$(deriveSerialize ''State3)

instance Component State3 where
    type Dependencies State3 = State1 :+: State2 :+: End
    initialValue = State3
-- Wait, I haven't defined any methods, so why do I need to call
-- mkMethods?  mkMethods actually does a good bit of boilerplate
-- that you need even if you haven't defined any Update or Query
-- methods for your component
$(mkMethods ''State3 []) 

main :: IO ()
main = do
  startSystemState (Proxy :: Proxy State3)
  s1 <- query GetState1
  print s1
  s2 <- query GetState2
  print s2
  update $ SetState1 10
  update $ SetState2 "TEST"