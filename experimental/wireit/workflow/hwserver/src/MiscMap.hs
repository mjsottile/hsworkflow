{-# LANGUAGE NoImplicitPrelude #-}
module MiscMap (
  empty
  , Map
  , Data.Map.mapWithKey
  , Data.Map.map
  , Data.Map.toList
  , Data.Map.keys
  , Data.Map.filter
  , Data.Map.null
  , Data.Map.delete -- allow normal deletes that just do nothing if key not found
  , Data.Map.size
  , Data.Map.elems
  , Data.Map.lookup
  , Data.Map.fromList
  , Data.Map.union
  , insertUqM
  , deleteM
  , adjustM      
  , adjustMM
)
where

import Data.Map
import Prelude hiding (lookup)

---- kind of like what's in Happstack.Data.IxSet, but just operate on vanilla haskell maps, don't use IxSet machinery
-- modification operations
-- reject insert if duplicate key
--insertUq :: (Ord a, Ord k1, Ord k2) => k1 -> a -> Map k1 (Map k2 a) -> Map k1 (Map k2 a)
--insertUq k v index = M.insertWith M.union k (Set.singleton v) index
--  lookup

insertUqM :: (Show k, Monad m, Ord k, Ord a) => k -> a -> Map k a -> m (Map k a)
insertUqM k v m =
  case lookup k m of
    Nothing -> return $ insert k v m
    Just _ -> fail $ "insertUqM, duplicate key: " ++ (show k)


deleteM :: (Show k, Monad m, Ord a, Ord k) => k -> Map k a -> m (Map k a)
deleteM k m = case lookup k m of
   Nothing -> fail $ "deleteM, key not found: " ++ (show k)
   Just _ -> return $ delete k m

-- how is update implemented in standard libs? Will this be efficient for macid?

adjustM :: (Ord k, Monad m, Show k) => k -> (a -> a) ->
           Map k a -> m (Map k a)
adjustM k f m = case lookup k m of
  Nothing -> fail $ "updateM, key not found: " ++ (show k)
  Just _ -> return . adjust f k $ m


-- similar to adjustM, except modification function can fail monadically

adjustMM ::(Ord k, Monad m, Show k) => k -> 
           (a -> Either String a) -> Map k a -> m (Map k a)
adjustMM k mf m = case lookup k m of
  Nothing -> fail $ "updateMM, key not found: " ++ (show k)
  Just val -> case mf val of
    Left err -> fail $ "adjustMM: " ++ err
    Right newval -> return . adjust (const newval) k $ m 

