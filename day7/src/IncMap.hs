module IncMap where

import qualified Data.Map as M
import qualified Data.IntMap as IM

data IncMap a = IncMap {
  _map   :: IM.IntMap a,
  _rMap  :: M.Map a Int,
  _nextI :: Int
}

empty :: Ord a => IncMap a
empty = IncMap mempty mempty 0

insert :: Ord a => a -> IncMap a -> IncMap a
insert x old@(IncMap m rm i) = if M.member x rm
  then old
  else IncMap (IM.insert i x m) (M.insert x i rm) (i+1)

getIndex :: Ord a => a -> IncMap a -> Int
getIndex x (IncMap _ m _) = m M.! x

getValue :: Int -> IncMap a -> a
getValue i (IncMap m _ _) = m IM.! i