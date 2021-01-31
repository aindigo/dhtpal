{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module NodeId (nodeId, distance) where

import qualified Control.Lens as L
import Control.Monad.Trans.State (State, get, put)
import Data.Bits (FiniteBits (countLeadingZeros), xor)
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map, lookupGE)
import Data.Word (Word8)
import Z.IO.Network (PortNumber, SocketAddr (SocketAddrIPv4), tupleToIPv4Addr)
import Z.IO.Network.SocketAddr (ipv4)

newtype NodeId = NodeId BS.ByteString deriving (Show, Eq, Ord)

data NodeInfo = NodeInfo
  { _sockAddr :: SocketAddr,
    _lastUpdated :: Int
  }
  deriving (Show)

newtype RoutingTable = RT (NodeId, NodeInfo, Map Int [(NodeId, NodeInfo)])

L.makeLenses ''NodeInfo
L.makeLenses ''RoutingTable

type RoutingTableS a = State RoutingTable a

getLocalId :: RoutingTableS NodeId
getLocalId = get >>= (\(RT (nodeId, _, _)) -> return nodeId)

getLocalInfo :: RoutingTableS NodeInfo
getLocalInfo = get >>= (\(RT (_, localInfo, _)) -> return localInfo)

getBuckets :: RoutingTableS (Map Int [(NodeId, NodeInfo)])
getBuckets = get >>= (\(RT (_, _, buckets)) -> return buckets)

getFreeBucket :: Int -> RoutingTableS (Maybe [(NodeId, NodeInfo)])
getFreeBucket distance = do
  buckets <- getBuckets
  let bucket = lookupGE distance buckets
   in case bucket of
        Just (key, b) ->
          if length bucket < 8
            then return (Just b)
            else return Nothing
        Nothing -> return Nothing

nodeId :: BS.ByteString -> Either String NodeId
nodeId bs =
  if BS.length bs == 20
    then Right (NodeId bs)
    else Left "wrong length"

node :: NodeId -> (Word8, Word8, Word8, Word8) -> PortNumber -> (NodeId, NodeInfo)
node ni tpl port = (ni, NodeInfo (SocketAddrIPv4 (tupleToIPv4Addr tpl) port) 0)

distance :: NodeId -> NodeId -> Int
distance (NodeId n1) (NodeId n2) =
  let z = dropWhile (/= 0) (BS.zipWith xor n1 n2)
   in if last z == 0
        then 0
        else 160 - (length z * 8) + (8 - countLeadingZeros (last z))
