{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module NodeId (NodeId, NodeInfo(..), nodeId, distance) where

import qualified Control.Lens as L
import Control.Monad.Trans.State (State, get, put)
import Data.Bits (FiniteBits (countLeadingZeros), xor)
import qualified Data.ByteString.Lazy as BS
import Data.Map (empty, fromList, lookupMin, Map, insert, lookupGE)
import Data.Word (Word8)
import GHC.Exts (sortWith)
import Z.IO.Network (PortNumber, SocketAddr (SocketAddrIPv4), tupleToIPv4Addr)
import Z.IO.Network.SocketAddr (ipv4)
import qualified Z.Data.CBytes as ZD

newtype NodeId = NodeId BS.ByteString deriving (Show, Eq, Ord)

data NodeInfo = NodeInfo 
  { _sockAddr :: SocketAddr,
    _lastUpdated :: Int
  }
  deriving (Show)

newtype RoutingTable = RT (NodeId, NodeInfo, Map Int [(NodeId, NodeInfo)])

k :: Int
k = 8

L.makeLenses ''NodeInfo
L.makeLenses ''RoutingTable

type RoutingTableS a = State RoutingTable a

localId :: RoutingTableS NodeId
localId = get >>= (\(RT (nodeId, _, _)) -> return nodeId)

localInfo :: RoutingTableS NodeInfo
localInfo = get >>= (\(RT (_, localInfo, _)) -> return localInfo)

buckets :: RoutingTableS (Map Int [(NodeId, NodeInfo)])
buckets = get >>= (\(RT (_, _, bks)) -> return bks)

freeBucket :: Int -> RoutingTableS (Maybe [(NodeId, NodeInfo)])
freeBucket distance =
  buckets
    >>= ( \bs ->
            let bucket = lookupGE distance bs
             in pure (bucket >>= (\(key, b) -> if length b < k then Just b else Nothing))
        )

insertNode :: NodeId -> NodeInfo -> RoutingTableS (Either String ())
insertNode nodeId nodeInfo = do
  lid <- localId
  lnfo <- localInfo
  bs <- buckets
  freeBucket <- freeBucket (distance lid nodeId)
  case freeBucket of
    Just b ->
      let newBucket = sortWith (distance lid . fst) (b ++ [(nodeId, nodeInfo)])
       in do
            put (RT (lid, lnfo, insert (distance lid nodeId) newBucket bs))
            return $ Right ()
    Nothing -> return (Left "not enought space.")

splitBucket :: RoutingTableS Bool
splitBucket = get >>=
  \(RT (lid, linfo, bs)) ->
    case lookupMin bs of
      Just (k, ls) ->
        if length ls == k then
                          let splited = span (\(nid,nfo) -> distance nid lid < k) ls
                              oldlast = insert k (snd splited) bs
                              newlast = insert k (fst splited) oldlast in do
                                put (RT (lid, linfo, newlast))
                                return True
                          else
                           return False
      Nothing -> return False


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
        else 160 - length z * 8 + (8 - countLeadingZeros (last z))
