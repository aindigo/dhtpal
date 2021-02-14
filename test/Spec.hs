import NodeId (NodeInfo(..),  NodeId, nodeId, distance )
import BEncode 

import Test.QuickCheck ()
import Data.Map ( empty, fromList, Map )
import qualified Z.Data.CBytes as ZD
import Z.IO.Network (ipv4)
import qualified Data.ByteString.Lazy as BS
import Data.Binary

testMap :: Map Int [(NodeId, NodeInfo)]
testMap = let nid = nodeId (BS.pack [0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9]) in
              case nid of
                Right n -> fromList [(distance n n , [(n , NodeInfo (ipv4 (ZD.pack "127.0.0.1") 9999) 0)])]
                Left e -> empty

main :: IO ()
main = print testMap
