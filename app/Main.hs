{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BEncode (BEncode)
import Control.Concurrent (forkFinally, forkIO, newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as E
import Control.Monad (void)
import Core.Text (unBytes)
import Data.Binary (encode, decode, decodeOrFail)
import qualified Data.ByteString.Lazy as BS
import NodeId (distance, nodeId)
import Z.Data.CBytes (fromBytes, pack)
import Z.Data.Vector (Bytes, unpack)
import Z.Data.Vector.Base (packASCII)
import Z.IO.Network (SocketAddr, initUDP, ipv4, recvUDPLoop)
import Z.IO.Network.UDP
import Z.IO.Resource (withResource)
import qualified Z.Data.Vector.Base as DB

udpConfig :: UDPConfig
udpConfig = UDPConfig 1450 (Just (ipv4 "127.0.0.1" 7890, UDP_REUSEADDR))

recvConfig :: UDPRecvConfig
recvConfig = UDPRecvConfig {recvMsgSize = 1450, recvBatchSize = 16}

main :: IO ()
main = do
  sem <- newEmptyMVar
  tid <-
    forkFinally
      ( do
          withResource (initUDP udpConfig) udploop
      )
      ( \e -> do
          print $ "error" ++ show e
          putMVar sem True
      )
  takeMVar sem
  return ()

udploop :: UDP -> IO ()
udploop a = do
  recvUDPLoop
    recvConfig
    a
    ( \case
        (Nothing, _, c) -> print "hello"
        (Just s, False, c) ->
          ( do
              case decodeOrFail (BS.pack (unpack c)) of
                Right (_, _, decoded) -> do
                  sendUDP a s (DB.pack (BS.unpack (encode (show decoded))))
                  print (decoded :: BEncode)
                Left (_, _, s) -> print s
          )
          where
            handler :: E.IOException -> IO ()
            handler e = do
              sendUDP a s (packASCII "error!")
              return ()
    )
