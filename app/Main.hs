{-# LANGUAGE OverloadedStrings #-}

module Main where

import BEncode (BEncode)
import NodeId (nodeId, distance)

import Z.IO.Network ( recvUDPLoop, ipv4, SocketAddr, initUDP )
import Z.Data.Vector (Bytes)
import Z.IO.Network.UDP

import Z.IO.Resource (withResource)

udpConfig :: UDPConfig
udpConfig = UDPConfig 16 (Just (ipv4 "0.0.0.0" 7890, UDP_REUSEADDR))

main :: IO ()
main = print ()
