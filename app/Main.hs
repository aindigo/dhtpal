module Main where

import Lib

import System.IO

import Control.Concurrent
import Control.Monad (forever, unless, void)
import Control.Monad.Trans.State
import qualified Control.Exception as E

import qualified Data.ByteString as S

import Network.Socket
import Network.Socket.ByteString

openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

suma :: Integer
suma = 1 + 1

sumas :: Num a => Maybe a -> Maybe a -> Maybe a
sumas a b = (+) <$> a <*> b

suma' :: Num a => Maybe a -> Maybe a -> Maybe a
suma' (Just a) (Just b) = Just (a + b)
suma' _ _ = Nothing

runUDPSocket :: Maybe HostName -> ServiceName -> ((S.ByteString, SockAddr) -> IO a) -> IO a
runUDPSocket hn port fn = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where

    resolve = do
      let hints = defaultHints {
                    addrFlags = [AI_PASSIVE] ,
                    addrSocketType = Datagram
                               }
      head <$> getAddrInfo (Just hints) hn (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      return sock

    loop sock = forever $ E.bracketOnError (recvFrom sock 1500) fn
      $ \t -> do
          let _ = fn t
          hFlush stdout
          loop sock

main :: IO ()
main = runUDPSocket (Just "localhost") "6666" onConnection where
  onConnection t = do
    putStrLn "recv!"
    unless (S.null $ fst t) $ do
      putStr (show $ fst t)
