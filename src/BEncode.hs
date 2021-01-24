{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module BEncode
  ( BEncode (..),
  )
where

import Data.Binary ( Get, Binary(get, put), Put )
import Data.Binary.Get ( getByteString, lookAheadM )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

import Data.Char (digitToInt, isDigit)
import qualified Data.Map as M
import Data.Word ()
import GHC.Generics ( Generic )
import GHC.Stack (HasCallStack)

data BEncode
  = BInt Integer
  | BStr B.ByteString
  | BList [BEncode]
  | BDict (M.Map B.ByteString BEncode)
  | Wrong
  deriving (Eq, Ord, Show, Generic)

encodeString :: String -> Put
encodeString = foldl (\a c -> a >> put c) (put ())

encodeByteString :: B.ByteString -> Put
encodeByteString = B.foldl (\a c -> a >> put c) (put ())

instance Binary BEncode where
  put (BInt i) = put 'i' >> encodeString (show i) >> put 'e'
  put (BStr bs) = encodeString (show (B.length bs)) >> put ':' >> encodeByteString bs
  put (BList bs) = put 'l' >> putElems bs >> put 'e'
    where
      putElems (x : xs) = put x >> putElems xs
      putElems [] = do put ()
  put (BDict ms) = M.foldlWithKey f (put 'd') ms >> put 'e'
    where
      f a k b = a >> put (BStr k) >> put b

  get = do
    c <- get
    case c of
      c | isDigit c -> readBStr c
      'i' -> readBInt
      'l' -> readBList
      'd' -> readBDict
      _ -> return Wrong

digitToInteger :: Char -> Integer
digitToInteger = toInteger . digitToInt

readInt' :: Int -> Get Int
readInt' acc = do
  d <- lookAheadM digit'
  case d of
    Just d -> readInt' (acc * 10 + digitToInt d)
    Nothing -> return acc
  where
    digit' = do
      d' <- get
      return
        if isDigit d' then Just d'
                      else Nothing

readBInt :: Get BEncode
readBInt = do
  d <- get
  if isDigit d
    then do
      n <- readInt' (digitToInt d)
      e <- get
      return
        if e == 'e'
          then BInt (toInteger n)
          else Wrong
    else return Wrong

readBStr :: Char -> Get BEncode
readBStr d = do
  sz <- readInt' (digitToInt d)
  col <- get
  if col == ':'
    then fmap BStr (getByteString sz)
    else return Wrong

readBList :: Get BEncode
readBList = do
  items <- getItems []
  e <- get
  return
    if e == 'e'
      then BList items
      else Wrong
  where
    getItems acc = do
      i <- lookAheadM maybeItem
      case i of
        Just its -> getItems (acc ++ [its])
        Nothing -> return acc
    maybeItem = do
      mi <- get
      return case mi of
        Wrong -> Nothing
        i -> Just i

readBDict :: Get BEncode
readBDict = do
  items <- getKeysNItems []
  e <- get
  return
    if e == 'e'
      then BDict (M.fromList items)
      else Wrong
  where
    getKeysNItems acc = do
      mk <- lookAheadM maybeKey
      case mk of
        Just k -> do
          v <- get
          getKeysNItems (acc ++ [(k, v)])
        Nothing -> return acc
    maybeKey = do
      mi <- get
      return case mi of
        BStr k -> Just k
        _ -> Nothing
