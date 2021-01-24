{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module BEncode
  ( BEncode (..),)
where

import Data.Binary (Binary (get, put), Get, Put)
import Data.Binary.Get (getByteString, lookAheadM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (digitToInt, isDigit)
import qualified Data.Map as M
import Data.Word ()
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

data BEncode
  = BInt Integer
  | BStr B.ByteString
  | BList [BEncode]
  | BDict (M.Map B.ByteString BEncode)
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
      putElems [] = put ()
  put (BDict ms) = M.foldlWithKey f (put 'd') ms >> put 'e'
    where
      f :: Put -> B.ByteString -> BEncode -> Put
      f a k b = a >> put (BStr k) >> put b

  get = do
    c <- get
    case c of
      'i' -> readBInt
      c | isDigit c -> readBStr c
      'l' -> readBList
      'd' -> readBDict
      _ -> fail "bad encoding"

throwError :: MonadFail m => m a
throwError = fail "error decoding bytestring"

readInt' :: Int -> Get Int
readInt' acc = do
  d <- lookAheadM digit'
  return (maybe acc (\digit -> acc * 10 + digitToInt digit) d)
  where
    digit' = do
      d' <- get
      return
        if isDigit d'
          then Just d'
          else Nothing

-- TODO : Handle negavite numbers
readBInt :: Get BEncode
readBInt = do
  d <- get
  if isDigit d
    then do
      n <- readInt' (digitToInt d)
      e <- get
      if e == 'e'
        then return (BInt (toInteger n))
        else throwError
    else throwError

readBStr :: Char -> Get BEncode
readBStr d = do
  sz <- readInt' (digitToInt d)
  col <- get
  if col == ':'
    then fmap BStr (getByteString sz)
    else throwError

readBList :: Get BEncode
readBList = do
  items <- getItems []
  e <- get
  if e == 'e'
    then return (BList items)
    else throwError
  where
    getItems :: [BEncode] -> Get [BEncode]
    getItems acc = do
      i <- get
      getItems (acc ++ [i])

readBDict :: Get BEncode
readBDict = do
  items <- getKeysNItems []
  e <- get
  if e == 'e'
    then return (BDict (M.fromList items))
    else throwError
  where
    getKeysNItems :: [(C8.ByteString, BEncode)] -> Get [(C8.ByteString, BEncode)]
    getKeysNItems acc = do
      k <- get
      v <- get
      getKeysNItems (acc ++ [(k, v)])
