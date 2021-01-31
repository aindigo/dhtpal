
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
      'i' -> do
        sign <- lookAheadM signDigit
        maybe (throwError c) readBInt sign
      c | isDigit c -> readBStr c
      'l' -> readBList
      'd' -> readBDict
      _ -> fail ("ee bad encoding " ++ show c)
      where
        signDigit :: Get (Maybe (Integer, Integer))
        signDigit = do
          s <- get
          case s of
            '-' -> return (Just (-1, 0))
            s | isDigit s -> return (Just (1, toInteger (digitToInt s)))
            _ -> return Nothing

throwError :: MonadFail m => Char -> m a
throwError c = fail ("error decoding bytestring : " ++ show c)

readInt' :: Integer -> Get Integer
readInt' acc = do
  d <- lookAheadM digit'
  maybe (return acc) (\digit -> readInt' (acc * 10 + toInteger (digitToInt digit))) d
  where
    digit' = do
      d' <- get
      return
        if isDigit d'
          then Just d'
          else Nothing

readBInt :: (Integer, Integer) -> Get BEncode
readBInt (sign, fd) = do
      n <- readInt' fd
      e <- get
      if e == 'e'
        then return (BInt (sign * toInteger n))
        else throwError e

readBStr :: Char -> Get BEncode
readBStr d = do
  sz <- readInt' (toInteger (digitToInt d))
  col <- get
  if col == ':'
    then fmap BStr (getByteString (fromIntegral sz))
    else throwError col

readBList :: Get BEncode
readBList = do
  items <- getItems []
  e <- get
  if e == 'e'
    then return (BList items)
    else throwError e
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
    else throwError e
  where
    getKeysNItems :: [(C8.ByteString, BEncode)] -> Get [(C8.ByteString, BEncode)]
    getKeysNItems acc = do
      k <- get
      v <- get
      getKeysNItems (acc ++ [(k, v)])
