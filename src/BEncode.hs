{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module BEncode (BEncode (..)) where

import qualified Control.Exception as E
import Data.Binary (Binary (get, put), Get, Put)
import Data.Binary.Get (getByteString, lookAheadM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
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
encodeString = foldl' (\a c -> a >> put c) (put ())

encodeByteString :: B.ByteString -> Put
encodeByteString = B.foldl' (\a c -> a >> put c) (put ())

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
  listOfBencode [] >>= \l -> return $ BList l
  where
    listOfBencode :: [BEncode] -> Get [BEncode]
    listOfBencode acc =
      lookAheadM maybeEnd
        >>= ( \case
                Nothing -> do
                  b <- get :: Get BEncode
                  listOfBencode (acc ++ [b])
                Just e -> return acc
            )

readBDict :: Get BEncode
readBDict = do
  listOfKVBencode [] >>= \l -> return $ BDict (M.fromList l)
  where
    listOfKVBencode :: [(B.ByteString, BEncode)] -> Get [(B.ByteString, BEncode)]
    listOfKVBencode acc =
      lookAheadM maybeEnd
        >>= ( \case
                Nothing -> do
                  k <- get :: Get BEncode
                  v <- get :: Get BEncode
                  case k of
                    BStr str -> listOfKVBencode (acc ++ [(str, v)])
                    _ -> throwError 'k'
                Just e -> return acc
            )

maybeEnd :: Get (Maybe Char)
maybeEnd =
  get >>= \case
    'e' -> return $ Just 'e'
    _ -> return Nothing
