{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Reflex.Binary (
    IncrementalDecoder(..)
  , runIncrementalDecoder
  , CanDecode
  , getDecoder
  , CanEncode(..)
  )
where

import Data.Int (Int8, Int16, Int32, Int64)
import Numeric.Natural (Natural)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Fingerprint.Type (Fingerprint)
import Data.Ix (Ix)
import Data.Void (Void)
import Data.Version (Version)
import Data.Ratio (Ratio)
import Data.Complex (Complex)
import Data.Fixed (Fixed)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Tree (Tree)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Array.IArray (IArray)
import Data.Array.Unboxed (UArray)
import Data.Array (Array)
import Data.Map (Map)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Binary
import Data.Binary.Get

data IncrementalDecoder m b where
  IncrementalDecoder :: c -> ((String -> m ()) -> (b -> m ()) -> BS.ByteString -> c -> m (Maybe c)) -> IncrementalDecoder m b

runIncrementalDecoder :: Monad m => (String -> m ()) -> (a -> m ()) -> (() -> m ()) -> (IncrementalDecoder m a -> m ()) -> IncrementalDecoder m a -> ByteString -> m ()
runIncrementalDecoder onError onRx onStop onContinue decoder bs =
  case decoder of
    IncrementalDecoder c stepRx -> do
      mDecoder <- stepRx onError onRx bs c
      case mDecoder of
        Nothing ->
          onStop ()
        Just c' ->
          onContinue $
            IncrementalDecoder c' stepRx

class CanDecode b where
  getDecoder :: IncrementalDecoder IO b

  default getDecoder :: Binary b => IncrementalDecoder IO b
  getDecoder =
    let
      initDecode =
        runGetIncremental (get :: Get b)
      handleDecode onError _ (Fail _ _ s) = do
        onError s
        pure Nothing
      handleDecode _ _ (Partial f) =
        pure . Just $ Partial f
      handleDecode onError onRx (Done bs _ a) = do
        onRx a
        let decoder = initDecode
        if BS.null bs
        then pure . Just $ decoder
        else stepDecode onError onRx bs decoder

      stepDecode onError onRx bs decoder =
        handleDecode onError onRx $ pushChunk decoder bs
    in
      IncrementalDecoder initDecode stepDecode

instance CanDecode BS.ByteString where
  getDecoder =
    IncrementalDecoder () $ \_ onRx bs _ ->
      onRx bs >> pure (Just ())

instance CanDecode LBS.ByteString where
  getDecoder =
    IncrementalDecoder () $ \_ onRx bs _ ->
      (onRx . LBS.fromStrict) bs >> pure (Just ())

class CanEncode a where
  doEncode :: a -> BS.ByteString

  default doEncode :: Binary a => a -> BS.ByteString
  doEncode =
    LBS.toStrict . encode

instance CanEncode BS.ByteString where
  doEncode =
    id

instance CanEncode LBS.ByteString where
  doEncode =
    LBS.toStrict

instance CanDecode Bool
instance CanDecode Char
instance CanDecode Double
instance CanDecode Float
instance CanDecode Int
instance CanDecode Int8
instance CanDecode Int16
instance CanDecode Int32
instance CanDecode Int64
instance CanDecode Integer
instance CanDecode Natural
instance CanDecode Ordering
instance CanDecode Word
instance CanDecode Word8
instance CanDecode Word16
instance CanDecode Word32
instance CanDecode Word64
instance CanDecode ()
instance CanDecode Void
instance CanDecode Version
instance CanDecode Fingerprint
instance CanDecode IntSet
instance Binary a => CanDecode [a]
instance Binary a => CanDecode (Maybe a)
instance (Binary a, Integral a) => CanDecode (Ratio a)
instance Binary a => CanDecode (Complex a)
instance CanDecode (Fixed a)
instance Binary e => CanDecode (IntMap e)
instance Binary e => CanDecode (Tree e)
instance Binary e => CanDecode (Seq e)
instance Binary a => CanDecode (Set a)
instance (Binary a, Binary b) => CanDecode (Either a b)
instance (Binary i, Ix i, Binary e, IArray UArray e) => CanDecode (UArray i e)
instance (Binary i, Ix i, Binary e) => CanDecode (Array i e)
instance (Binary k, Binary e) => CanDecode (Map k e)
instance (Binary a, Binary b) => CanDecode (a, b)
instance (Binary a, Binary b, Binary c) => CanDecode (a, b, c)
instance (Binary a, Binary b, Binary c, Binary d) => CanDecode (a, b, c, d)
instance (Binary a, Binary b, Binary c, Binary d, Binary e) => CanDecode (a, b, c, d, e)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f) => CanDecode (a, b, c, d, e, f)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g) => CanDecode (a, b, c, d, e, f, g)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g, Binary h) => CanDecode (a, b, c, d, e, f, g, h)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g, Binary h, Binary i) => CanDecode (a, b, c, d, e, f, g, h, i)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g, Binary h, Binary i, Binary j) => CanDecode (a, b, c, d, e, f, g, h, i, j)

instance CanEncode Bool
instance CanEncode Char
instance CanEncode Double
instance CanEncode Float
instance CanEncode Int
instance CanEncode Int8
instance CanEncode Int16
instance CanEncode Int32
instance CanEncode Int64
instance CanEncode Integer
instance CanEncode Natural
instance CanEncode Ordering
instance CanEncode Word
instance CanEncode Word8
instance CanEncode Word16
instance CanEncode Word32
instance CanEncode Word64
instance CanEncode ()
instance CanEncode Void
instance CanEncode Version
instance CanEncode Fingerprint
instance CanEncode IntSet
instance Binary a => CanEncode [a]
instance Binary a => CanEncode (Maybe a)
instance (Binary a, Integral a) => CanEncode (Ratio a)
instance Binary a => CanEncode (Complex a)
instance CanEncode (Fixed a)
instance Binary e => CanEncode (IntMap e)
instance Binary e => CanEncode (Tree e)
instance Binary e => CanEncode (Seq e)
instance Binary a => CanEncode (Set a)
instance (Binary a, Binary b) => CanEncode (Either a b)
instance (Binary i, Ix i, Binary e, IArray UArray e) => CanEncode (UArray i e)
instance (Binary i, Ix i, Binary e) => CanEncode (Array i e)
instance (Binary k, Binary e) => CanEncode (Map k e)
instance (Binary a, Binary b) => CanEncode (a, b)
instance (Binary a, Binary b, Binary c) => CanEncode (a, b, c)
instance (Binary a, Binary b, Binary c, Binary d) => CanEncode (a, b, c, d)
instance (Binary a, Binary b, Binary c, Binary d, Binary e) => CanEncode (a, b, c, d, e)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f) => CanEncode (a, b, c, d, e, f)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g) => CanEncode (a, b, c, d, e, f, g)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g, Binary h) => CanEncode (a, b, c, d, e, f, g, h)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g, Binary h, Binary i) => CanEncode (a, b, c, d, e, f, g, h, i)
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g, Binary h, Binary i, Binary j) => CanEncode (a, b, c, d, e, f, g, h, i, j)


