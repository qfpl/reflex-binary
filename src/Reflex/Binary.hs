{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Binary (
    CanEncode(..)
  , CanDecode(..)
  , IncrementalDecoder(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Binary
import Data.Binary.Get

class CanEncode a where
  doEncode :: a -> ByteString

  default doEncode :: Binary a => a -> ByteString
  doEncode = 
    LBS.toStrict . encode

instance CanEncode BS.ByteString where
  doEncode = 
    id 

instance CanEncode LBS.ByteString where
  doEncode = 
    LBS.toStrict 

data IncrementalDecoder b where
  IncrementalDecoder :: c -> ((String -> IO ()) -> (b -> IO ()) -> BS.ByteString -> c -> IO (Maybe c)) -> IncrementalDecoder b 

class CanDecode b where
  getDecoder :: IncrementalDecoder b

  default getDecoder :: Binary b => IncrementalDecoder b
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

