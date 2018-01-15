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
