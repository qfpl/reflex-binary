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
  , CanEncode
  , doEncode
  )
where

import Data.Functor.Identity

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

class CanDecode' m b where
  getDecoder :: IncrementalDecoder m b

instance Monad m => CanDecode' m BS.ByteString where
  getDecoder =
    IncrementalDecoder () $ \_ onRx bs _ ->
      onRx bs >> pure (Just ())

instance Monad m => CanDecode' m LBS.ByteString where
  getDecoder =
    IncrementalDecoder () $ \_ onRx bs _ ->
      (onRx . LBS.fromStrict) bs >> pure (Just ())

instance (Monad m, Binary b) => CanDecode' m b where
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

type CanDecode b = CanDecode' IO b

class CanEncode' m a where
  doEncode' :: a -> m BS.ByteString

instance Monad m => CanEncode' m BS.ByteString where
  doEncode' =
    pure

instance Monad m => CanEncode' m LBS.ByteString where
  doEncode' =
    pure . LBS.toStrict

instance (Monad m, Binary a) => CanEncode' m a where
  doEncode' =
    pure . LBS.toStrict . encode

type CanEncode a = CanEncode' Identity a

doEncode :: CanEncode a => a -> BS.ByteString
doEncode = runIdentity . doEncode'
