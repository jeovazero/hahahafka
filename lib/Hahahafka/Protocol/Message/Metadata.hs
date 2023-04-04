{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Hahahafka.Protocol.Message.Metadata where

import Data.Serialize (runPut)
import Data.Serialize.Put (Put, putInt32be, putInt16be, putByteString)
import Data.Serialize.Get
    ( Get, runGet, getInt32be, getByteString, getInt16be )

import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B8
import Data.Int ( Int16, Int32 )
import Prelude

import qualified Hahahafka.Protocol.Constant.ApiKey as ApiKey
import Control.Monad (replicateM)

newtype KString = KString B8.ByteString deriving (Show)
type TopicName = KString

newtype MetadataReqV0 = MetadataReqV0 [TopicName] deriving (Show)

data Broker
  = Broker {
      nodeId :: Int32,
      host :: KString,
      port :: Int32
    } deriving (Show)

data TopicMetadata
  = TopicMetadata {
      errorCode :: Int16,
      name :: KString,
      partitions :: [PartitionMeta]
    } deriving (Show)

data PartitionMeta
  = PartitionMeta {
      errorCode :: Int16,
      partitionId :: Int32,
      leader :: Int32,
      replicas :: [Int32],
      isr :: [Int32]
    } deriving (Show)

data MetadataResponseV0
  = MetadataReponseV0 {
      brokers :: [Broker],
      topicsMetadata :: [TopicMetadata]
    } deriving (Show)

--
-- Temporary
--
correlationId = 0
clientId = "hahahafka"
apiVersion = 0

--
-- Encode
--
class KEncode a where
  encode :: a -> ByteString
  encode a = runPut $ putBinary a

  putBinary :: a -> Put

--
-- basic
--
instance KEncode KString where
  putBinary (KString bs) = do
    putInt16be . fromIntegral $ B8.length bs -- string uses int16
    putByteString bs

instance (KEncode a) => KEncode [a] where
  putBinary ks = do
    putInt32be . fromIntegral . Prelude.length $ ks -- :( O(n)
    mapM_ putBinary ks

--
-- specific
--
instance KEncode MetadataReqV0 where
  putBinary (MetadataReqV0 topics) = do
    putInt16be ApiKey.metadata
    putInt16be apiVersion
    putInt32be correlationId
    putBinary $ KString clientId
    putBinary topics

--
-- Decode
--
class KDecode a where
  decode :: ByteString -> Either String a
  decode = runGet getBinary

  getBinary :: Get a

--
-- basic
--
instance (KDecode a) => KDecode [a] where
  getBinary = do
    size <- getInt32be
    replicateM (fromIntegral size) getBinary

instance KDecode KString where
  getBinary = do
    size <- getInt16be
    str <- getByteString $ fromIntegral size
    pure $ KString str

instance KDecode Int32 where
  getBinary = getInt32be

--
-- specific
-- 
instance KDecode Broker where
  getBinary = do
    nodeId <- getInt32be
    host <- getBinary
    port <- getInt32be
    pure $ Broker {..}

instance KDecode PartitionMeta where
  getBinary = do
    errorCode <- getInt16be
    partitionId <- getInt32be
    leader <- getInt32be
    replicas <- getBinary
    isr <- getBinary
    pure $ PartitionMeta { .. }

instance KDecode TopicMetadata where
  getBinary = do
    errorCode <- getInt16be
    name <- getBinary
    partitions <- getBinary
    pure $ TopicMetadata {..}

instance KDecode MetadataResponseV0 where
  getBinary = do
    _responseSize <- getInt32be -- to validate
    _correlationId <- getInt32be -- to validate
    brokers <- getBinary :: Get [Broker]
    topics <- getBinary :: Get [TopicMetadata]
    pure $ MetadataReponseV0 brokers topics

