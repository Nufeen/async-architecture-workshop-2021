{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Kafka where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception       (bracket)
import Control.Monad           (forM_)
import Control.Monad.IO.Class  (MonadIO(..))
import Data.ByteString         (ByteString)
import Data.ByteString.Char8   (pack)
import Kafka.Consumer          (Offset)
import Kafka.Producer
import Data.Text               (Text)

-- Global producer properties
producerProps :: ProducerProperties
producerProps = brokersList [BrokerAddress "localhost:29092"]
             -- <> sendTimeout (Timeout 10000)
             <> setCallback (deliveryCallback print)
             <> logLevel KafkaLogDebug

-- Topic to send messages to
targetTopic :: TopicName
targetTopic = TopicName "tasks"

mkMessage :: Maybe ByteString -> Maybe ByteString -> ProducerRecord
mkMessage k v = ProducerRecord
                  { prTopic = targetTopic
                  , prPartition = UnassignedPartition
                  , prKey = k
                  , prValue = v
                  }

runProducer :: String -> String ->  IO ()
runProducer k v =
    bracket mkProducer clProducer runHandler >>= print
    where
      mkProducer = newProducer producerProps
      clProducer (Left _)     = return ()
      clProducer (Right prod) = closeProducer prod
      runHandler (Left err)   = return $ Left err
      runHandler (Right prod) = sendMessages prod k v

sendMessages :: KafkaProducer -> String -> String -> IO (Either KafkaError ())
sendMessages prod k v  = do
  err1 <- produceMessage prod (mkMessage (Just $ pack k) (Just $ pack v))
  forM_ err1 print
  return $ Right ()
