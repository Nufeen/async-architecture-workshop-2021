{-# LANGUAGE OverloadedStrings #-}

module Kafka  where

import qualified Actions

import           Control.Monad        (forM_)
import           Data.Conduit -- ((.|))
import qualified Data.Conduit.List    as L
import           Data.Monoid          ((<>))
import           Kafka.Conduit.Sink   as KSnk
import           Kafka.Conduit.Source as KSrc
import Conduit
import Data.ByteString (ByteString)

kafkaBroker :: BrokerAddress
kafkaBroker = BrokerAddress "localhost:29092"

-- Topic to write to and read from
topic :: TopicName
topic = TopicName "tasks"

-- Global consumer properties
consumerProps :: ConsumerProperties
consumerProps = KSrc.brokersList [kafkaBroker]
             <> groupId (ConsumerGroupId "accounting")
             <> noAutoCommit

-- Subscription to topics
consumerSub :: Subscription
consumerSub = topics [topic] <> offsetReset Earliest

-- Global producer properties
producerProps :: ProducerProperties
producerProps = KSnk.brokersList [kafkaBroker]

main :: IO ()
main = do
  putStrLn "Start input stream processing..."
  runConduitRes $ inputStream .|  proceed
  putStrLn "Terminating..."
    where
      inputStream = kafkaSource consumerProps consumerSub (Timeout 1000)

proceed = do
  x <- await
  case x of
    Nothing -> liftIO $ print "Stream ended"
    Just (Left _) -> proceed
    Just (Right res) -> do
      liftIO $ handle res
      proceed

-- TODO: commit offset
handle :: ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> IO ()
handle record = do
  case (t, k, v) of
    ("tasks", Just "taskReassigned", Just n) -> Actions.reassign n
    ("tasks", Just "taskClosed", Just n) -> Actions.close n
    ("tasks", Just "cronTaskClearBalance", Just n) -> Actions.clearBalanceSheet
    (_, _, _) -> return ()
  where
    t = unTopicName $ crTopic record
    k = crKey record
    v = crValue record
