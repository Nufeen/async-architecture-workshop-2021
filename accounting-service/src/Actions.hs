{-# LANGUAGE OverloadedStrings #-}

module Actions where

import DB

import Data.ByteString (ByteString)

-- TODO implement writeoff
reassign :: ByteString -> IO ()
reassign n = do
  print $ "reassigned " <> n <> " event recieved"

-- TODO implement charge
close :: ByteString -> IO ()
close n = do
  print $ "close task " <> n <> " event recieved"

-- TODO implement balance clear
clearBalanceSheet :: IO ()
clearBalanceSheet = do
  print "Daily CRON task recieved"
