{-# LANGUAGE OverloadedStrings #-}
module Main where

import OAuth
import Network.HTTP.Simple
import System.Environment
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Conduit (Sink, await, yield, ConduitM, Void, (.|))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Word
import Debug.Trace
import qualified Data.HashMap.Strict as HM
import Data.Aeson (decode)
import Data.Aeson.Types
import qualified Data.Text as T

type Handler = Maybe Object -> IO ()

handler :: Handler
handler d = case tweet of
  Nothing -> print $ "not a tweet: "  <> show d
  Just t -> putStrLn $ "tweet:" <> T.unpack t
  where
    take_string v = case v of
      String s -> Just s
      otherwise -> Nothing
    tweet = d >>= HM.lookup "text" >>= take_string

splitter :: ConduitM B.ByteString B.ByteString IO ()
splitter = inner "" where
  inner buf = do
    md <- await
    case md of
      Nothing -> return ()
      Just d -> case CB.breakSubstring (CB.pack "\r\n") (buf <> d) of
        (remaining, "") -> inner remaining
        (matched, remaining) -> yield matched >> inner (B.drop 2 remaining)

sink :: (Monad m, MonadIO m) => Handler -> Response () -> ConduitM B.ByteString Void m ()
sink handler response =
  if getResponseStatusCode response > 300 then
    liftIO $ print "error"
  else do
    b <- await
    case b of
      Nothing -> return ()
      Just d -> (liftIO . handler . decode . LB.fromStrict $ d) >> sink handler response


main :: IO ()
main = do
  let url = "https://userstream.twitter.com/1.1/user.json"
  let param = [("replies", "all")]
  key <- TwitterKey <$> (getEnv "CK") <*> (getEnv "CS") <*> (getEnv "AT") <*> (getEnv "AS")
  request <- createRequest False url param [] key
  httpSink request $ \res -> splitter .| (sink handler res)
  print "done"


