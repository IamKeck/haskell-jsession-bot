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
import Data.Foldable (forM_)

type Handler = Maybe Object -> IO ()

name = "Keck_init"

takeString :: Value -> Maybe String
takeString (String s) = Just . T.unpack $ s
takeString _ = Nothing

handler :: Handler
handler d = case tweet of
  Nothing -> return ()
  Just t -> putStrLn $ "tweet:" <> t
  where
    tweet = d >>= HM.lookup "text" >>= takeString

replyHandler :: Handler
replyHandler d = do
  case reply_text of
    Just s -> putStrLn s
    Nothing -> return ()
    where
      reply_text = do
        obj <- d
        screen_name <- HM.lookup "in_reply_to_screen_name" obj >>= takeString
        tweet <- HM.lookup "text" obj >>= takeString
        if screen_name == name then return () else Nothing
        return $ "reply!!!! tweet:" <> tweet



splitter :: ConduitM B.ByteString B.ByteString IO ()
splitter = inner "" where
  inner buf = do
    md <- await
    case md of
      Nothing -> return ()
      Just d -> case CB.breakSubstring (CB.pack "\r\n") (buf <> d) of
        (remaining, "") -> inner remaining
        (matched, remaining) -> yield matched >> inner (B.drop 2 remaining)

sink :: (Monad m, MonadIO m) => [Handler] -> Response () -> ConduitM B.ByteString Void m ()
sink handlers response =
  if getResponseStatusCode response > 300 then
    liftIO $ print "error"
  else do
    b <- await
    case b of
      Nothing -> return ()
      Just d ->
        let
          obj = decode . LB.fromStrict $ d
        in (liftIO $ forM_ handlers (\h -> h obj)) >> sink handlers response


main :: IO ()
main = do
  let url = "https://userstream.twitter.com/1.1/user.json"
  let param = [("replies", "all")]
  let handlers = [handler, replyHandler]
  key <- TwitterKey <$> (getEnv "CK") <*> (getEnv "CS") <*> (getEnv "AT") <*> (getEnv "AS")
  request <- createRequest False url param [] key
  httpSink request $ \res -> splitter .| (sink handlers res)
  print "done"


