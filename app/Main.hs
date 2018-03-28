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
import Operation
import Data.Maybe
import Control.Monad.Reader
import qualified Songs as S

data HandlerProp = HandlerProp {getData :: Maybe Object, getKey :: TwitterKey,
                                getSongBase :: S.SongBase}
type Handler = ReaderT HandlerProp IO ()

name = "Keck_init"

askData :: ReaderT HandlerProp IO (Maybe Object)
askData =  getData <$> ask

askKey :: ReaderT HandlerProp IO (TwitterKey)
askKey =  getKey <$> ask

askSongBase :: ReaderT HandlerProp IO (S.SongBase)
askSongBase =  getSongBase <$> ask

handler :: Handler
handler = do
  d <- askData
  let tweet = d >>= HM.lookup "text" >>= takeString
  case tweet of
    Nothing -> return ()
    Just t -> liftIO . putStrLn $ "tweet:" <> t

replyHandler :: Handler
replyHandler = do
  d <- askData
  key <- askKey
  case reply_text d of
    Just s -> liftIO . putStrLn $ s
    Nothing -> return ()
  case d of
    Just obj ->
      if is_reply_to_me d then
        (liftIO $ reply key obj "got replied") >> return ()
      else
        return ()
    Nothing -> return ()

    where
      is_reply_to_me_m d = do
        obj <- d
        screen_name <- HM.lookup "in_reply_to_screen_name" obj >>= takeString
        if screen_name == name then return True else return False

      is_reply_to_me = fromMaybe False . is_reply_to_me_m

      reply_text d = do
        obj <- d
        tweet <- HM.lookup "text" obj >>= takeString
        if is_reply_to_me d then return () else Nothing
        return $ "reply!!!! tweet:" <> tweet

followBackHandler :: Handler
followBackHandler = do
  d <- askData
  key <- askKey
  case follower d of
    Just s -> (liftIO . putStrLn $ "followed!:" <> s) >> (liftIO $ followUser key s)
              >> return ()
    Nothing -> return ()
    where
      follower d = do
        obj <- d
        event_name <- HM.lookup "event" obj >>= takeString
        if event_name == "follow" then Just () else Nothing
        followed_user <- case HM.lookup "target" obj of
          Just (Object o) -> HM.lookup "screen_name" o >>= takeString
          _ -> Nothing
        if followed_user == name then Just () else Nothing
        case HM.lookup "source" obj of
          Just (Object o) -> HM.lookup "id_str" o >>= takeString
          _ -> Nothing


splitter :: ConduitM B.ByteString B.ByteString IO ()
splitter = inner "" where
  inner buf = do
    md <- await
    case md of
      Nothing -> return ()
      Just d -> case CB.breakSubstring (CB.pack "\r\n") (buf <> d) of
        (remaining, "") -> inner remaining
        (matched, remaining) -> yield matched >> inner (B.drop 2 remaining)

sink :: (Monad m, MonadIO m) => TwitterKey -> S.SongBase ->
                                [Handler] -> Response () -> ConduitM B.ByteString Void m ()
sink key song_base handlers response =
  if getResponseStatusCode response > 300 then
    liftIO $ print "error"
  else do
    b <- await
    case b of
      Nothing -> return ()
      Just d ->
        let
          obj = decode . LB.fromStrict $ d
          prop = HandlerProp obj key song_base
        in (liftIO $ forM_ handlers (\h -> runReaderT h prop)) >> sink key song_base handlers response


main :: IO ()
main = do
  json_path <- head <$> getArgs
  songs_json <- B.readFile json_path
  let song_base_e = S.createSongs songs_json >>= return . S.createSongBase
  case song_base_e of
    Left err -> putStrLn err
    Right song_base -> do
      let url = "https://userstream.twitter.com/1.1/user.json"
      let param = [("replies", "all")]
      key <- TwitterKey <$> (getEnv "CK") <*> (getEnv "CS") <*> (getEnv "AT") <*> (getEnv "AS")
      let handlers = [handler, replyHandler, followBackHandler]
      request <- createRequest False url param [] key
      httpSink request $ \res -> splitter .| (sink key song_base handlers res)
      print "done"



