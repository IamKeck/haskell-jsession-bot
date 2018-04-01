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
import Handler
import qualified Songs as S
import Control.Monad.Reader
import Tweet

name = "jsession_bot"

replyHandler :: Handler
replyHandler = do
  d <- askData
  key <- askKey
  if isReplyToMe name d then do
    chosen_song <- askSongBase >>= liftIO . S.chooseSongIO
    (liftIO . reply key d $ "次は" ++ chosen_song ++ "なんていかがでしょうか")
    return ()
  else
    return ()


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
      event_name <- getEventName d
      if event_name == "follow" then Just () else Nothing
      followed_user <- getEventTargetName d
      if followed_user == name then Just () else Nothing
      getEventSourceId d


main :: IO ()
main = do
  json_path <- head <$> getArgs
  songs_json <- B.readFile json_path
  let song_base_e = S.createSongs songs_json >>= return . S.createSongBase
  case song_base_e of
    Left err -> putStrLn err
    Right song_base -> do
      key <- TwitterKey <$> (getEnv "CK") <*> (getEnv "CS") <*> (getEnv "AT") <*> (getEnv "AS")
      let handlers = [replyHandler, followBackHandler]
      connectUserStream song_base key [("replies", "all")] handlers
      print "done"



