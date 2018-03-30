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


name = "jsession_bot"


handler :: Handler
handler = do
  d <- askData
  let tweet = Just d >>= HM.lookup "text" >>= takeString
  case tweet of
    Nothing -> return ()
    Just t -> liftIO . putStrLn $ "tweet:" <> t

replyHandler :: Handler
replyHandler = do
  d <- askData
  key <- askKey
  if is_reply_to_me d then do
    chosen_song <- askSongBase >>= liftIO . S.chooseSongIO
    (liftIO . reply key d $ "次は" ++ chosen_song ++ "なんていかがでしょうか")
    return ()
  else
    return ()

  where
    is_reply_to_me_m obj = do
      screen_name <- HM.lookup "in_reply_to_screen_name" obj >>= takeString
      if screen_name == name then return True else return False

    is_reply_to_me = fromMaybe False . is_reply_to_me_m

followBackHandler :: Handler
followBackHandler = do
  d <- askData
  key <- askKey
  case follower d of
    Just s -> (liftIO . putStrLn $ "followed!:" <> s) >> (liftIO $ followUser key s)
              >> return ()
    Nothing -> return ()
    where
      follower obj = do
        event_name <- HM.lookup "event" obj >>= takeString
        if event_name == "follow" then Just () else Nothing
        followed_user <- case HM.lookup "target" obj of
          Just (Object o) -> HM.lookup "screen_name" o >>= takeString
          _ -> Nothing
        if followed_user == name then Just () else Nothing
        case HM.lookup "source" obj of
          Just (Object o) -> HM.lookup "id_str" o >>= takeString
          _ -> Nothing


main :: IO ()
main = do
  json_path <- head <$> getArgs
  songs_json <- B.readFile json_path
  let song_base_e = S.createSongs songs_json >>= return . S.createSongBase
  case song_base_e of
    Left err -> putStrLn err
    Right song_base -> do
      key <- TwitterKey <$> (getEnv "CK") <*> (getEnv "CS") <*> (getEnv "AT") <*> (getEnv "AS")
      let handlers = [handler, replyHandler, followBackHandler]
      connectUserStream song_base key [("replies", "all")] handlers
      print "done"



