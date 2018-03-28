{-# LANGUAGE OverloadedStrings #-}
module Operation where

import OAuth
import Network.HTTP.Simple
import qualified Data.HashMap.Strict as HM
import Data.Aeson (decode)
import Data.Aeson.Types
import qualified Data.Text as T

data CreateFriendship = CreateFriendship {screenName :: Maybe String, userId :: Maybe String, follow :: Maybe Bool}
  deriving Show


takeString :: Value -> Maybe String
takeString (String s) = Just . T.unpack $ s
takeString _ = Nothing

takeObj :: Value -> Maybe Object
takeObj (Object o) = Just o
takeObj _ = Nothing

followUser :: TwitterKey -> String -> IO Bool
followUser key user = do
  let url = "https://api.twitter.com/1.1/friendships/create.json"
  request <- createRequest True url [] [("user_id", user)] key
  res <- httpBS request
  let response_no = getResponseStatusCode res
  if response_no > 300 then
    (print $ getResponseBody res) >> return False
  else
    return True

reply :: TwitterKey -> Object -> String -> IO Bool
reply key obj message = do
  case create_reply of
    Nothing -> print obj >> return False
    Just reply -> do
      res <- reply >>= httpBS
      if getResponseStatusCode res > 300 then
        (print $ getResponseBody res) >> return False
      else
        return True
  where
    create_reply = do
      let url = "https://api.twitter.com/1.1/statuses/update.json"
      status_id <- HM.lookup "id_str" obj >>= takeString
      screen_name <-  HM.lookup "user" obj >>= takeObj >>= HM.lookup "screen_name" >>= takeString
      let reply_message = "@" ++ screen_name ++ " " ++ message
      let param = [("status", reply_message), ("in_reply_to_status_id", status_id)]
      return $ createRequest True url [] param key

