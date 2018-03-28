module Operation where

import OAuth
import Network.HTTP.Simple

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




