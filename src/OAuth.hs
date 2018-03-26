{-# LANGUAGE OverloadedStrings #-}

module OAuth where
import Network.URI.Encode (encode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (random, randomRIO)
import Data.List (sortOn, intercalate)
import Data.HMAC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString.Base64  as B64 (encode)
import Network.HTTP.Simple
import Control.Monad.State.Lazy

data TwitterKey = TwitterKey {consumerKey :: String,
                              consumerSecret :: String,
                              accessToken :: String,
                              accessTokenSecret :: String} deriving Show

createHmacKey :: TwitterKey -> String
createHmacKey key = (encode . consumerSecret $ key) ++ "&" ++
                    (encode . accessTokenSecret $  key)

createAuthBase :: TwitterKey -> String -> String -> [(String, String)]
createAuthBase key nonce timestamp = [
  ("oauth_consumer_key", consumerKey key),
  ("oauth_nonce", nonce),
  ("oauth_signature_method", "HMAC-SHA1"),
  ("oauth_timestamp", timestamp),
  ("oauth_token", accessToken key),
  ("oauth_version", "1.0")]


createAuthBaseDefault :: TwitterKey -> IO [(String, String)]
createAuthBaseDefault key = do
  time <- show . floor . realToFrac <$> getPOSIXTime
  nonce <- show <$> randomRIO (0, 1.0 :: Float)
  return $ createAuthBase key nonce time


createSignature :: Bool -> String -> [(String, String)] -> [(String, String)] -> String -> String
createSignature is_post url param auth_base hmac_key =
  let
    auth_param = auth_base ++ param
    encoded_param = [(encode k, encode v) | (k, v) <- auth_param ]
    sorted_param = sortOn fst encoded_param
    auth_str = intercalate "&" [k ++ "=" ++ v | (k, v) <- sorted_param] :: String
    method = if is_post then "POST" else "GET"
    auth_base_str = method ++ "&" ++ (encode url) ++ "&" ++ (encode auth_str) :: String
    str_to_octets = B.unpack . CB.pack
  in
    CB.unpack . B64.encode . B.pack $ hmac_sha1 (str_to_octets hmac_key) (str_to_octets auth_base_str)

createAuthHeader :: [(String, String)] -> String -> String
createAuthHeader auth_base signature =
  let
    auth_base_sorted = sortOn fst $ ("oauth_signature", signature) :  auth_base
  in
    "OAuth " ++ intercalate ", " [(encode k) ++ "=\"" ++ (encode v) ++ "\""  | (k, v) <- auth_base_sorted]

createRequest :: Bool -> String -> [(String, String)]
                      -> [(String, String)] -> TwitterKey -> IO Request
createRequest is_post url query_param post_param key = do
    auth_base <- createAuthBaseDefault key
    let param = query_param ++ post_param
    let signature = createSignature is_post url param auth_base $ createHmacKey key
    let auth_header = createAuthHeader auth_base signature
    execState (setRequest auth_header) <$> parseRequest url
    where
      setRequest :: String -> State Request ()
      setRequest auth_header = do
        let bs_query_param = [(CB.pack f, Just . CB.pack $ s) | (f, s) <- query_param]
        modify $ addRequestHeader "Authorization" $ CB.pack auth_header
        modify $ setRequestQueryString bs_query_param



