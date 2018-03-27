{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import OAuth
import Songs

consumerSecret = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw"
tokenSecret = "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE"
nonce = "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg"
timestamp = "1318622958"
token = "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"
consumer = "xvz1evFS4wEEPTGEFPHBog"
key = TwitterKey consumer Main.consumerSecret token tokenSecret
param = [ ("status", "Hello Ladies + Gentlemen, a signed OAuth request!"),
          ("include_entities", "true")
        ]
url = "https://api.twitter.com/1/statuses/update.json"
signature = "tnnArxj06cWHq44gCs1OSKk/jLY="

authHeader = "OAuth oauth_consumer_key=\"xvz1evFS4wEEPTGEFPHBog\", \
             \oauth_nonce=\"kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg\", \
             \oauth_signature=\"tnnArxj06cWHq44gCs1OSKk%2FjLY%3D\", \
             \oauth_signature_method=\"HMAC-SHA1\", \
             \oauth_timestamp=\"1318622958\", \
             \oauth_token=\"370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb\", \
             \oauth_version=\"1.0\""

main :: IO ()
main = hspec $ do
  describe "createHmacKey" $
    it "standard" $
      createHmacKey key `shouldBe` "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw&LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE"
  describe "createSignature" $
    it "standard" $
      let
        hmac_key = createHmacKey key
        auth_base = createAuthBase key nonce timestamp
      in
        createSignature True url param auth_base hmac_key `shouldBe` signature
  describe "createAuthHeader" $
    it "standard" $
      let
        hmac_key = createHmacKey key
        auth_base = createAuthBase key nonce timestamp
      in
        createAuthHeader auth_base signature `shouldBe` authHeader
  describe "createSongs" $
    it "standard" $
      let
        songs_byte = "[{\"name\": \"songA\", \"rate\": 1}, {\"name\": \"songB\", \"rate\": 2}]"
        songs_ans = Right [Song "songA" 1, Song "songB" 2]
      in
        createSongs songs_byte `shouldBe` songs_ans

