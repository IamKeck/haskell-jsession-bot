{-# LANGUAGE DeriveGeneric #-}

module Songs where

import GHC.Generics
import Data.ByteString as B
import Data.Aeson

data Song = Song {name :: String, rate :: Int } deriving (Show, Eq, Generic)

instance FromJSON Song

createSongs :: B.ByteString -> Either String [Song]
createSongs = eitherDecodeStrict

