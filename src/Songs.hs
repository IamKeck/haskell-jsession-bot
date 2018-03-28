{-# LANGUAGE DeriveGeneric #-}

module Songs where

import GHC.Generics
import Data.ByteString as B (ByteString)
import Data.Aeson
import System.Random
import Data.Char (toUpper)

data Song = Song {name :: String, rate :: Int } deriving (Show, Eq, Generic)
type SongBase = [String]

instance FromJSON Song

createSongs :: B.ByteString -> Either String [Song]
createSongs = eitherDecodeStrict

createSongBase :: [Song] -> SongBase
createSongBase = foldr inner [] where
  inner song acc = acc ++ replicate (rate song) (name song)


chooseSong :: SongBase -> StdGen -> String
chooseSong songBase = map toUpper . (songBase !!) . fst .  randomR (0, length songBase)

chooseSongIO :: SongBase -> IO String
chooseSongIO songBase = chooseSong songBase <$> newStdGen

