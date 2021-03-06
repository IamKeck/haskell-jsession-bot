module Handler where

import Control.Monad.Reader
import Data.Aeson.Types
import qualified Songs as S

import OAuth

data HandlerProp = HandlerProp {getData :: Value, getKey :: TwitterKey,
                                getSongBase :: S.SongBase}
type Handler = ReaderT HandlerProp IO ()

askData :: ReaderT HandlerProp IO (Value)
askData =  getData <$> ask

askKey :: ReaderT HandlerProp IO (TwitterKey)
askKey =  getKey <$> ask

askSongBase :: ReaderT HandlerProp IO (S.SongBase)
askSongBase =  getSongBase <$> ask

