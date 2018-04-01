{-# LANGUAGE OverloadedStrings #-}
module Tweet where
import Data.Aeson.Lens
import Control.Lens
import Data.Aeson.Types (Value)
import Data.Text.Lens (unpacked)

getEventName :: AsValue a => a -> Maybe String
getEventName = (^? key "event" . _String . unpacked)

getEventTargetName :: AsValue a => a -> Maybe String
getEventTargetName = (^? key "target" . key "screen_name" . _String . unpacked)

getEventSourceId :: AsValue a => a -> Maybe String
getEventSourceId = (^? key "source" . key "id_str" . _String . unpacked)

isReplyToMe :: AsValue a => String -> a -> Bool
isReplyToMe me obj = case obj ^? key "in_reply_to_screen_name"  . _String . unpacked of
  Just name -> me == name
  Nothing -> False

getStatusId :: AsValue a => a -> Maybe String
getStatusId = (^? key "id_str" . _String . unpacked)

getScreenName :: AsValue a => a -> Maybe String
getScreenName = (^? key "user" . key "screen_name" . _String . unpacked)







