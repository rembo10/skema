{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common types for the notification system.
module Skema.Notifications.Types
  ( Notification(..)
  , NotificationSender(..)
  ) where

import GHC.Generics (Generic)

-- | A notification to be sent.
data Notification = Notification
  { notificationTitle :: Text
    -- ^ Notification title
  , notificationMessage :: Text
    -- ^ Notification message body
  , notificationUrl :: Maybe Text
    -- ^ Optional URL to open when notification is clicked
  } deriving (Show, Eq, Generic)

-- | Interface for notification providers.
--
-- Each provider (Pushover, etc.) implements this interface
-- to send notifications through their specific API.
class NotificationSender provider where
  -- | Send a notification through the provider.
  --
  -- Returns either an error message or success.
  sendNotification :: provider -> Notification -> IO (Either Text ())
