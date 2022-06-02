{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Email
  ( StatusMessage
  , createAndSendTestEmail
  ) where

import           Control.Exception.Extra (try_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.Mail.Mime (Address (..), Mail (..), simpleMail')
import qualified Network.Mail.SMTP as SMTP

import           Cardano.Tracer.Handlers.RTView.UI.Types

type StatusMessage = Text

createAndSendTestEmail
  :: EmailSettings
  -> IO StatusMessage
createAndSendTestEmail settings@EmailSettings {esEmailTo, esEmailFrom, esSubject} =
  sendEmail settings $ simpleMail' to from esSubject body
 where
  to   = Address Nothing esEmailTo
  from = Address (Just "Cardano RTView") esEmailFrom
  body = "This is a test notification from Cardano RTView. Congrats: your email settings are correct!"

sendEmail
  :: EmailSettings
  -> Mail
  -> IO StatusMessage
sendEmail EmailSettings {esEmailTo, esSMTPHost, esSMTPPort, esUsername, esPassword, esSSL} mail =
  try_ (sender host port user pass mail) >>= \case
    Left e  -> return $ "Unable to send email: " <> T.pack (show e)
    Right _ -> return $ "Yay! Notification to " <> esEmailTo <> " sent."
 where
  sender = case esSSL of
             TLS      -> SMTP.sendMailWithLoginTLS'
             STARTTLS -> SMTP.sendMailWithLoginSTARTTLS'
             NoSSL    -> SMTP.sendMailWithLogin'
  host = T.unpack esSMTPHost
  port = fromIntegral esSMTPPort
  user = T.unpack esUsername
  pass = T.unpack esPassword
