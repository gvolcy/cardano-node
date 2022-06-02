{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Email
  ( createAndSendTestEmail
  ) where

import           Control.Exception.Extra (try_)
import qualified Network.Mail.SMTP as SMTP
import           Network.Mail.Mime (Address (..), Mail (..), simpleMail')
import           Data.Text (Text)
import qualified Data.Text as T

import           Cardano.Tracer.Handlers.RTView.UI.Types

type StatusMessage = Text

createAndSendTestEmail
  :: EmailSettings
  -> IO StatusMessage
createAndSendTestEmail settings =
  sendEmail settings $ simpleMail' to from subject body
 where
  to      = Address Nothing (esEmailTo settings)
  from    = Address (Just "Cardano RTView") (esEmailFrom settings)
  subject = esSubject settings
  body    = "This is a test notification from Cardano RTView. Congrats: your email settings are correct!"

sendEmail
  :: EmailSettings
  -> Mail
  -> IO StatusMessage
sendEmail settings mail =
  try_ (sender host port user pass mail) >>= \case
    Left e  -> return $ "Unable to send email: " <> T.pack (show e)
    Right _ -> return $ "Yay! Email notification to " <> esEmailTo settings <> " sent."
 where
  sender = case esSSL settings of
             TLS      -> SMTP.sendMailWithLoginTLS'
             STARTTLS -> SMTP.sendMailWithLoginSTARTTLS'
             NoSSL    -> SMTP.sendMailWithLogin'
  host = T.unpack (esSMTPHost settings)
  port = fromIntegral (esSMTPPort settings)
  user = T.unpack (esUsername settings)
  pass = T.unpack (esPassword settings)
