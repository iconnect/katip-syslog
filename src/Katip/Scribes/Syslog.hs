{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Katip.Scribes.Syslog
    ( mkSyslogScribe
    ) where


import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.Aeson (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Unsafe ( unsafeUseAsCStringLen )
import qualified Data.ByteString as B
#if MIN_VERSION_bytestring(0,11,0)
import           Data.ByteString (toStrict)
#else
import           Data.ByteString.Lazy (toStrict)
#endif
import           Data.String.Conv
import           Foreign.C.String ( CStringLen, withCStringLen )
import           Katip.Core
import           System.Posix.Syslog
import qualified Data.Text as T

class LogMessage m where
  toCStringLen :: m -> (CStringLen -> IO a) -> IO a

instance LogMessage String where
  toCStringLen = withCStringLen

instance LogMessage B.ByteString where
  toCStringLen = unsafeUseAsCStringLen

write :: LogMessage a => Priority -> a -> IO ()
write pri msg = toCStringLen msg (syslog Nothing pri)

--------------------------------------------------------------------------------
-- | A syslog `Scribe` which respects the main Katip's guidelines.
-- Returns a tuple containing the `Scribe` and a finaliser.
mkSyslogScribe :: Namespace -> Severity -> Verbosity -> IO (Scribe, IO ())
mkSyslogScribe ns sev verb = do
  let identifier = T.intercalate "." (unNamespace ns)
#if (MIN_VERSION_katip(0,5,0))
  let scribe = Scribe (\i@Item{..} -> do
#if (MIN_VERSION_katip(0,8,0))
                          permit <- permitItem sev i
                          when permit $ do
#else
                          when (permitItem sev i) $ do
#endif
                            res <- try $ withSyslog (toS identifier) [LogPID, Console, DelayedOpen, ImmediateOpen] User $ write (toSyslogPriority _itemSeverity) (toStrict $ formatItem verb i)
                            case res of
                              Left (e :: SomeException) -> putStrLn (show e)
                              Right () -> return ())
#if (MIN_VERSION_katip(0,8,0))
                      (return ()) (permitItem sev)
#else
                      (return ())
#endif
#else
  let scribe = Scribe $ \ i@Item{..} -> do
                            when (permitItem sev i) $ do
                              res <- try $ withSyslog cfg $ \syslog -> syslog USER (toSyslogPriority _itemSeverity) (toS $ formatItem verb i)
                              case res of
                                Left (e :: SomeException) -> putStrLn (show e)
                                Right () -> return ()
#endif
  return (scribe, return ())

--------------------------------------------------------------------------------
-- | Syslog won't handle correctly things like newlines, so it's programmer's
-- responsibility to escape those.
formatItem :: LogItem a => Verbosity -> Item a -> ByteString
formatItem verb = encode . itemJson verb

--------------------------------------------------------------------------------
toSyslogPriority :: Severity -> Priority
toSyslogPriority DebugS      =  Debug
toSyslogPriority InfoS       =  Info
toSyslogPriority NoticeS     =  Notice
toSyslogPriority WarningS    =  Warning
toSyslogPriority ErrorS      =  Error
toSyslogPriority CriticalS   =  Critical
toSyslogPriority AlertS      =  Alert
toSyslogPriority EmergencyS  =  Emergency
