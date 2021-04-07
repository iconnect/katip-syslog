{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Katip.Scribes.Syslog
    ( mkSyslogScribe
    ) where


import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.Aeson (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.String.Conv
import qualified Data.Text as T
import           Katip.Core
import           System.Posix.Syslog

--------------------------------------------------------------------------------
-- | A syslog `Scribe` which respects the main Katip's guidelines.
-- Returns a tuple containing the `Scribe` and a finaliser.
mkSyslogScribe :: Namespace -> Severity -> Verbosity -> IO (Scribe, IO ())
mkSyslogScribe ns sev verb = do
  let identifier = T.intercalate "." (unNamespace ns)
  let cfg = defaultConfig {  identifier   = toS identifier
                           , options      = [PID, CONS, ODELAY, NDELAY]
                           , priorityMask = NoMask -- Katip does the masking for us.
                           }
#if (MIN_VERSION_katip(0,5,0))
  let scribe = Scribe (\i@Item{..} -> do
#if (MIN_VERSION_katip(0,8,0))
                          permit <- permitItem sev i
                          when permit $ do
#else
                          when (permitItem sev i) $ do
#endif
                            res <- try $ withSyslog cfg $ \syslog -> syslog USER (toSyslogPriority _itemSeverity) (toS $ formatItem verb i)
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
