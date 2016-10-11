{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module:       $HEADER$
-- Description:  Low level FFI.
-- Copyright:
-- License:      GPL-2
--
-- Maintainer:   Jan Sipr <jan.sipr@ixperta.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
module Phone.Internal.FFI
  where

-- GHC lower than 8.0 don't have alignment macro.
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- This allows to retrieve value from enums and defines
-- Pjsua uses extremely tricky enums...
#let enumToValue t = "%d", (int)t

#include <pjsua-lib/pjsua.h>

import Control.Monad
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Text.Show (Show)

-- | Equivalent to PJ_TRUE
pjTrue :: CInt
pjTrue = #{enumToValue PJ_TRUE}

-- | Equivalent to PJ_FALSE
pjFalse :: CInt
pjFalse = #{enumToValue PJ_FALSE}

-- | Equivalent to PJ_SUCCESS
pjSuccess :: CInt
pjSuccess = #{enumToValue PJ_SUCCESS}

data PjString
type PjStatus = CInt

foreign import ccall "create_pj_str" createPjString
    :: CString -> IO (Ptr PjString)
foreign import ccall "delete_pj_str" deletePjString
    :: Ptr PjString -> IO ()

-- | Calls createPjSua which load the pjsua library in to memory.
--
-- __Must be called before any another hsua (pjsua) function is called.__
foreign import ccall "pjsua_create" createPjSua :: IO PjStatus

-- | Opposite function to 'createPjSua' function. It destroys the hsua (pjsua) library
-- memory representaiton.
--
-- __No hsua (pjsua) function may be called after this function.__
foreign import ccall "pjsua_destroy" destroyPjSua :: IO PjStatus

-- {{{ pj configuration -------------------------------------------------------

data PjSuaConfig

foreign import ccall "create_pj_config" createPjConfig :: IO (Ptr PjSuaConfig)
foreign import ccall "pjsua_config_default" defaultPjConfig
    :: Ptr PjSuaConfig -> IO ()

-- }}} pj configuration -------------------------------------------------------
-- {{{ pj logging configuration -----------------------------------------------

data LoggingConfig

foreign import ccall "create_pjsua_logging_config" createLoggingConfig
    :: IO (Ptr LoggingConfig)
foreign import ccall "pjsua_logging_config_default" defaultLoggingConfig
    :: Ptr LoggingConfig -> IO ()

-- }}} pj logging configuration -----------------------------------------------
-- {{{ pj media configuration -------------------------------------------------

data MediaConfig

foreign import ccall "create_pjsua_media_config" createMediaConfig
    :: IO (Ptr MediaConfig)
foreign import ccall "pjsua_media_config_default" defaultMedaiConfig
    :: Ptr MediaConfig -> IO ()

-- }}} pj media configuration -------------------------------------------------

-- | Looks like the LoggingConfig and MediaConfig may be null
foreign import ccall "pjsua_init" initializePjSua
    :: Ptr PjSuaConfig -> Ptr LoggingConfig -> Ptr MediaConfig -> IO PjStatus

data SipEvent
type CallId = CInt
type OnCallStateHandler = CallId -> Ptr SipEvent -> IO ()

foreign import ccall safe "wrapper"
    toOnCallState :: OnCallStateHandler -> IO (FunPtr OnCallStateHandler)

foreign import ccall "pjsua_set_on_call_state" setOnCallStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnCallStateHandler -> IO ()

type AccountId = CInt
data RxData

type OnIncomingCallHandler = AccountId -> CallId -> Ptr RxData -> IO ()

foreign import ccall safe "wrapper" toOnIncomingCall
    :: OnIncomingCallHandler -> IO (FunPtr OnIncomingCallHandler)

foreign import ccall "pjsua_set_on_incoming_call" setOnIncomingCallCallback
    :: Ptr PjSuaConfig -> FunPtr OnIncomingCallHandler -> IO ()

type OnRegistrationStateHandler = AccountId -> IO ()

foreign import ccall safe "wrapper" toOnRegistrationState
    :: OnRegistrationStateHandler -> IO (FunPtr OnRegistrationStateHandler)

foreign import ccall "pjsua_set_on_incoming_call" setOnRegistrationStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnRegistrationStateHandler -> IO ()

type OnMediaStateHandler = CallId -> IO ()

foreign import ccall safe "wrapper" toOnMediaState
    :: OnMediaStateHandler -> IO (FunPtr OnMediaStateHandler)

foreign import ccall "pjsua_set_on_media_state" setOnMediaStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnMediaStateHandler -> IO ()


data Reason
data MsgData

-- | Send response to incoming INVITE request with call setting param.
-- Depending on the status code specified as parameter, this function may send
-- provisional response, establish the call, or terminate the call. Notes about
-- call setting:
--
--  * if call setting is changed in the subsequent call to this function, only
--    the first call setting supplied will applied. So normally application
--    will not supply call setting before getting confirmation from the user.
--  * if no call setting is supplied when SDP has to be sent, i.e: answer with
--    status code 183 or 2xx, the default call setting will be used, check
--    pjsua_call_setting for its default values.
foreign import ccall "pjsua_call_answer" callAnswer
    :: CallId
    -> CUInt
    -- ^ Status code to be used to answer the call.
    -> Ptr Reason
    -- ^ Optional reason phrase which will be find into SIP header. If null,
    -- the default phrase will be used.
    -> Ptr MsgData
    -- ^ Optional list of headers to be added to SIP msg.
    -> IO PjStatus

-- | Hangup call by using method that is appropriate according to the call
-- state. This function is different than answering the call with 3xx-6xx
-- response (with pjsua_call_answer()), in that this function will hangup the
-- call regardless of the state and role of the call, while pjsua_call_answer()
-- only works with incoming calls on EARLY state.
foreign import ccall "pjsua_call_hangup" callHangup
    :: CallId
    -> CUInt -- ^ Status code to be used to hangup the call.
    -> Ptr Reason -- ^ Optional reason phrase which will be find into SIP
                  --   header. If null, the default phrase will be used.
    -> Ptr MsgData -- ^ Optional list of headers to be added to SIP msg.
    -> IO PjStatus

data TransportConfig
data TransportId

foreign import ccall "create_pjsua_transport_config" createTransportConfig
    :: IO (Ptr TransportConfig)

foreign import ccall "pjsua_transport_config_set_port" setPort
    :: Ptr TransportConfig -> CUInt -> IO ()

udpTransport :: CInt
udpTransport = #{enumToValue PJSIP_TRANSPORT_UDP}

tcpTransport :: CInt
tcpTransport = #{enumToValue PJSIP_TRANSPORT_TCP}

foreign import ccall "pjsua_transport_create" createTransport
    :: CInt -> Ptr TransportConfig -> Ptr TransportId -> IO PjStatus

-- data TransportType =
--     Unspecified
--     | UDP
--     | TCP
--     | TLS
--     | SCTP
--     | LOOP
--     | LOOP_DGRAM
--     | START_OTHER
--     | IPV6
--     | UDP6
--     | TCP6
--     | TLS6
--   deriving (Show, Eq)
--
-- instance Enum TransportType where
--   fromEnum Unspecified = 0
--   fromEnum UDP = 1
--   fromEnum TCP = 2
--
--   toEnum 0 = PlainKey
--   toEnum 1 = SpecialKey
--   toEnum 2 = NoKey
--   toEnum unmatched = error ("Key.toEnum: Cannot match " ++ show unmatched)

foreign import ccall "pjsua_start" pjsuaStart :: IO PjStatus

data AccountConfig

foreign import ccall "crete_pjsua_acc_config" createAccountConfig
    :: IO (Ptr AccountConfig)

foreign import ccall "pjsua_acc_config_default" defaultAccountConfig
    :: (Ptr AccountConfig) -> IO ()

foreign import ccall "setAccountId" setAccoutId
    :: (Ptr AccountConfig) -> Ptr PjString -> IO ()

foreign import ccall "setAccountRegUri" setAccountRegUri
    :: (Ptr AccountConfig) -> Ptr PjString -> IO ()

foreign import ccall "setAccountCredCount" setAccountCredCount
    :: (Ptr AccountConfig) -> CInt -> IO ()

foreign import ccall "setAccountRealm" setAccountRealm
    :: (Ptr AccountConfig) -> CInt -> Ptr PjString -> IO ()

foreign import ccall "setAccountSchema" setAccountSchema
    :: (Ptr AccountConfig) -> CInt -> Ptr PjString -> IO ()

foreign import ccall "setAccountUsername" setAccountUsername
    :: (Ptr AccountConfig) -> CInt -> Ptr PjString -> IO ()

credDataPlainPasswd :: CInt
credDataPlainPasswd = #{enumToValue PJSIP_CRED_DATA_PLAIN_PASSWD}

foreign import ccall "setAccountDataType" setAccountDataType
    :: (Ptr AccountConfig) -> CInt -> CInt -> IO ()

foreign import ccall "setAccountData" setAccountData
    :: (Ptr AccountConfig) -> CInt -> Ptr PjString -> IO ()

-- | Add a new account to hsua (pjsua). If registration is configured for this
-- account, this function would also start the SIP registration session with
-- the SIP registrar server. This SIP registration session will be maintained
-- internally by the library, and application doesn't need to do anything to
-- maintain the registration session.
foreign import ccall "pjsua_acc_add" setAccount
    :: Ptr AccountConfig
    -- ^ Account configuration.
    -> CInt
    -- ^ If non-zero, this account will be set as the default account. The
    -- default account will be used when sending outgoing requests
    -- (e.g. making call) when no account is specified, and when receiving
    -- incoming requests when the request does not match any accounts. It is
    -- recommended that default account is set to local/LAN account.
    -> Ptr AccountId
    -- ^ Pointer to receive account ID of the new account.
    -> IO PjStatus

foreign import ccall "pjsua_verify_sip_url" verifySipUrl
    :: CString -> IO PjStatus

foreign import ccall "pjsua_verify_url" verifyTelUrl
    :: CString -> IO PjStatus

data CallSetting
data UserData

-- | Make call to specified URI.
foreign import ccall "pjsua_call_make_call" makeCall
    :: AccountId
    -- ^ Account id for account we want to originate the call.
    -> Ptr PjString
    -- ^ Destination URI in format \"sip:(name/number)@address.com\".
    -> Ptr CallSetting
    -- ^ Optional ('nullPtr') call settings.
    -> Ptr UserData
    -- ^ Optional ('nullPtr') arbitrary user data to be attached to the call,
    -- and can be retrieved later.
    -> Ptr MsgData
    -- ^ Optional ('nullPtr') headers to be added to SIP msg.
    -> Ptr CallId
    -- ^ Optional ('nullPtr') Pointer to CallId where the call id will be
    -- stored.
    -> IO PjStatus

-- | Terminate (end) all calls. In other words this will call 'callHangup' to
-- all currently active calls.
foreign import ccall "pjsua_call_hangup_all" hanhupAll :: IO ()

foreign import ccall "pjsua_set_null_snd_dev" setNullSndDev :: IO ()
foreign import ccall "printDevices" printDevices :: IO ()
