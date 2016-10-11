{-# LANGUAGE ForeignFunctionInterface #-}
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
data PjSuaConfig
type PjStatus = CInt

foreign import ccall "create_pj_str" createPjString
    :: CString
    -> IO (Ptr PjString)
foreign import ccall "delete_pj_str" deletePjString
    :: Ptr PjString -> IO ()

foreign import ccall "pjsua_create" createPjSua :: IO PjStatus
foreign import ccall "pjsua_destroy" destroyPjSua :: IO PjStatus

-- {{{ pj configuration -------------------------------------------------------

foreign import ccall "create_pj_config" createPjConfig :: IO (Ptr PjSuaConfig)
foreign import ccall "pjsua_config_default" defaultPjConfig
    :: Ptr PjSuaConfig
    -> IO ()


-- }}} pj configuration -------------------------------------------------------
-- {{{ pj logging configuration -----------------------------------------------

data LoggingConfig

foreign import ccall "create_pjsua_logging_config" createLoggingConfig
    :: IO (Ptr LoggingConfig)
foreign import ccall "pjsua_logging_config_default" defaultLoggingConfig
    :: Ptr LoggingConfig
    -> IO ()

-- }}} pj logging configuration -----------------------------------------------
-- {{{ pj media configuration -------------------------------------------------

data MediaConfig

foreign import ccall "create_pjsua_media_config" createMediaConfig
    :: IO (Ptr MediaConfig)
foreign import ccall "pjsua_media_config_default" defaultMedaiConfig
    :: Ptr MediaConfig
    -> IO ()

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
    :: OnIncomingCallHandler
    -> IO (FunPtr OnIncomingCallHandler)

foreign import ccall "pjsua_set_on_incoming_call" setOnIncomingCallCallback
    :: Ptr PjSuaConfig -> FunPtr OnIncomingCallHandler -> IO ()

type OnRegistrationStateHandler = AccountId -> IO ()

foreign import ccall safe "wrapper" toOnRegistrationState
    :: OnRegistrationStateHandler
    -> IO (FunPtr OnRegistrationStateHandler)

foreign import ccall "pjsua_set_on_incoming_call" setOnRegistrationStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnRegistrationStateHandler -> IO ()

type OnMediaStateHandler = CallId -> IO ()

foreign import ccall safe "wrapper" toOnMediaState
    :: OnMediaStateHandler
    -> IO (FunPtr OnMediaStateHandler)

foreign import ccall "pjsua_set_on_media_state" setOnMediaStateCallback
    :: Ptr PjSuaConfig -> FunPtr OnMediaStateHandler -> IO ()


data Reason
data MsgData

foreign import ccall "pjsua_call_answer" callAnswer
    :: CallId -> CUInt -> Ptr Reason -> Ptr MsgData -> IO PjStatus

-- pj_status_t pjsua_call_answer    (   pjsua_call_id   call_id,
--        unsigned    code,
--        const pj_str_t *    reason,
--        const pjsua_msg_data *      msg_data
--    )

foreign import ccall "pjsua_call_hangup" callHangup
    :: CallId
    -> CUInt
    -> Ptr Reason
    -> Ptr MsgData
    -> IO PjStatus

-- pj_status_t pjsua_call_hangup   (   pjsua_call_id   call_id,
--         unsigned    code,
--         const pj_str_t *    reason,
--         const pjsua_msg_data *      msg_data
--     )

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

foreign import ccall "pjsua_acc_add" setAccount
    :: (Ptr AccountConfig) -> CInt -> Ptr AccountId -> IO PjStatus
-- status = pjsua_acc_add(&cfg, PJ_TRUE, &acc_id);


foreign import ccall "pjsua_verify_sip_url" verifySipUrl
    :: CString -> IO PjStatus

foreign import ccall "pjsua_verify_url" verifyTelUrl
    :: CString -> IO PjStatus

data CallSetting
data UserData

foreign import ccall "pjsua_call_make_call" makeCall
    :: AccountId
    -> Ptr PjString
    -> Ptr CallSetting
    -> Ptr UserData
    -> Ptr MsgData
    -> Ptr CallId
    -> IO PjStatus
-- pj_status_t pjsua_call_make_call     (   pjsua_acc_id    acc_id,
--        const pj_str_t *    dst_uri,
--        const pjsua_call_setting *      opt,
--        void *      user_data,
--        const pjsua_msg_data *      msg_data,
--        pjsua_call_id *     p_call_id
--    )

foreign import ccall "pjsua_call_hangup_all" hanhupAll :: IO ()

foreign import ccall "pjsua_set_null_snd_dev" setNullSndDev :: IO ()
foreign import ccall "printDevices" printDevices :: IO ()
