{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Alfred () where

import           Alfred.Types
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Aeson as J
import qualified Data.Aeson                    as JSON
import qualified Data.ByteString.Base64        as BASE
                                                ( encode
                                                , decode
                                                )
import qualified Data.ByteString.Lazy          as BS
import qualified Data.ByteString.Char8          as BSC
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.Environment           (lookupEnv, getArgs)
import System.Exit (die)

class AlfStatable s where
  putState :: s -> T.Text         -- ^ Used for seriazing the state and send to the output
  getState :: T.Text -> Maybe s    -- ^ Used for recovering the state from previous runs 
  emptyState :: s

envStateKey :: String
envStateKey = "AlfredState"

type Args = [String]

data (AlfStatable s) => AlfState s = AlfState { alfState :: s
                                            , query :: Args
                                            }


newtype AlfM s a = AlfM {runAlf :: StateT (AlfState s) (ExceptT AlfredError IO) a }
  deriving (Functor, Monad, Applicative)

initialState :: AlfStatable s => AlfM s ()
initialState = AlfM $ do
  -- Arguments
  argsS        <- liftIO getArgs

  -- Passed in state via arguments
  mStateString <- liftIO $ lookupEnv envStateKey
  let stateString =
        fmap (getState . decodeUtf8) . BASE.decode . BSC.pack <$> mStateString
  case stateString of
    Nothing                  -> put $ AlfState emptyState argsS
    (Just (Left  e        )) -> lift $ throwE $ Base64DecodingError $ "Failed to decode base64 string from env variable: " <> e
    (Just (Right Nothing  )) -> lift $ throwE $ StateParsingError "Failed to parse state from env variable"
    (Just (Right (Just as))) -> put $ AlfState as argsS
  return ()

writeStateToReturn :: AlfStatable s => AlfM s AlfredReturn
writeStateToReturn = AlfM $ do
  s <- get
  return undefined

alfMain :: AlfStatable s => AlfM s AlfredReturn -> IO ()
alfMain ops =
  let run = runAlf $ do
        initialState
        ops
        writeStateToReturn
      putRet (Left  e) = die $ show e
      putRet (Right r) = BS.putStr $ J.encode r
  in  do
        res <- runExceptT
          $ evalStateT run (AlfState (emptyState :: AlfStatable s => s) [])
        putRet res








getWorkflowName :: IO (Either AlfredError String)
getWorkflowName = lookupEnv "alfred_workflow_name" >>= \case
  Just n  -> return $ Right n
  Nothing -> return $ Left $ EnvVarError
    "Failed to get env variable \" alfred_workflow_name\""

getWorkflowCache :: IO (Either AlfredError FilePath)
getWorkflowCache = lookupEnv "alfred_workflow_cache" >>= \case
  Just n  -> return $ Right n
  Nothing -> return $ Left $ EnvVarError
    "Failed to get env variable \"alfred_workflow_cache\" \n is workflow uid set?"

getWorkflowData :: IO (Either AlfredError FilePath)
getWorkflowData = lookupEnv "alfred_workflow_data" >>= \case
  Just n  -> return $ Right n
  Nothing -> return $ Left $ EnvVarError
    "Failed to get env variable \"alfred_workflow_cache\" \n is workflow uid set?"

data AlfredError = StateParsingError String |
                   Base64DecodingError String |
                   EnvVarError String |
                   OtherError String
                   deriving Show
