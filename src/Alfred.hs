{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module helps to write Alfred script filter indeded to be embedded in workflows.
--  It also provides a persistent state on disk between runs.
--  
--  @
-- import Alfred
-- import Data.ByteString
-- import Data.Time.LocalTime
--
-- newtype MyState = MyState String
--
-- instance AlfStatable MyState where
--   encodeState (MyState s) = fromString s
--   decodeState = Right . MyState . toString
--   defaultState = MyState []
--
-- myReturn :: AlfM MyState Return
-- myReturn = do
--   time <- liftIO getZonedTime
--   (MyState prevTime) <- get
--   put (MyState $ show time)
--   return $ defaultReturn
--     { items = [ defaultItem { title    = \"Time\"
--                             , subtitle = Just "Outputs the last time the scipt was run"
--                             , arg      = Just $ show prevTime }]}
--
-- main = alfMain myReturn
--  @

module Alfred
  ( AlfStatable(..)
  , AlfM
  , Args
  , alfMain
  , alfArgs
  , envVariable
  , envVariableThrow
  , workflowCacheDir
  , workflowDataDir
  , AlfredError(..)
  , throwAlfE
  , module Alfred.Types
  , put
  , get
  , liftIO
  )
where

import           Alfred.Types
import           Control.Exception.Lifted
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Control.Monad                  ( when )
import           Data.Aeson                     ( encode )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
                                                ( writeFile
                                                , readFile
                                                , empty
                                                )
import qualified Data.ByteString.Lazy          as LBS
                                                ( putStr )
import           Data.Maybe
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                )
import           System.Environment             ( getArgs
                                                , lookupEnv
                                                , getProgName
                                                )
import           System.Exit                    ( die )

-- | Class of the persistent user supplied state data type, 
-- serialized and stored on disk between runs.
-- `()` can be used if state is not required
class AlfStatable s where
  encodeState :: s -> ByteString
  decodeState :: ByteString -> Either AlfredError s
  defaultState :: s  -- ^ The state is initialized at first run

instance AlfStatable ByteString where
  encodeState = id
  decodeState = Right 
  defaultState = BS.empty

instance AlfStatable () where
  encodeState = const BS.empty
  decodeState = const $Right ()
  defaultState = ()

-- | The arguments passed from Alfred
type Args = [String]

-- | The main monad
type AlfM s =  StateT s (ExceptT AlfredError IO)

-- | Loads the state from disk, reads the arguments,
--   processes your operations, saves the state. Then
--   writes any errors to stderr otherwise output the return
alfMain :: AlfStatable s => AlfM s Return -> IO ()
alfMain ops =
  runExceptT (evalStateT runIt (defaultState :: AlfStatable s => s))
    >>= either (die . show) (LBS.putStr . encode)
 where
  runIt = do
    mustRunFromAlfred
    mustHaveBundleID
    loadState
    r <- ops
    saveState
    return r

-- | The file where state is saved between runs
stateFp :: AlfM s FilePath
stateFp = do
  dataDir <- workflowDataDir
  u       <- envVariableThrow "alfred_workflow_uid"
  return $ dataDir <> "/AlfredState." <> u

-- | Loads a state from disk if there is one otherwise puts an empty state
loadState :: AlfStatable s => AlfM s ()
loadState = do
  file   <- stateFp
  exists <- liftIO $ doesFileExist file
  if not exists
    then put (defaultState :: AlfStatable s => s)
    else do
      file' <- handle alfIOerrHand $ liftIO $ BS.readFile file
      case decodeState file' of
        (Left  err     ) -> throwAlfE err
        (Right newState) -> put newState

-- | Save state to disk for next run
saveState :: AlfStatable s => AlfM s ()
saveState = do
  st   <- get
  file <- stateFp
  handle alfIOerrHand $ liftIO $ BS.writeFile file $ encodeState st


-- | The input from Alfred
alfArgs :: AlfM s Args
alfArgs = liftIO getArgs

-- | Environment varibles that can be passed to the script.
--
-- Some variables may be passed as default, see <https://www.alfredapp.com/help/workflows/script-environment-variables/>
envVariable :: String -> AlfM s (Maybe String)
envVariable varName = liftIO $ lookupEnv varName

-- | Version of 'envVariable' that throws a 'EnvVarError' if the variable is not present
envVariableThrow :: String -> AlfM s String
envVariableThrow varName = do
  mVar <- liftIO $ lookupEnv varName
  case mVar of
    Just v  -> return v
    Nothing -> throwAlfE $ EnvVarError $ "No environment variable named: " <> varName

-- | The directory to store volatile data, is created upon request
workflowCacheDir :: AlfM s FilePath
workflowCacheDir = do
  path <- envVariableThrow "alfred_workflow_cache"
  handle alfIOerrHand $ liftIO $ createDirectoryIfMissing True path
  return path

-- | The directory to store permanent data, is created upon request
--
-- The state is stored here between runs
workflowDataDir :: AlfM s FilePath
workflowDataDir = do
  path <- envVariableThrow "alfred_workflow_data"
  handle alfIOerrHand $ liftIO $ createDirectoryIfMissing True path
  return path

-- | Exits with error if run from outside Alfred
mustRunFromAlfred :: AlfM s ()
mustRunFromAlfred = liftIO $ do
    v <- lookupEnv "alfred_version"
    n <- getProgName
    when (isNothing v) $ die $ n <> " must be run from Alfred."

-- | Exit with error if bundle ID if not set
mustHaveBundleID :: AlfM s ()
mustHaveBundleID = liftIO $ do
    v <- lookupEnv "alfred_workflow_bundleid"
    when (isNothing v) $ die "Bundle Id must be set for the workflow"

-- | Custom errors
data AlfredError
  = StateUnserializingError String -- ^ Error when decoding saved state
  | EnvVarError String             -- ^ Error when a requested environment variable is not present
  | FileOperationError String      -- ^ IOErrors lifted
  | OtherError String              -- ^ For use in scripts
  deriving (Show)

-- | Convenience for throwing a 'AlfredError'
throwAlfE :: AlfredError -> AlfM s a
throwAlfE = lift .throwE

-- | Handler for IOErrors
alfIOerrHand :: IOError -> AlfM s a
alfIOerrHand = throwAlfE . FileOperationError . show
