{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Alfred.Types where

import qualified Data.Text                     as T
import           Data.Aeson
import           System.FilePath
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           GHC.Generics





-- | The Return to alfred
data AlfredReturn = AlfredReturn { items :: [ReturnItem]
                               , rerun :: Float             -- ^ A timer 0.1 to 5.0 sec that makes the script rerun
                               , globalVars :: Vars         -- ^ These are overwritten by any item variables with the same key
                               } deriving (Show,Generic)

instance ToJSON AlfredReturn where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = modF }
    where modF "globalVars" = "variables"
          modF field = field

data ReturnItem = ReturnItem { uid :: Maybe T.Text          -- ^ used for sorting and ordering of actioned results.
                             , title :: T.Text              -- ^ The title displayed in the result row.
                             , subtitle :: Maybe T.Text     -- ^ The subtitle displayed in the result row.
                             , arg :: Maybe T.Text          -- ^ The argument which is passed through the workflow to the output.
                             , icon :: Icon                 -- ^ The icon displayed in the result row.
                             , valid :: Maybe Bool          -- ^ Unvalid item wont be acted upon.
                             , match :: Maybe T.Text        -- ^ What Alfred matches against when "Filters Results" is set
                             , autocomplete :: Maybe T.Text -- ^ Autocomplete target
                             , retType :: RetType
                             , mods :: [Mod]
                             , text :: Maybe RetText
                             , quicklook :: Maybe T.Text    -- ^  Quick Look : tapping shift, or cmd+y
                             , itemVars :: Maybe Vars
                             } deriving (Show, Generic)

instance ToJSON ReturnItem where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = modF }
    where modF "retType" = "type"
          modF "itemVars" = "variables"
          modF field = field

-- | By specifying FileRet, makes Alfred treat your result as a file on your system.
-- This allows the user to perform actions on the file like they can with Alfred's standard file filters.
data RetType =  DefaultRet | FileRet | FileRetSkip deriving (Show)

instance ToJSON RetType where
  toJSON rt = String $ case rt of DefaultRet -> "default"
                                  FileRet -> "file"
                                  FileRetSkip -> "file:skipcheck"


-- | Modifier Keys
data ModKey = Alt | Cmd deriving Show

modkeyText :: ModKey -> T.Text
modkeyText Alt = "alt"
modkeyText Cmd = "cmd"

-- | The mod element gives you control over how the modifier keys react.
-- You can now define the valid attribute to mark if the result is valid
-- based on the modifier selection
-- and set a different arg to be passed out if actioned with the modifier.
data Mod = Mod ModKey ReturnItem deriving Show

instance ToJSON Mod where
  toJSON (Mod k ri) = object [ modkeyText k .= toJSON ri]

-- | The text element defines the text the user will get
-- when copying the selected result row with cmd-C
-- or displaying large type with cmd-L.
data RetText = RetText { copy :: Maybe T.Text
                       , largetype :: Maybe T.Text
                       } deriving (Show, Generic)

instance ToJSON RetText

-- | Variables passed out of the script
-- Item Variables overide all global if present
newtype Vars = Vars (Map T.Text T.Text) deriving (Show, Generic)

instance ToJSON Vars

type UseIconFromFile = Bool

-- | An icon in the filepath, or the image file itself
data Icon = Icon UseIconFromFile FilePath deriving Show

instance ToJSON Icon where
  toJSON (Icon i fp) = object (("path" .= fp) : [ "type" .= ("fileicon" :: T.Text) | i ])


-- | Data passed in as envVars
data AlfEnvVars = AlfEnvVars { alfWorkflowCache :: Maybe FilePath -- ^ Temporary file location, BundleID Must be set
                             , alfWorkflowData :: Maybe FilePath -- ^ Permanent file location, BundleID Must be set
                             , alfWorkflowName :: String
                             }

