{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Alfred.Types where

import           Data.Aeson
import           Data.Map.Strict                ( Map
                                                , empty
                                                )
import           GHC.Generics

-- | Represents the output from the script
data Return = Return
  { items :: [Item]          -- ^ The possible outputs, i.e. items in alfred.
  , rerun :: Maybe Float     -- ^ \0.1 to 5.0 sec.
  , retVars :: Vars          -- ^ Passed out from the script.
  } deriving (Show, Generic)

instance ToJSON Return where
  toJSON =
    genericToJSON $
      defaultOptions {fieldLabelModifier = modF, omitNothingFields = True, sumEncoding = UntaggedValue}
    where
      modF "retVars" = "variables"
      modF field = field

-- | Constructor:
--
-- @
-- myReturn = defaultReturn { items = myitems
--                          , retVars = myvars }
-- @
defaultReturn :: Return
defaultReturn =
  Return { items = [defaultItem], rerun = Nothing, retVars = empty }

-- | Represents one item displayed by Alfred
data Item = Item
  { uid :: Maybe String          -- ^ Used for sorting and ordering of actioned results.
  , title :: String              -- ^ The title of the item.
  , subtitle :: Maybe String     -- ^ The subtitle of the item.
  , arg :: Maybe String          -- ^ Main output from the script.
  , icon :: Maybe Icon           -- ^ The icon for the item.
  , valid :: Bool                -- ^ Unvalid item won't be acted upon.
  , match :: Maybe String        -- ^ What Alfred matches against when "Filters Results" is on
  , autocomplete :: Maybe String -- ^ Autocomplete target.
  , retType :: RetType
  , mods :: Mods
  , text :: Maybe RetText
  , quicklookurl :: Maybe String -- ^ Quick Look cmd+y, can be file path.
  , itemVars :: Vars             -- ^ Item variables, any with the same key overrides 'retVars'.
  } deriving (Show, Generic)

instance ToJSON Item where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = modF
                                          , omitNothingFields = True }
    where modF "retType" = "type"
          modF "itemVars" = "variables"
          modF field = field

-- | Constructor:
--
-- @
-- myItem = defaultItem { uid      = "123abc"
--                      , title    = "Im a item"
--                      , subtitle = "Im a subtitle" }
-- @
defaultItem :: Item
defaultItem = Item
  { uid          = Nothing
  , title        = "Title"
  , subtitle     = Nothing
  , arg          = Nothing
  , icon         = Nothing
  , valid        = True
  , match        = Nothing
  , autocomplete = Nothing
  , retType      = DefaultRet
  , mods         = defaultMods
  , text         = Nothing
  , quicklookurl = Nothing
  , itemVars     = empty
  }

-- | A image to use as icon, if 'useFileIcon' is true
-- the existing icon from the filepath is used
data Icon = Icon { useFileIcon :: Bool
                 , iconFilePath :: FilePath}
                 deriving Show

instance ToJSON Icon where
  toJSON (Icon i fp) =
    object (("path" .= fp) : [ "type" .= ("fileicon" :: String) | i ])

-- | Changing return type makes Alfred treat your result as a file on your system.
-- This allows the user to perform actions on the file like they can with Alfred's standard file filters.
data RetType
  = DefaultRet    -- ^ Return is not a file
  | FileRet       -- ^ Return is a file
  | FileRetSkip   -- ^ Return is a file, dont bother checking its existence, faster but unsafe
  deriving (Show)

instance ToJSON RetType where
  toJSON rt =
    String $
    case rt of
      DefaultRet -> "default"
      FileRet -> "file"
      FileRetSkip -> "file:skipcheck"

-- | The mod element gives you control over how the item
--  is affected by modifier keys.
data Mod = Mod
  { modValid :: Maybe Bool -- ^ Override 'valid'
  , modArg :: Maybe String -- ^ Override 'arg'
  , modSubtitle :: Maybe String -- ^ Override 'subtitle'
  , modIcon :: Maybe Icon -- ^ Override 'icon'
  , modVars :: Maybe Vars -- ^ Override 'retVars', __Does not inherit__
  } deriving (Show, Generic)

instance ToJSON Mod where
  toJSON =
    genericToJSON $
    defaultOptions {fieldLabelModifier = modF, omitNothingFields = True}
    where
      modF f =
        case f of
          "modValid" -> "valid"
          "modArg" -> "arg"
          "modSubtitle" -> "subtitle"
          "modIcon" -> "icon"
          "modVars" -> "variables"
          _ -> f

-- | Constructor:
--
-- @
-- myMod =
--   defaultMod
--     { modArg = "Alternative Output"
--     , modIcon = myIcon
--     , modSubtitle = "Im another subtitle"
--     }
-- @
defaultMod :: Mod
defaultMod = Mod
  { modValid    = Just True
  , modArg      = Nothing
  , modSubtitle = Nothing
  , modIcon     = Nothing
  , modVars     = Nothing
  }

-- | The Possible Modifier Keys
data Mods = Mods
  { alt :: Maybe Mod
  , cmd :: Maybe Mod
  , shift :: Maybe Mod
  , fn :: Maybe Mod
  , ctrl :: Maybe Mod
  } deriving (Show, Generic)

instance ToJSON Mods where
  toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }

-- | Constructor:
--
-- @
-- myMod = defaultMods {cmd = myMod}
-- @
defaultMods :: Mods
defaultMods = Mods
  { alt   = Nothing
  , cmd   = Nothing
  , shift = Nothing
  , fn    = Nothing
  , ctrl  = Nothing
  }

-- | Defines the text the user will get
-- when copying the selected result row with __\<Cmd-C\>__
-- or displaying large type with __\<Cmd-L\>__.
data RetText = RetText
  { copy :: Maybe String
  , largetype :: Maybe String
  } deriving (Show, Generic)

instance ToJSON RetText where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

-- | Variables passed out of the script
--
-- @
-- myVars = Data.Map.Strict.fromList [("key", "value")]
-- @
type Vars = Map String String
