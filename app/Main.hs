{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Alfred
import           Alfred.Updater
import qualified Data.Map.Strict               as M
import           Data.Time                      ( getZonedTime )
import           Data.Maybe                     ( maybeToList )
import           Control.Exception
import qualified System.IO.Strict              as S

main :: IO ()
main = alfMain showExamples

showExamples :: AlfM String (Maybe Return)
showExamples = do
  state <- get
  args <- alfArgs
  -- The name of the script
  name <- envVariableThrow "alfred_workflow_name"
  -- A variable passed thru environment
  inputVar <- envVariable "TestInput"
  inputVar' <- case inputVar of
                 Nothing ->  throwAlfE $ EnvVarError "Failed to get variable TestInput"
                 (Just v ) -> return v

  perDir <- workflowDataDir
  volDir <- workflowCacheDir
  -- The time saved on disk from the previous run
  perTime <- loadTime
  -- save for next run
  saveCurrentTime
  -- save the args to state for the next run
  put $ concat args

  -- Get the update Item
  updateItem' <- updateItem "meck" "alfred-hoogle"

  let it =
        [ defaultItem
            {title =  name, subtitle = Just "The Name of the script"}
        , defaultItem
            { title =  qouteString $ unlines args
            , subtitle = Just "The arguments to the script"
            }
        , defaultItem
            { title = qouteString state
            , subtitle = Just "The State which is the previous input"
            }
        , defaultItem
            { title =  qouteString inputVar'
            , subtitle = Just "A Variable passed from Alfred"
            }
        , defaultItem
            { title =  perTime
            , subtitle = Just "The time the previous time this was run"
            }
        , defaultItem
            { title = "Data Dir"
            , subtitle = Just "The permanent data store directory"
            , retType = FileRet
            , arg = Just perDir
            }
        , defaultItem
            { title = "Cache Dir"
            , subtitle = Just "The cache data store directory"
            , retType = FileRet
            , arg = Just volDir
            }
        , defaultItem
            { title = "Quicklook"
            , subtitle = Just "Quicklook to website with <Cmd-Y>"
            , quicklookurl = Just "https://www.haskell.org"
            }
        , defaultItem
            { title = "Match"
            , subtitle = Just "This should match on hctaM"
            , match = Just "hctaM"
            }
        , defaultItem
            { title = "Auto complete"
            , subtitle = Just "changes to success when auto completing with ac"
            , autocomplete = Just "success"
            }
        , defaultItem
            { title = "Test output"
            , subtitle = Just "sends a \"Hi🍊\" to the output"
            , arg = Just "Hi🍊"
            }
        , defaultItem
            { title = "Icon"
            , subtitle = Just "Displays an icon"
            , icon = Just $ Icon False "Haskell logo.png"
            }
        , defaultItem
            { title = "Icon From File"
            , subtitle = Just "Displays the icon from the desktop folder"
            , icon = Just $ Icon True "~/Desktop"
            }
        , defaultItem
            { title = "File"
            , subtitle = Just "Pass output as file"
            , arg = Just "~/Desktop"
            , retType = FileRet
            }
        , defaultItem
            { title = "File without check"
            , subtitle = Just "Pass output as file not checking existence"
            , arg = Just "~/notafile"
            , retType = FileRetSkip
            }
        , defaultItem
            { title = "Copy Text"
            , subtitle = Just "Copy with <Cmd-C> or large text with <Cmd-L>"
            , text =
                Just RetText {copy = Just "Copy me", largetype = Just "Large"}
            }
        , defaultItem
            { title = "Invalidated"
            , subtitle = Just "Has arguments but should not be run"
            , arg = Just "NeverRun"
            , valid = False
            }
        , defaultItem
            { title = "Item Var"
            , subtitle = Just "Output a environment var from an item🍊"
            , itemVars = M.fromList [("OutputVar", "Output var from an item, overriding")]
            }
        , defaultItem
            { title = "Modifers"
            , subtitle = Just "Test all the modifers"
            , mods = Mods (testModKey "alt") (testModKey "cmd") (testModKey "shift") (testModKey "fn") (testModKey "ctrl")
            }
        ]
        ++ maybeToList updateItem'

  asScript <- envVariable "asScript"
  case asScript of
    (Just "true") -> do
      liftIO $ putStr "Simple Script output"
      return Nothing
    _ -> return $ Just $ defaultReturn
      { items   = addUids it
      , retVars = M.fromList [("OutputVar", "Output var from the main Object")]
              -- , rerun = Just 1
      }


testModKey :: String -> Maybe Mod
testModKey k = Just $ defaultMod {
                             modArg = Just $ "Modified Argument: " <> k
                            , modSubtitle = Just $ "Modified Subtitle: " <> k
                            , modIcon = Just $ Icon False "Haskell logo.png"
                            , modVars = Just $ M.fromList [("OutputVar", "Output var from mod: " <> k <> "🍊")]
                              }

addUids :: [Item] -> [Item]
addUids is = zipWith (\i u -> i { uid = u }) is uids
  where uids = [ Just $ "alf" <> show i | i <- [0 :: Int ..] ]

qouteString :: String -> String
qouteString x = q <> x <> q where q = "\""

saveCurrentTime :: AlfM String ()
saveCurrentTime = do
  time    <- liftIO getZonedTime
  dataDir <- workflowDataDir
  liftIO $ writeFile (dataDir <> "/Timefile") $ show time

loadTime :: AlfM String String
loadTime = do
 dataDir <- workflowDataDir
 liftIO $ (\(x :: Either IOError String) -> case x of
            (Left _) -> "Timefile not found"
            (Right v) -> v) <$> try (S.readFile (dataDir <> "/Timefile"))
