{-# LANGUAGE OverloadedStrings #-}
-- | This module is for checking for updates to the Alfred Workflow
--   using github release.
--
--   The releases must be tagged using Semver: https://semver.org
--   i.e. @2.1.4@
--
--   The asset to download must be have the same name as the workflow
--   appended with @.workflow@ i.e. a packaged workflow:
--   @Hoogle.alfredworkflow@

module Alfred.Updater (updateItem) where

import           Alfred
import           Control.Applicative            ( liftA2 )
import           GitHub.Data.Releases
import           GitHub.Endpoints.Repos.Releases
import           Data.SemVer
import qualified Data.Text                     as T
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

-- | Checks for any available updates and returns a 'Item'
-- with a link if any newer versions
-- are available. Any error is thrown as 'AlfredError UpdaterError'
--
-- Returns 'Nothing' if github can't be reached
updateItem :: String   -- ^ The github owner
           -> String   -- ^ The github repo
           -> AlfM s (Maybe Item)
updateItem owner repo = do
    wfName <- (<> ".alfredworkflow") <$> envVariableThrow "alfred_workflow_name"
    eRels   <- liftIO
        $ releases (mkOwnerName $ T.pack owner) (mkRepoName $ T.pack repo)
    case eRels of
        (Right rels) -> do
            mRel <- newerVersion rels
            mLink <- sequenceA $ assetNameDlLink wfName <$> mRel
            let mVer = fmap T.unpack $ releaseTagName <$> mRel
            pure $ liftA2 mkUpdateItem mVer mLink
        (Left (HTTPError _)) -> pure Nothing
        (Left e) -> throwAlfE $ UpdaterError $ show e

-- | Check for the newest release and see if it is newer
--   then current one.
newerVersion :: Vector Release -> AlfM s (Maybe Release)
newerVersion rels = do
    curVer  <- envVariableThrow "alfred_workflow_version"
    curVer' <- case fromText $ T.pack curVer of
        (Right v) -> pure v
        _         -> throwAlfE $ UpdaterError "Workflow has bad version number"
    let go' r Nothing = if curVer' < fst r then Just r else Nothing
        go' r (Just acc) =
            if fst acc < fst r && curVer' < fst r then Just r else Just acc
    if V.null releaseMap
        then throwAlfE $ UpdaterError "No valid releases found in repo"
        else pure $ snd <$> foldr go' Nothing releaseMap
  where
    releaseMap = V.mapMaybe go $ ((,) =<< fromText . releaseTagName) <$> rels
    go (Left  _, _) = Nothing
    go (Right x, y) = Just (x, y)

-- | Get the download link from a release using a asset name
--   "workflowname.alfredworkflow"
assetNameDlLink :: String -> Release -> AlfM s String
assetNameDlLink name rel = if V.null as
     then
         throwAlfE
         $  UpdaterError
         $  "No matching assets in release: \""
         <> T.unpack (releaseTagName rel)
         <> "\""
     else pure $ T.unpack $ releaseAssetBrowserDownloadUrl $ V.head as
  where
    as = V.filter ((name ==) . T.unpack . releaseAssetName) $ releaseAssets rel

mkUpdateItem :: String -> String -> Item
mkUpdateItem ver url = defaultItem
    { title    = "Workflow update available"
    , subtitle = Just $ "Download version: " <> ver
    , arg      = Just url
    }
