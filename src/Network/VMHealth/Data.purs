module Network.VMHealth.Data where

import Prelude
import Data.JSON
import Data.Array
import Data.Foldable
import Data.Monoid
import Data.Either
import Data.Maybe
import Optic.Core hiding ((..))

import qualified Text.Smolder.HTML as H
import qualified Text.Smolder.Markup as H
import qualified Text.Smolder.HTML.Attributes as A

role = H.attribute "role"
dataToggle = H.attribute "data-toggle"
dataParent = H.attribute "data-parent"

class ToMarkup a where
    toMarkup :: a -> H.Markup

instance stringToMarkup :: ToMarkup String where
    toMarkup str = H.text str

instance maybeToMarkup :: (ToMarkup a) => ToMarkup (Maybe a) where
    toMarkup (Just val) = toMarkup val
    toMarkup Nothing = mempty

-- instance arrayToMarkup :: ToMarkup Array where
--     toMarkup arr = foldMap toMarkup arr

data VMStatus
    = VMStatus
      { appian :: AppianEngines
      , name :: String
      }
    | Updating

_engines :: LensP VMStatus (Maybe AppianEngines)
_engines = lens getEngines setEngines
 where
   getEngines (VMStatus status) = Just status.appian
   getEngines Updating = Nothing
   setEngines (VMStatus status) (Just newEngines) = VMStatus (status {appian = newEngines})
   setEngines (VMStatus status) Nothing = VMStatus status
   -- setEngines Updating _ = fail "No engines to update!"

instance showVMStatus :: Show VMStatus where
    show (VMStatus { appian = app, name = vmName }) =
        "VMStatus: " ++ show app ++ " " ++ vmName
    show Updating = "Updating..."

instance vmStatusFromJSON :: FromJSON VMStatus where
    parseJSON (JObject o) = do
      engines <- o .: "appianStatus"
      vmName <- o .: "name"
      return $ VMStatus {appian: engines, name: vmName}
    parseJSON _ = fail "VMStatus parse failed"

createPanel :: Either String VMStatus -> Int -> H.Markup
createPanel (Right status) idx =
        H.with (H.div panel) (A.className "panel panel-default")
      where
        panel = heading <> panelCollapse
        heading = H.with (H.div title) (A.className "panel-heading" <> role "tab" <> A.id "heading")
        title = H.with (H.h4 button) (A.className "panel-title")
        button = H.with (H.a $ getName status) (role "button" <> dataToggle "collapse" <> dataParent "#accordion" <> A.href ("#collapse" <> show idx))
        panelCollapse = H.with (H.div body) (A.id ("collapse" <> show idx) <> A.className "panel-collapse collapse" <> role "tabpanel")
        body = H.with (H.div $ getDetails status) (A.className "panel-body")
        getName (VMStatus o) = toMarkup o.name
        getName Updating = toMarkup "Updating..."
        getDetails (VMStatus o) = toMarkup o.appian
        getDetails Updating = toMarkup "Updating..."
createPanel (Left msg) _ = toMarkup msg

createPill :: Either String VMStatus -> Int -> H.Markup
createPill eStat idx =
    H.with (H.li (H.with (H.a $ pillLabel eStat) (A.href ("#vm" <> show idx) <> dataToggle "tab"))) isActive
  where
    isActive :: H.Attribute
    isActive =
        if idx == 1
        then A.className "active"
        else mempty
    pillLabel (Right (VMStatus o)) = displayName o.name (maxIssue o.appian)
    pillLabel (Right Updating) = displayName "Updating..." $ Just Okay
    pillLabel (Left msg) = displayName "ERROR!" (Just $ Fatal msg)

displayName name issue = H.with (H.i $ H.text mempty) (A.className ("fa " <> markIssue issue))
                <> H.span (toMarkup $ " " ++ name)

maxIssue :: AppianEngines -> Maybe EngineStatus
maxIssue (AppianEngines engines) = maximum $ statuses engines
    where
      statuses engines = map getStatus engines
      getStatus (Engine _ _ status) = status
maxIssue NoLicense = Just $ Fatal "No license or license expired"

markIssue (Just Okay) = "glyphicon-none"
markIssue (Just (Warn _)) = "fa-exclamation-triangle text-warning"
markIssue (Just (Fatal _)) = "fa-exclamation-circle text-danger"

showVMDetails :: Either String VMStatus -> Int -> H.Markup
showVMDetails eStat idx = H.with (H.div (getDetails eStat)) (A.className "tab-pane" <> A.id ("vm" <> show idx))
    where
      getDetails (Right status) = toMarkup (status ^. _engines)
      getDetails (Right Updating) = toMarkup "Updating..."
      getDetails (Left msg) = toMarkup msg

data AppianEngines
    = AppianEngines (Array Engine)
    | NoLicense

instance showAppianEngines :: Show AppianEngines where
    show (AppianEngines engines) =
        "AppianEngines: " ++ show engines
    show NoLicense =
        "NoLicense"

instance appianEnginesFromJSON :: FromJSON AppianEngines where
    parseJSON (JObject o) = do
      licenseStatus <- o .: "licenseStatus"
      case licenseStatus of
        "Okay" -> do
               engines <- o .: "AppianEngines"
               return $ AppianEngines engines
        "NoLicense" -> return NoLicense
    parseJSON _ = fail "AppianEngines parse failed"

instance appianEnginesToMarkup :: ToMarkup AppianEngines where
    toMarkup (AppianEngines engines) =
        H.with (H.table (H.tbody rows)) (A.className "table")
      where
        rows = foldMap engineRow engines
        engineRow engine = H.tr $ toMarkup engine
    toMarkup NoLicense =
        H.tr $ toMarkup "No license or licens has expired!"

data EngineStatus
    = Okay
    | Warn String
    | Fatal String

instance engineStatusOrd :: Ord EngineStatus where
    compare Okay Okay = EQ
    compare Okay _ = LT
    compare _ Okay = GT
    compare (Warn _) (Warn _) = EQ
    compare (Warn _) _ = LT
    compare _ (Warn _) = LT
    compare (Fatal _) (Fatal _) = EQ

instance engineStatusEq :: Eq EngineStatus where
    eq Okay Okay = true
    eq (Warn _) (Warn _) = true
    eq (Fatal _) (Fatal _) = true
    eq _ _ = false

instance engineStatusShow :: Show EngineStatus where
    show Okay = "Okay"
    show (Warn msg) = "Warning: " ++ msg
    show (Fatal msg) = "Fatal: " ++ msg

instance engineStatusFromJSON :: FromJSON EngineStatus where
    parseJSON (JObject o) = do
      status <- o .: "status"
      case status of
        "Okay" -> return Okay
        "Warn" -> do
          msg <- o.: "message"
          return $ Warn msg
        "Fatal" -> do
          msg <- o.: "message"
          return $ Fatal msg
    parseJSON _ = fail "EngineStatus parse failed"

instance engineStatusToMarkup :: ToMarkup EngineStatus where
    toMarkup Okay = H.td $ toMarkup "Okay" <> H.td mempty
    toMarkup (Warn msg) = H.td (toMarkup "Warning") <> H.td (toMarkup msg)
    toMarkup (Fatal msg) = H.td (toMarkup "Fatal") <> H.td (toMarkup msg)

data Engine =
    Engine String Active EngineStatus

instance engineShow :: Show Engine where
    show (Engine name active status) =
        name ++ " " ++ show active ++ " " ++ show status

instance engineFromJSON :: FromJSON Engine where
    parseJSON (JObject o) = do
      name <- o .: "name"
      active <- o .: "active"
      details <- o .: "details"
      return $ Engine name active details
    parseJSON _ = fail "Engine parse failed"

instance engineToMarkup :: ToMarkup Engine where
    toMarkup (Engine name active status) =
        H.td (toMarkup name) <> H.td (toMarkup active) <> H.td (toMarkup status)

data Active
    = Active String String

instance showActive :: Show Active where
    show (Active up total) =
        up ++ "/" ++ total

instance activeFromJSON :: FromJSON Active where
    parseJSON (JObject o) = do
      up <- o .: "up"
      total <- o .: "total"
      return $ Active up total
    parseJSON _ = fail "Active parse failed"

instance activeToMarkup :: ToMarkup Active where
    toMarkup (Active up total) =
        H.td (toMarkup up) <> H.td (toMarkup total)