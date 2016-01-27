module Network.VMHealth.Data where

import Prelude
import Data.JSON
import Data.Array
import Data.Foldable
import Data.Monoid

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

-- instance arrayToMarkup :: ToMarkup Array where
--     toMarkup arr = foldMap toMarkup arr

data VMStatus
    = VMStatus
      { appian :: AppianEngines
      , name :: String
      }
    | Updating

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

instance vmStatusToMarkup :: ToMarkup VMStatus where
    toMarkup (VMStatus { appian = appEngines, name = vmName}) =
        H.with (H.div panel) (A.className "panel panel-default")
      where
        panel = heading <> body
        heading = H.with (H.div title) (A.className "panel-heading" <> role "tab" <> A.id "heading")
        title = H.with (H.h4 button) (A.className "panel-title")
        button = H.with (H.a $ toMarkup vmName) (role "button" <> dataToggle "collapse" <> dataParent "#accordion" <> A.href "#collapseOne")
        panelCollapse = H.with (H.div body) (A.id "collapse" <> A.className "panel-collapse collapse in" <> role "tabpanel")
        body = H.with (H.div $ toMarkup appEngines) (A.className "panel-body")
    toMarkup Updating = H.text "Updating..."

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