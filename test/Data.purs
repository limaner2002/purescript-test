module Test.Data where

import Prelude
import Data.JSON
import Data.Array

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

data EngineStatus
    = Okay
    | Warn String
    | Fatal String

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