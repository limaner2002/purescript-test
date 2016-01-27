module Test.Test where

import Network.VMHealth.Data
import Network.VMHealth
import Data.Array
import Data.JSON
import Data.Either
import Prelude (map)

testStatus :: Array (Either String VMStatus)
testStatus = decoded
    where
    decoded = map eitherDecode vmTests
    vmTests = [ "{\"appianStatus\":{\"licenseStatus\":\"Okay\",\"AppianEngines\":[{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Discussion Forums\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Notifications Service\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Notifications Email Processor\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Channels\",\"details\":{\"status\":\"Okay\"}}]},\"name\":\"usappiandev1\"}"
              , "{\"appianStatus\":{\"licenseStatus\":\"Okay\",\"AppianEngines\":[{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Discussion Forums\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Notifications Service\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Notifications Email Processor\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Channels\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Collaboration\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Collaboration Statistics\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Personalization\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Portal\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Process-Design\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Process-Analytics0000\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Process-Analytics0001\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Process-Analytics0002\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Process-Exec00\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Process-Exec01\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Process-Exec02\",\"details\":{\"status\":\"Okay\"}}]},\"name\":\"usappiandev2\"}"
              ]

testEngines :: Either String AppianEngines
testEngines = eitherDecode "{\"licenseStatus\":\"Okay\",\"AppianEngines\":[{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Discussion Forums\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Notifications Service\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Notifications Email Processor\",\"details\":{\"status\":\"Okay\"}},{\"active\":{\"up\":\"1\",\"total\":\"1\"},\"name\":\"Channels\",\"details\":{\"status\":\"Okay\"}}]}"

getValue :: forall a. Either String a -> a
getValue (Right val) = val

getMessage :: forall a. Either String a -> String
getMessage (Left msg) = msg