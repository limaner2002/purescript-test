module Network.VMHealth where

import Network.VMHealth.Data

import Prelude
import Data.Array
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.JSON
import Data.Either

import Control.Monad.Aff
import qualified Network.HTTP.Affjax as AJ
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception
import DOM.Timer hiding (delay)

--import Signal.DOM
import Signal.Time
import Signal.Channel
import Signal hiding (foldp, filter)

import Flare
import Flare.Drawing
import Flare.Smolder

import qualified Text.Smolder.HTML as H
import qualified Text.Smolder.Markup as H
import qualified Text.Smolder.HTML.Attributes as A

data Bar = Bar Number Number

drawBar :: Bar -> Drawing
drawBar (Bar leftPos height) =
    filled (fillColor (hsl 140.0 0.8 0.4)) (rectangle leftPos 0.0 80.0 height)

drawBars :: Array Bar -> Drawing
drawBars bars = fold $ map drawBar bars

f1 :: Maybe Bar -> Number
f1 (Just (Bar l _)) = l + 100.0
f1 Nothing = 0.0

f :: forall e. UI e (Array Bar)
f = foldp id [Bar 0.0 70.0] actions

actions :: forall e. UI e (Array Bar -> Array Bar)
actions = number "Add new value" 112.0 <**> button "Add" (flip const) (\height -> addBar height)

ui1 :: forall e. UI e Drawing
ui1 = drawBars <$> f

addBar :: Number -> Array Bar -> Array Bar
addBar height bars =
    cons (newBar height bars) bars

newBar :: Number -> Array Bar -> Bar
newBar height bars =
    Bar newPos height
  where
    newPos = f1 $ head bars

light on = H.with H.div arg mempty
    where arg | on = A.className "on"
              | otherwise = mempty

ui2 = light <$> liftSF (since 3000.0) (button "Switch on" unit unit)

getStatus :: forall e. String
          -> (Either String VMStatus -> Eff ( ajax :: AJ.AJAX
                                            , chan :: Chan
                                            , console :: CONSOLE
                                            , timer :: Timer | e) Unit)
          -> Eff ( ajax :: AJ.AJAX
                 , chan :: Chan
                 , console :: CONSOLE
                 , timer :: Timer | e) Unit
getStatus url f = go
    where
      go =
          runAff (\error -> log $ message error)
                 (\result -> do
                    f $ eitherDecode result.response
                    t <- timeout 30000 go
                    log $ "Looping"
                 )
                 (do
                   liftEff $ f $ Right Updating
                   AJ.get url
                 )


hello :: forall e. Eff (console :: CONSOLE | e) (Signal String)
hello = return $ constant "Hello World!"

nowPlaying :: String -> H.MarkupM Unit
nowPlaying trackInfo = H.div arg
    where
      arg = H.text trackInfo

newChan :: forall e. Eff (ajax :: AJ.AJAX, err :: EXCEPTION, chan :: Chan | e) (Channel String)
newChan = channel "Updating..."

test url = do
  chan <- channel $ Right Updating
  getStatus url $ send chan
  
  return chan

status :: Either String VMStatus -> H.MarkupM Unit
status (Right status) = markIssue (maxIssue status) (H.li $ H.text $ arg status)
    where
      arg (VMStatus o) = o.name
      arg Updating = "Updating..."
      markIssue (Just Okay) markup = H.with markup (A.className "list-group-item")
      markIssue (Just (Warn _)) markup = H.with markup (A.className "list-group-item list-group-item-warning")
      markIssue (Just (Fatal _)) markup = H.with markup (A.className "list-group-item list-group-item-danger")
      markIssue Nothing _ = H.with (H.li $ H.text "No engines found?") (A.className "list-group-item list-group-item-danger")
status (Left msg) = H.li $ H.text msg

statuses :: Array (Either String VMStatus) -> H.MarkupM Unit
statuses stats = vmStats
    where
      table = H.with tableStatus (A.className "list-group")
      tableStatus = H.ul $ H.tbody $ foldMap status stats
      vmStats = H.with (H.div table) (A.className "col-xs-2")

accordion :: Array (Either String VMStatus) -> H.MarkupM Unit
accordion eStats = foldMap createPanel eStats
    where
      createPanel (Left msg) = H.text msg
      createPanel (Right status) = toMarkup status

markIssue (Just Okay) = "default"
markIssue (Just (Warn _)) = "warning"
markIssue (Just (Fatal _)) = "danger"

vms :: Array String
vms = [ "http://10.203.50.241:3000/vmStatus"
      , "http://10.203.50.239:3000/vmStatus"
      , "http://appiansandbox.persistent.com:3000/vmStatus"
      , "http://10.203.50.211:3000/vmStatus"
      , "http://10.203.51.109:3000/vmStatus"
      , "http://appianworks.persistent.com:3000/vmStatus"
      , "http://usappiandev1.persistent.co.in:3000"
      , "http://usappiandev2.persistent.co.in:3000"
      , "http://appian-demo.persistent.co.in:3000"
      , "http://appian-prod-demo.persistent.co.in:3000"
      ]

signals = map test vms

maxIssue :: VMStatus -> Maybe EngineStatus
maxIssue (VMStatus o) = maxEngine o.appian
    where
      maxEngine (AppianEngines engines) = maximum $ statuses engines
      maxEngine NoLicense = Just $ Fatal "License has expired!"
      statuses engines = map getStatus engines
      getStatus (Engine _ _ status) = status
maxIssue Updating = Just Okay

g = map subscribe <$> signals
h = lift <$> g

ui3 = accordion <$> sequence h
