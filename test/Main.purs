module Test.Main where

import Test.Data

import Prelude
import Data.Array
import Data.Maybe
import Data.Foldable
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

test = do
  chan <- channel $ Right Updating
  getStatus "http://10.203.50.241:3000/vmStatus" $ send chan
  
  return chan

status :: Either String VMStatus -> H.MarkupM Unit
status (Right status) = H.div arg
    where
      arg = H.text $ show status
status (Left msg) = H.div $ H.text msg

ui3 = status <$> lift (subscribe <$> test)

main = do
  runFlareHTML "controls3" "output3" ui3