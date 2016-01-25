module Test.Main where

import Prelude
import Data.Array
import Data.Maybe
import Data.Foldable
import Data.Monoid

import Control.Monad.Aff
import qualified Network.HTTP.Affjax as AJ
import Network.HTTP.Affjax.Response
import Network.HTTP.Method
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception

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

getPlaying :: forall e. (String -> Eff (ajax :: AJ.AJAX, chan :: Chan, console :: CONSOLE | e) Unit) -> Eff (ajax :: AJ.AJAX, chan :: Chan, console :: CONSOLE | e) Unit
getPlaying f = do
  -- launchAff $ do
  runAff (\error -> log $ message error)
         (\result -> f result.response)
         (AJ.get "http://websiteservices.musicchoice.com/api/channels/NowPlaying/ttla/44")

      --result <- (AJ.get "http://websiteservices.musicchoice.com/api/channels/NowPlaying/ttla/44")
      --liftEff $ log result.response
      --liftEff $ f result.response

-- mcSignal :: forall e. Eff (chan :: Chan, err :: EXCEPTION, ajax :: AJ.AJAX | e) (Channel String)
-- mcSignal = do
--   chan <- channel "Updating..."
--   liftEff $ getPlaying $ send chan
--   return chan

-- test :: forall e . Eff (chan :: Chan, err :: EXCEPTION, ajax :: AJ.AJAX, console :: CONSOLE | e) Unit
-- test = do
--   sig <- mcSignal
--   sig ~> log

hello :: forall e. Eff (console :: CONSOLE | e) (Signal String)
hello = return $ constant "Hello World!"

-- test :: forall e. Signal (Eff (console :: CONSOLE | e) Unit)
-- test = undefined

nowPlaying :: String -> H.MarkupM Unit
nowPlaying trackInfo = H.div arg
    where
      arg = H.text trackInfo

newChan :: forall e. Eff (ajax :: AJ.AJAX, err :: EXCEPTION, chan :: Chan | e) (Channel String)
newChan = channel "Updating..."

test = do
  chan <- channel "Updating..."
  -- mChan <- mcSignal
  -- getPlaying $ send chan
  
  runAff (\error -> log $ message error)
         (\result -> send chan result.response)
         (AJ.get "http://websiteservices.musicchoice.com/api/channels/NowPlaying/ttla/44")

  runSignal $ do
         let sig = delay 10000.0 (subscribe chan)
         sig ~> log


--appTest = 
-- ui3 = nowPlaying <$> 

-- main = do
--   runFlareDrawing "controls1" "output1" ui1
--   runFlareHTML "controls2" "output2" ui2
