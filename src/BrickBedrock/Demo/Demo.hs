{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BrickBedrock.Demo.Demo
    ( run
    ) where

import           Protolude
import Brick ((<+>))
import qualified Brick as B
import           Control.Exception.Safe (throwString)
import           Control.Lens (makeLenses, (?~))
import qualified Data.Text as Txt
import qualified Data.UUID.V4 as UU
import qualified Data.Version as Ver
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K

import qualified BrickBedrock.Core as Bb
import qualified BrickBedrock.Defaults as Bbd
import qualified BrickBedrock.Model as Bb
import qualified Paths_brickBedrock as Paths

-- Make the code a bit easier to read, but I don't really like aliases...
type Event' = Bb.Event DemoState DemoPopup DemoWindow DemoName DemoEvent
type UIState' = Bb.UIState DemoState DemoPopup DemoWindow DemoName DemoEvent
type Window' = Bb.Window DemoState DemoPopup DemoWindow DemoName DemoEvent
type Popup' = Bb.Popup DemoState DemoPopup DemoWindow DemoName DemoEvent
type Name' = Bb.Name DemoName




data DemoState = DemoState
  { _ustStatus :: !Text
  }

data DemoPopup
  = PopDemo

data DemoWindow
  = DWMain

data DemoName
  = DNameTodo
  deriving (Show, Eq, Ord)

data DemoEvent
  = ETodo

makeLenses ''DemoState


run :: IO ()
run = do
  let uiinit = Bbd.defaultInit
       { Bb._uioStartWindow = regWindowMain
       , Bb._uioAppName = "Demo Brick App"
       , Bb._uioAppVersion = Txt.pack $ Ver.showVersion Paths.version
       , Bb._uioHandleUserEvents = handleEvent
       }

  let ust = DemoState
       { _ustStatus = ""
       }

  Bb.runTui uiinit ust


regWindowMain :: Window'
regWindowMain =
  Bb.WUser DWMain "Main" . Just $
    Bb.WindowReg
      { Bb._wrDraw = const drawMain
      , Bb._wrEventHandler = const windowKeyHandler
      }


regPopDemo :: Popup'
regPopDemo =
  Bb.PopUser PopDemo . Just $
    Bb.PopupReg
      { Bb._prDraw = const drawPopup
      , Bb._prEventHandler = const Bb.nopKeyHandler
      }


handleEvent :: DemoEvent -> UIState' -> B.EventM Name' (B.Next UIState')
handleEvent ev st =
  case ev of
    ETodo -> B.continue st


windowKeyHandler :: UIState' -> B.BrickEvent Name' Event' -> B.EventM Name' (B.Next UIState')
windowKeyHandler st ev =
  case ev of
    (B.VtyEvent (V.EvKey k ms)) ->
      case (k, ms) of
        (K.KFun 2, []) -> B.continue $ st & Bb.uiPopup ?~ regPopDemo
        (K.KFun 3, []) -> do
          st2 <- liftIO $ demoBlockingRequest st
          B.continue st2
        (K.KFun 4, []) -> do
          st2 <- liftIO $ demoAsyncRequest st
          B.continue st2

        _ -> B.continue st

    _ -> B.continue st


drawPopup :: UIState' -> B.Widget Name'
drawPopup _st =
  B.txt "this is a demo"


drawMain :: UIState' -> B.Widget Name'
drawMain _st =
  B.txt "demo main window" <+> B.fill ' '


demoBlockingRequest :: UIState' -> IO UIState'
demoBlockingRequest st = do
  id <- UU.nextRandom

  Bb.addBlockingAction st . Bb.PendingAction id "demo.blocking" $ do
    Bb.sendStatusMessage st Bb.StsInfo "Blocking action demo starting" Nothing
    for_ [0..5] $ \i -> do
      threadDelay (500000)
      Bb.sendStatusMessage st Bb.StsTrace ("Blocking: " <> show i) Nothing

    throwString "bang"
    --pure (\stx -> stx, []) --TODO demo do something to state here, e.g. send message


demoAsyncRequest :: UIState' -> IO UIState'
demoAsyncRequest st = do
  id <- UU.nextRandom

  Bb.addAsyncAction st . Bb.PendingAction id "demo.async" $ do
    Bb.sendStatusMessage st Bb.StsInfo "Async action demo starting" Nothing
    for_ [0..3] $ \i -> do
      threadDelay (1000000)
      Bb.sendStatusMessage st Bb.StsTrace ("Async: " <> show i) Nothing

    throwString "ouch"
    --pure (\stx -> stx, []) --TODO demo do something to state here, e.g. send message

  pure st
