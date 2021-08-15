{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BrickBedrock.Core
    ( runTui

    , addAsyncAction
    , addBlockingAction
    , addBlockingActionAsync
    , sendStatusMessage
    , sendErrorNotify
    , Bbd.nopKeyHandler
    , Bbd.popTextForText
    , Bbd.popTextReg
    ) where

import           Protolude
import           Brick ((<=>))
import qualified Brick as B
import qualified Brick.AttrMap as BA
import qualified Brick.BChan as BCh
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import qualified Control.Concurrent.STM.TVar as TV
import           Control.Lens (at,  (%~), (.~), (?~), (^.))
import qualified Data.Map.Strict as Map
import qualified Data.Time as DT
import qualified Data.UUID.V4 as UU
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import qualified BrickBedrock.Defaults as Bbd
import qualified BrickBedrock.Model as Bb



runTui :: (Ord un) => Bb.UIOptions ust up uw un ue -> ust -> IO ()
runTui uio ust = do
  uiChan <- BCh.newBChan 10
  now <- DT.getCurrentTime
  tz <- DT.getCurrentTimeZone

  tickCount <- TV.newTVarIO 0
  blockingActions <- TV.newTVarIO Map.empty
  lastBlocking <- TV.newTVarIO True

  let
    localTime = DT.utcToLocalTime tz now

    bg =
      Bb.BgTask
        { Bb._bgUiChan = uiChan
        , Bb._bgTickCount = tickCount
        , Bb._bgBlockingActions = blockingActions
        , Bb._bgLastBlocking = lastBlocking
        }

    st =
     Bb.UIState
        { Bb._uiBgTask = bg
        , Bb._uiTickCount = 0
        , Bb._uiWindow = uio ^. Bb.uioStartWindow
        , Bb._uiPopup = Nothing
        , Bb._uiChan = uiChan
        , Bb._uiOptions = uio
        , Bb._uiSt = ust
        , Bb._uiStatusMessage = Nothing
        , Bb._uiTime = localTime
        , Bb._uiTimeZone = tz
        , Bb._uiWindowErrors = Bb.UIErrors
          { Bb._uieErrors = BL.list Bb.NameErrorMsgList Vec.empty 1
          , Bb._uieInfos = BL.list Bb.NameInfoMsgList Vec.empty 1
          , Bb._uieFocus = BF.focusRing [Bb.NameInfoMsgList, Bb.NameErrorMsgList, Bb.NameErrorMsgDetail]
          , Bb._uieDetail = BE.editorText Bb.NameErrorMsgDetail Nothing ""
          }
        , Bb._uiErrorMessageHistory = []
        , Bb._uiInfoMessageHistory = []
        , Bb._uiPopText = BE.editorText Bb.NamePopTextEdit Nothing ""
        , Bb._uiBlocked = True
        }

  void . forkIO $
    forever $ do
      threadDelay (60 * 1000000)  -- 1 min
      BCh.writeBChan uiChan $ Bb.EvtUpdate

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  st2 <- addBlockingAction st ((uio ^. Bb.uioAppInit) st)
  void . forkIO $ runBackgroundTask bg st
  void $ B.customMain initialVty buildVty (Just uiChan) (app uio) st2


app :: Bb.UIOptions ust up uw un ue -> B.App (Bb.UIState ust up uw un ue) (Bb.Event ust up uw un ue) (Bb.Name un)
app uio =
  let
    defMap = Map.fromList $ Bbd.defaultAttrs
    usrMap = Map.fromList $ uio ^. Bb.uioUserAttrs
    combined = Map.union usrMap defMap
    final = BA.attrMap V.defAttr $ Map.toList combined
  in
  B.App
    { B.appDraw = drawUI
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = handleEvent
    , B.appStartEvent = pure
    , B.appAttrMap = const final
    }


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Event handlers
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
runBackgroundTask :: Bb.BgTask ust up uw un ue -> Bb.UIState ust up uw un ue -> IO ()
runBackgroundTask bg st = forever $ do
  threadDelay $ 100000 -- 0.1 sec
  atomically $ TV.modifyTVar' (bg ^. Bb.bgTickCount) (\t -> t + 1 `mod` 1000000)

  --------------------------------
  -- Has blocked status changed?
  --------------------------------
  (blockedChanged, isBlocked) <- atomically $ do
    last <- TV.readTVar $ bg ^. Bb.bgLastBlocking
    blocked <- not . null <$> TV.readTVar (bg ^. Bb.bgBlockingActions)

    if last == blocked
      then pure (False, blocked)
      else do
        TV.writeTVar (bg ^. Bb.bgLastBlocking) blocked
        pure (True, blocked)

  -- Update on change
  -- or send when blocked so that the spinner can redraw
  when (isBlocked || blockedChanged) $ do
    BCh.writeBChan (st ^. Bb.uiChan) $ Bb.EvtUpdate
  --------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Event handlers
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
handleEvent :: Bb.UIState ust up uw un ue -> B.BrickEvent (Bb.Name un) (Bb.Event ust up uw un ue) -> B.EventM (Bb.Name un) (B.Next (Bb.UIState ust up uw un ue))
handleEvent st ev =
  case ev of
    (B.AppEvent Bb.EvtUpdate) -> do
      -- Clear expired messages
      now <- liftIO DT.getCurrentTime
      let localTime = DT.utcToLocalTime (st ^. Bb.uiTimeZone) now
      let st2 = case st ^. Bb.uiStatusMessage of
                  Nothing -> st
                  Just (_, _, til) ->
                    if now > til
                      then st & Bb.uiStatusMessage .~ Nothing
                      else st

      tickCount <- liftIO . TV.readTVarIO $ st2 ^. Bb.uiBgTask . Bb.bgTickCount
      isBlocked <- liftIO $ not . null <$>  TV.readTVarIO (st2 ^. Bb.uiBgTask . Bb.bgBlockingActions)

      B.continue $ st2 & Bb.uiTime .~ localTime
                       & Bb.uiTickCount .~ tickCount
                       & Bb.uiBlocked .~ isBlocked

    (B.AppEvent (Bb.EvtBlockingResp resp)) -> do
      st2 <- liftIO $ handleBlockingResponse resp st
      B.continue st2


    (B.AppEvent (Bb.EvtSetStatusMessage lvl msg detail ttl)) -> do
      now <- liftIO DT.getCurrentTime
      logId <- liftIO $ UU.nextRandom
      let
        ttl' = fromMaybe (st ^. Bb.uiOptions . Bb.uioDefaultMessageTimeoutSecs) ttl
        showUntil = DT.addUTCTime (fromIntegral ttl') now
        -- Set the status message
        st2 = st & Bb.uiStatusMessage ?~ (lvl, msg, showUntil)
        -- Show an error popup if required
        st3 = case lvl of
                Bb.StsErrorNotify ->
                  -- Get the previous popup, but dont stack error messages
                  let prev = case st2 ^. Bb.uiPopup of
                               Just (Bb.PopError _ _ (Just p)) -> Just p
                               x -> x
                  in
                  st2 & Bb.uiPopup ?~ Bb.PopError msg detail prev

                _ -> st2

        logMsg = (logId, now, msg, detail)
        depth = st ^. Bb.uiOptions . Bb.uioKeepLogEntries
        -- Add messages to the log
        st4 = case lvl of
                Bb.StsErrorNotify -> st3 & Bb.uiErrorMessageHistory %~ (\h -> take depth $ logMsg : h)
                Bb.StsError -> st3 & Bb.uiErrorMessageHistory %~ (\h -> take depth $ logMsg : h)
                Bb.StsTrace -> st3 & Bb.uiInfoMessageHistory %~ (\h -> take depth $ logMsg : h)
                Bb.StsInfo -> st3
      -- Call onError for error messages
      st5 <- case lvl of
              Bb.StsErrorNotify -> liftIO $ (st4 ^. Bb.uiOptions . Bb.uioOnError) msg detail st4
              Bb.StsError -> liftIO $ (st4 ^. Bb.uiOptions . Bb.uioOnError) msg detail st4
              Bb.StsTrace -> pure st4
              Bb.StsInfo -> pure st4

      B.continue st5

    (B.AppEvent (Bb.EvtAddBlockingAction act)) -> do
      st2 <- liftIO $ addBlockingAction st act
      B.continue st2

    (B.AppEvent (Bb.EvtUser ue)) -> do
      st2 <- liftIO $ (st ^. Bb.uiOptions . Bb.uioHandleUserEvents) ue st
      B.continue st2

    _ ->
      (st ^. Bb.uiOptions . Bb.uioGlobalKeyHandler) st ev
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Drawing
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
drawUI :: Bb.UIState ust up uw un ue -> [B.Widget (Bb.Name un)]
drawUI st =
  let
    windowType = st ^. Bb.uiWindow
    windowWidget = (st ^. Bb.uiOptions . Bb.uioDrawWindow) windowType st
  in
  (if not (st ^. Bb.uiBlocked)
     then []
     else [(st ^. Bb.uiOptions . Bb.uioDrawSpinner) st]
  )
  <>
  (case st ^. Bb.uiPopup of
    Nothing -> []
    Just p -> [(st ^. Bb.uiOptions . Bb.uioDrawPopupContainer) p st]
  )
  <>
  [ (st ^. Bb.uiOptions . Bb.uioDrawWindowContainer) windowType st windowWidget
    <=>
    (st ^. Bb.uiOptions . Bb.uioDrawStatusBar) st
  ]
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Pending actions
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | Add an action that runs in the background but does not block the UI
addAsyncAction :: Bb.UIState ust up uw un ue -> Bb.PendingAction ust up uw un ue -> IO ()
addAsyncAction st (Bb.PendingAction paId paName req) = do
  void . forkIO $ do
    catch
      (do
        resp <- req
        BCh.writeBChan (st ^. Bb.uiChan) $ Bb.EvtBlockingResp (Bb.PendingResponse paId paName resp)
      )
      (\(e::SomeException) -> sendErrorNotify st ("Exception in async action: " <> paName) (Just . show $ e))


-- | Add an action that runs in the background but does not block the UI
addBlockingAction :: Bb.UIState ust up uw un ue -> Bb.PendingAction ust up uw un ue -> IO (Bb.UIState ust up uw un ue)
addBlockingAction st pa@(Bb.PendingAction paId paName _) = do
  atomically $ TV.modifyTVar' (st ^. Bb.uiBgTask . Bb.bgBlockingActions) (\b -> b & at paId ?~ paName)
  runBlockingAction st pa
  pure st


-- | Async version of addBlockingAction
addBlockingActionAsync :: Bb.UIState ust up uw un ue -> Bb.PendingAction ust up uw un ue -> IO ()
addBlockingActionAsync st pa = do
  BCh.writeBChan (st ^. Bb.uiChan) (Bb.EvtAddBlockingAction pa)


runBlockingAction :: Bb.UIState ust up uw un ue -> Bb.PendingAction ust up uw un ue -> IO ()
runBlockingAction st (Bb.PendingAction id name req) = do
  void . forkIO $ do
    catch
      (do
        resp <- req
        BCh.writeBChan (st ^. Bb.uiChan) $ Bb.EvtBlockingResp (Bb.PendingResponse id name resp)
      )
      (\(e::SomeException) -> do
        -- Show the error
        sendErrorNotify st ("Exception in blocking action: " <> name) (Just . show $ e)
        -- Clear the blocking action
        BCh.writeBChan (st ^. Bb.uiChan) $ Bb.EvtBlockingResp (Bb.PendingResponse id name (identity, []))
      )


handleBlockingResponse :: Bb.PendingResponse ust up uw un ue -> Bb.UIState ust up uw un ue -> IO (Bb.UIState ust up uw un ue)
handleBlockingResponse (Bb.PendingResponse pId paName (rfn, ioActions)) st = do
  -- Remove the bending action
  atomically $ TV.modifyTVar' (st ^. Bb.uiBgTask . Bb.bgBlockingActions) (\b -> b & at pId .~ Nothing)
  -- Run the list of IO actions
  let st2 = rfn st
  foldM (\sta act -> act sta) st2 ioActions
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Status messages
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | Set the status text
sendStatusMessage :: Bb.UIState ust up uw un ue -> Bb.StatusLevel -> Text -> Maybe Text -> IO ()
sendStatusMessage st l m d = BCh.writeBChan (st ^. Bb.uiChan) $ Bb.EvtSetStatusMessage l m d Nothing


-- | Set the status text and show an error popup
sendErrorNotify :: Bb.UIState ust up uw un ue -> Text -> Maybe Text -> IO ()
sendErrorNotify st m d = sendStatusMessage st Bb.StsErrorNotify m d
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
