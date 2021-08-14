{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BrickBedrock.Defaults
    ( defaultInit
    , defaultAttrs
    , defaultDrawPopupContainer
    , defaultGlobalKeyHandler
    , defaultPopupKeyHandler
    , defaultWindowKeyHandler
    , defaultDrawPopupContent
    , defaultDrawWindow
    , defaultDrawWindowContainer
    , defaultAppInit
    , defaultWinRegErrors
    , defaultEventHandlerErrorsWindow
    , nopKeyHandler
    , popTextReg
    , popTextForText
    ) where

import           Protolude
import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.AttrMap as BA
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Dialog as BD
import qualified Brick.Widgets.List as BL
import qualified Control.Concurrent.STM.TVar as TV
import           Control.Lens ((%~), (.~), (?~), (^.))
import qualified Data.Text as Txt
import qualified Data.Time as DT
import qualified Data.UUID as UU
import qualified Data.Vector as Vec
import qualified Data.Version as Ver
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K

import qualified Paths_brickBedrock as Paths
import qualified BrickBedrock.Model as Bb
import qualified BrickBedrock.Controls.ReadOnlyEdit as BbRo



defaultInit :: (Show un, Ord un) => Bb.UIOptions ust up uw un ue
defaultInit =
  Bb.UIOptions
    { Bb._uioGlobalKeyHandler = defaultGlobalKeyHandler
    , Bb._uioWindowKeyHandler = defaultWindowKeyHandler
    , Bb._uioPopupKeyHandler = defaultPopupKeyHandler
    , Bb._uioDrawPopupContainer = defaultDrawPopupContainer
    , Bb._uioDrawPopupContent = defaultDrawPopupContent
    , Bb._uioDrawSpinner = defaultDrawSpinner
    , Bb._uioDrawWindowContainer = defaultDrawWindowContainer
    , Bb._uioDrawWindow = defaultDrawWindow
    , Bb._uioDrawStatusBar = defaultDrawStatusBar
    , Bb._uioStartWindow = Bb.WUnknown
    , Bb._uioAppInit = defaultAppInit
    , Bb._uioDefaultMessageTimeoutSecs = 360
    , Bb._uioAppName = "Brick Bedrock"
    , Bb._uioAppVersion = Txt.pack $ Ver.showVersion Paths.version
    , Bb._uioKeepLogEntries = 100
    , Bb._uioOnError = \_ _ st -> pure st
    , Bb._uioErrorsWindowReg = defaultWinRegErrors
    , Bb._uioHandleUserEvents = \_ s -> pure s
    , Bb._uioUserAttrs = []
    , Bb._uioHelpPopupReg = Bb.PopupReg
                         { Bb._prDraw = const defaultDrawHelp
                         , Bb._prEventHandler = const nopKeyHandler
                         }
    , Bb._uioErrorPopupReg = Bb.PopupReg
                         { Bb._prDraw = defaultDrawErrorPopup
                         , Bb._prEventHandler = const nopKeyHandler
                         }
    , Bb._uioTextPopupReg = popTextReg
    }


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Default spinner
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
defaultDrawSpinner :: Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
defaultDrawSpinner st =
  BD.renderDialog (BD.dialog Nothing Nothing 20) (B.vBox [ B.vLimit 2 . B.hLimit 13 $ B.fill ' '
                                                         , B.vLimit 2 . B.hLimit 13 . BC.center . B.withAttr "spinnerText" . B.txt $ fromMaybe "." (atMay Bb.spinner (st ^. Bb.uiTickCount `mod` length Bb.spinner))
                                                         , B.vLimit 2 . B.hLimit 13 $ B.fill ' '
                                                         ]
                                                 )
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Default key map
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
defaultAttrs :: [(BA.AttrName, V.Attr)]
defaultAttrs =
    [ (BE.editAttr, V.black `B.on` V.cyan)
    , (BE.editFocusedAttr, V.black `B.on` V.yellow)
    , (BL.listAttr, V.white `B.on` V.blue)
    , (BL.listSelectedAttr, V.blue `B.on` V.white)
    , (BL.listSelectedFocusedAttr, V.black `B.on` V.yellow)
    , ("infoTitle", B.fg V.cyan)
    , ("button", V.defAttr)
    , ("buttonFocus", V.black `B.on` V.yellow)
    , ("messageError", B.fg V.red)
    , ("messageWarn", B.fg V.brightYellow)
    , ("messageInfo", B.fg V.cyan)
    , ("titleText", B.fg V.green)
    , ("normalText", B.fg V.white)
    , ("topBar", V.black `B.on` V.white)
    , ("status_Stopped", B.fg V.brightRed)
    , ("status_Terminated", B.fg V.yellow)
    , ("status_Running", B.fg V.brightGreen)
    , ("popup", V.green `B.on` V.black)
    , ("spinner", V.white `B.on` V.black)
    , ("spinnerText", B.fg V.brightGreen)
    , ("msgInfo", B.fg V.brightCyan)
    , ("msgError", B.fg V.brightRed)
    , ("windowTitle", V.brightWhite `B.on` V.magenta)
    ]
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Default key handlers
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nopKeyHandler :: Bb.UIState ust up uw un ue -> B.BrickEvent (Bb.Name un) (Bb.Event ust up uw un ue) -> B.EventM (Bb.Name un) (B.Next (Bb.UIState ust up uw un ue))
nopKeyHandler st _ev = B.continue st


defaultGlobalKeyHandler :: Bb.UIState ust up uw un ue -> B.BrickEvent (Bb.Name un) (Bb.Event ust up uw un ue) -> B.EventM (Bb.Name un) (B.Next (Bb.UIState ust up uw un ue))
defaultGlobalKeyHandler st ev = do
  bgTasks <- liftIO . TV.readTVarIO $ st ^. Bb.uiBgTask . Bb.bgBlockingActions
  let next =
        case (null bgTasks, st ^. Bb.uiPopup) of
          -- No other key handlers run when there is a blocking action
          (False, _) -> \s _ -> B.continue s
          -- Popup next
          (True, Just p) -> (st ^. Bb.uiOptions . Bb.uioPopupKeyHandler) p
          -- Window next
          (True, Nothing) -> st ^. Bb.uiOptions . Bb.uioWindowKeyHandler

  case ev of
    (B.VtyEvent (V.EvKey k ms)) ->
      case (k, ms) of
        (K.KChar 'q', [K.MCtrl]) -> B.halt st
        _ -> next st ev

    _ -> next st ev


defaultWindowKeyHandler :: Bb.UIState ust up uw un ue -> B.BrickEvent (Bb.Name un) (Bb.Event ust up uw un ue) -> B.EventM (Bb.Name un) (B.Next (Bb.UIState ust up uw un ue))
defaultWindowKeyHandler st ev =
  case ev of
    (B.VtyEvent (V.EvKey k ms)) ->
      case (k, ms) of
        (K.KFun 1, []) -> B.continue $ st & Bb.uiPopup ?~ Bb.PopHelp

        (K.KFun 12, []) ->
          case st ^. Bb.uiWindow of
            Bb.WErrors _ -> B.continue st
            prev -> do
              let detail =
                   case headMay $ st ^. Bb.uiErrorMessageHistory of
                     Just (_, _, _, Just d) -> d
                     _ -> ""

              B.continue $ st & Bb.uiWindow .~ Bb.WErrors prev
                              & Bb.uiWindowErrors . Bb.uieErrors .~ BL.list Bb.NameErrorMsgList (Vec.fromList $ st ^. Bb.uiErrorMessageHistory) 1
                              & Bb.uiWindowErrors . Bb.uieInfos .~ BL.list Bb.NameInfoMsgList (Vec.fromList $ st ^. Bb.uiInfoMessageHistory) 1
                              & Bb.uiWindowErrors . Bb.uieDetail .~ BE.editorText Bb.NameErrorMsgDetail Nothing detail

        (K.KEsc, []) ->
          case st ^. Bb.uiWindow of
            Bb.WErrors p -> B.continue $ st & Bb.uiWindow .~ p
            Bb.WUserHist _ _ _ p -> B.continue $ st & Bb.uiWindow .~ p
            _ -> B.continue st

        _ ->
          case st ^. Bb.uiWindow of
            Bb.WErrors _ -> (st ^. Bb.uiOptions . Bb.uioErrorsWindowReg . Bb.wrEventHandler) (st ^. Bb.uiWindow) st ev
            Bb.WUser _ _ (Just wr) -> (wr ^. Bb.wrEventHandler) (st ^. Bb.uiWindow) st ev
            Bb.WUserHist _ _ (Just wr) _ -> (wr ^. Bb.wrEventHandler) (st ^. Bb.uiWindow) st ev
            _ -> B.continue st

    _ -> B.continue st


defaultPopupKeyHandler :: Bb.Popup ust up uw un ue -> Bb.UIState ust up uw un ue -> B.BrickEvent (Bb.Name un) (Bb.Event ust up uw un ue) -> B.EventM (Bb.Name un) (B.Next (Bb.UIState ust up uw un ue))
defaultPopupKeyHandler pop st ev =
  case ev of
    (B.VtyEvent (V.EvKey k ms)) ->
      case (k, ms) of
        (K.KEsc, []) ->
          B.continue $ st & Bb.uiPopup .~ case st ^. Bb.uiPopup of
                                         Just (Bb.PopError _ _ (Just prev)) -> Just prev
                                         _ -> Nothing

        _ -> next st
    _ -> next st

  where
    next stx =
      case pop of
        Bb.PopUser _ (Just pr) -> (pr ^. Bb.prEventHandler) pop stx ev
        _ -> B.continue stx


eventHandlerPopupText :: Bb.Popup ust up uw un ue -> Bb.UIState ust up uw un ue -> B.BrickEvent (Bb.Name un) (Bb.Event ust up uw un ue) -> B.EventM (Bb.Name un) (B.Next (Bb.UIState ust up uw un ue))
eventHandlerPopupText _p st ev = do
  case ev of
    (B.VtyEvent ve) -> do
      e <- BbRo.handleReadOnlyEditorEvent ve (st ^. Bb.uiPopText)
      B.continue $ st & Bb.uiPopText .~ e
    _ -> B.continue st
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Default drawing
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
defaultDrawPopupContainer :: Bb.Popup  ust up uw un ue -> Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
defaultDrawPopupContainer p st =
  BD.renderDialog (BD.dialog Nothing Nothing 210) (B.vLimit 75 $ (st ^. Bb.uiOptions . Bb.uioDrawPopupContent) p st)


defaultDrawHelp :: Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
defaultDrawHelp _st =
  B.txt "Default help popup"


defaultDrawPopupContent :: Bb.Popup  ust up uw un ue -> Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
defaultDrawPopupContent p st =
  case p of
    Bb.PopHelp -> (st ^. Bb.uiOptions . Bb.uioHelpPopupReg . Bb.prDraw) p st
    Bb.PopError _ _ _ -> (st ^. Bb.uiOptions . Bb.uioErrorPopupReg . Bb.prDraw) p st
    Bb.PopText _ -> (st ^. Bb.uiOptions . Bb.uioTextPopupReg . Bb.prDraw) p st
    Bb.PopUser _ (Just pr) -> (pr ^. Bb.prDraw) p st

    _ -> B.txt "User popup draw expected"


defaultDrawErrorPopup :: Bb.Popup ust up uw un ue -> Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
defaultDrawErrorPopup (Bb.PopError title detail _) _st =
  B.withAttr "msgError" (B.txt title)
  <=>
  (B.txt $ "\n\n" <> fromMaybe "" detail)
defaultDrawErrorPopup _ _st =
  (B.txt "Invalid popup for error")


defaultDrawWindowContainer :: Bb.Window ust up uw un ue -> Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un) -> B.Widget (Bb.Name un)
defaultDrawWindowContainer w _st windowWidget = do
  let title = case w of
                Bb.WUnknown -> "unknown"
                Bb.WErrors _ -> "Errors"
                Bb.WUser _ t _ -> t
                Bb.WUserHist _ t _ _ -> t
  (B.withAttr "windowTitle" . B.vLimit 1 $ B.fill ' ' <+> B.txt title <+> B.fill ' ')
  <=>
  windowWidget


defaultDrawWindow :: Bb.Window ust up uw un ue -> Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
defaultDrawWindow w st =
  case w of
    Bb.WUnknown -> B.txt "Brick base unknown window" <+> B.fill ' '
    Bb.WErrors _ -> (st ^. Bb.uiOptions . Bb.uioErrorsWindowReg . Bb.wrDraw) w st
    Bb.WUser _ _ (Just wr) -> (wr ^. Bb.wrDraw) w st
    Bb.WUserHist _ _ (Just wr) _ -> (wr ^. Bb.wrDraw) w st
    _ -> B.txt "User window draw expected" <+> B.fill ' '


defaultDrawStatusBar :: Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
defaultDrawStatusBar st =
  let
    time = Txt.pack $ DT.formatTime DT.defaultTimeLocale "%H-%M " (st ^. Bb.uiTime)
    statusLeft =
      case st ^. Bb.uiStatusMessage of
        Nothing -> B.emptyWidget
        Just (Bb.StsInfo, m, _) -> B.withAttr "msgInfo" . B.txt $ m
        Just (Bb.StsTrace, m, _) -> B.withAttr "msgInfo" . B.txt $ m
        Just (Bb.StsError, m, _) -> B.withAttr "msgError" . B.txt $ m
        Just (Bb.StsErrorNotify, m, _) -> B.withAttr "msgError" . B.txt $ m
    statusRight = B.withAttr "normalText" . B.txt $ " | " <> (st ^. Bb.uiOptions . Bb.uioAppName) <> " | " <> (st ^. Bb.uiOptions . Bb.uioAppVersion) <> " | " <> time
  in
  (B.vLimit 1 $ statusLeft <+> B.fill ' ' <+> statusRight)


drawPopupText :: (Ord un, Show un) => Bb.Popup ust up uw un ue -> Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
drawPopupText (Bb.PopText _t) st =
  BE.renderEditor (B.txt . Txt.unlines) True (st ^. Bb.uiPopText)
drawPopupText _ _ =
  B.txt "invalid popup type for text"
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Errors window
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
defaultWinRegErrors :: (Ord un, Show un) => Bb.WindowReg ust up uw un ue
defaultWinRegErrors =
  Bb.WindowReg
    { Bb._wrDraw = const defaultDrawErrorsWindow
    , Bb._wrEventHandler = const defaultEventHandlerErrorsWindow
    }


defaultEventHandlerErrorsWindow :: (Ord un) => Bb.UIState ust up uw un ue -> B.BrickEvent (Bb.Name un) (Bb.Event ust up uw un ue) -> B.EventM (Bb.Name un) (B.Next (Bb.UIState ust up uw un ue))
defaultEventHandlerErrorsWindow st ev =
  case ev of
    (B.VtyEvent ve@(V.EvKey k ms)) ->
      case (k, ms) of
        (K.KChar '\t', []) -> B.continue . updateDetail $ st & Bb.uiWindowErrors . Bb.uieFocus %~ BF.focusNext
        (K.KBackTab, _) -> B.continue . updateDetail $ st & Bb.uiWindowErrors . Bb.uieFocus %~ BF.focusPrev
        _ ->
          case BF.focusGetCurrent (st ^. Bb.uiWindowErrors . Bb.uieFocus) of
            Just Bb.NameErrorMsgList -> do
              l <- BL.handleListEventVi BL.handleListEvent ve $ st ^. Bb.uiWindowErrors . Bb.uieErrors
              B.continue . updateDetail $ st & Bb.uiWindowErrors . Bb.uieErrors .~ l

            Just Bb.NameInfoMsgList -> do
              l <- BL.handleListEventVi BL.handleListEvent ve $ st ^. Bb.uiWindowErrors . Bb.uieInfos
              B.continue . updateDetail $ st & Bb.uiWindowErrors . Bb.uieInfos .~ l

            Just Bb.NameErrorMsgDetail -> do
              e <- BbRo.handleReadOnlyEditorEvent ve $ st ^. Bb.uiWindowErrors . Bb.uieDetail
              B.continue $ st & Bb.uiWindowErrors . Bb.uieDetail .~ e

            _ -> (st ^. Bb.uiOptions . Bb.uioWindowKeyHandler) st ev

    _ -> (st ^. Bb.uiOptions . Bb.uioWindowKeyHandler) st ev

  where
    updateDetail sx =
      case BF.focusGetCurrent (sx ^. Bb.uiWindowErrors . Bb.uieFocus) of
        Just Bb.NameErrorMsgList -> do
          let detail =
               case snd <$> BL.listSelectedElement (st ^. Bb.uiWindowErrors . Bb.uieErrors) of
                 Just (_, _, _, Just d) -> d
                 _ -> ""
          sx & Bb.uiWindowErrors . Bb.uieDetail .~ BE.editorText Bb.NameErrorMsgDetail Nothing detail

        Just Bb.NameInfoMsgList -> do
          let detail =
               case snd <$> BL.listSelectedElement (st ^. Bb.uiWindowErrors . Bb.uieInfos) of
                 Just (_, _, _, Just d) -> d
                 _ -> ""
          sx & Bb.uiWindowErrors . Bb.uieDetail .~ BE.editorText Bb.NameErrorMsgDetail Nothing detail

        _ -> sx



defaultDrawErrorsWindow :: (Ord un, Show un) => Bb.UIState ust up uw un ue -> B.Widget (Bb.Name un)
defaultDrawErrorsWindow st =
  B.vBox
    [ B.withAttr "infoTitle" $ B.txt "Error Messages"
    , BL.renderList (\_ m -> B.vLimit 1 $ drawLogMsg m) (BF.focusGetCurrent (st ^. Bb.uiWindowErrors . Bb.uieFocus) == Just Bb.NameErrorMsgList) (st ^. Bb.uiWindowErrors . Bb.uieErrors)
    , B.padTop (B.Pad 1) . B.vLimit 20 $ B.withAttr "infoTitle" $ B.txt "Info Messages"
    , BL.renderList (\_ m -> B.vLimit 1 $ drawLogMsg m) (BF.focusGetCurrent (st ^. Bb.uiWindowErrors . Bb.uieFocus) == Just Bb.NameInfoMsgList) (st ^. Bb.uiWindowErrors . Bb.uieInfos)
    , B.padTop (B.Pad 1) . B.vLimit 20 $ B.withAttr "infoTitle" $ B.txt "Detail"
    , B.vLimit 20 $ BE.renderEditor (B.txt . Txt.unlines) True (st ^. Bb.uiWindowErrors . Bb.uieDetail)
    ]
  where
    drawLogMsg (_id, at', msg, detail) = do
      let
        logAt = DT.utcToLocalTime (st ^. Bb.uiTimeZone) at'
        time = Txt.pack $ DT.formatTime DT.defaultTimeLocale "%H:%M:%S " logAt
      B.txt $ time <> " | " <> Txt.take 100 msg <> " | " <> Txt.take 200 (fromMaybe "" detail)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Default initialiser
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
defaultAppInit :: Bb.UIState ust up uw un ue -> Bb.PendingAction ust up uw un ue
defaultAppInit _st =
  let
    id = UU.fromWords 1463586227 404832366 3211243334 302378666
    name = "init.default"
  in
  Bb.PendingAction id name (pure (identity, []))
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Text popup
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
popTextReg :: (Ord un, Show un) => Bb.PopupReg ust up uw un ue
popTextReg =
  Bb.PopupReg
    { Bb._prDraw = drawPopupText
    , Bb._prEventHandler = eventHandlerPopupText
    }

popTextForText :: Text -> Bb.Popup ust up uw un ue
popTextForText t =
  Bb.PopText t
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
