{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BrickBedrock.Model
    ( Name (..)
    , Event (..)
    , Popup (..)
    , Window (..)
    , PendingAction (..)
    , PendingResponse (..)
    , StatusLevel (..)

    , UIOptions (..)
    , uioGlobalKeyHandler
    , uioWindowKeyHandler
    , uioPopupKeyHandler
    , uioUserAttrs
    , uioDrawPopupContainer
    , uioDrawPopupContent
    , uioDrawSpinner
    , uioDrawWindow
    , uioDrawWindowContainer
    , uioStartWindow
    , uioAppInit
    , uioDefaultMessageTimeoutSecs
    , uioAppName
    , uioAppVersion
    , uioDrawStatusBar
    , uioErrorsWindowReg
    , uioHandleUserEvents
    , uioHelpPopupReg
    , uioErrorPopupReg
    , uioTextPopupReg
    , uioKeepLogEntries
    , uioOnError

    , UIState (..)
    , uiTickCount
    , uiWindow
    , uiPopup
    , uiChan
    , uiBlockingActions
    , uiSt
    , uiOptions
    , uiStatusMessage
    , uiTime
    , uiTimeZone
    , uiErrorMessageHistory
    , uiInfoMessageHistory
    , uiPopText
    , uiWindowErrors

    , WindowReg (..)
    , wrDraw
    , wrEventHandler

    , PopupReg (..)
    , prDraw
    , prEventHandler


    , UIErrors (..)
    , uieErrors
    , uieInfos
    , uieFocus
    , uieDetail

    , spinner
    ) where

import           Protolude
import qualified Brick as B
import qualified Brick.AttrMap as BA
import qualified Brick.BChan as BCh
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import           Control.Lens (makeLenses)
import           Data.Time (UTCTime)
import qualified Data.Time as DT
import qualified Data.UUID as UU
import qualified Graphics.Vty as V


spinner :: [Text]
spinner = ["⢀⠀", "⡀⠀", "⠄⠀", "⢂⠀", "⡂⠀", "⠅⠀", "⢃⠀", "⡃⠀", "⠍⠀", "⢋⠀", "⡋⠀", "⠍⠁", "⢋⠁", "⡋⠁", "⠍⠉", "⠋⠉", "⠋⠉", "⠉⠙", "⠉⠙", "⠉⠩", "⠈⢙", "⠈⡙", "⢈⠩", "⡀⢙", "⠄⡙", "⢂⠩", "⡂⢘", "⠅⡘", "⢃⠨", "⡃⢐", "⠍⡐", "⢋⠠", "⡋⢀", "⠍⡁", "⢋⠁", "⡋⠁", "⠍⠉", "⠋⠉", "⠋⠉", "⠉⠙", "⠉⠙", "⠉⠩", "⠈⢙", "⠈⡙", "⠈⠩", "⠀⢙", "⠀⡙", "⠀⠩", "⠀⢘", "⠀⡘", "⠀⠨", "⠀⢐", "⠀⡐", "⠀⠠", "⠀⢀", "⠀⡀"]

data StatusLevel
  = StsError
  | StsInfo
  | StsTrace
  | StsErrorNotify

data Event ust up uw un ue
  = EvtTick
  | EvtAddBlockingAction (PendingAction ust up uw un ue)
  | EvtBlockingResp (PendingResponse ust up uw un ue)
  | EvtSetStatusMessage StatusLevel Text (Maybe Text) (Maybe Int)
  | EvtUser ue

data Name un
  = NameErrorMsgList
  | NameInfoMsgList
  | NameErrorMsgDetail
  | NameUser un
  | NamePopTextEdit
  deriving (Show, Eq, Ord)

data Popup  ust up uw un ue
  = PopHelp
  | PopUser up (Maybe (PopupReg ust up uw un ue))
  | PopError Text (Maybe Text) (Maybe (Popup  ust up uw un ue))
  | PopText Text

data Window ust up uw un ue
  = WUnknown
  | WErrors (Window ust up uw un ue)
  | WUser uw Text (Maybe (WindowReg ust up uw un ue))
  | WUserHist uw Text (Maybe (WindowReg ust up uw un ue)) (Window ust up uw un ue) -- ^ User window with history, i.e. ESC goes back to prev window

data WindowReg ust up uw un ue = WindowReg
  { _wrDraw :: !(Window ust up uw un ue -> UIState ust up uw un ue -> B.Widget (Name un))
  , _wrEventHandler :: !(Window ust up uw un ue -> UIState ust up uw un ue -> B.BrickEvent (Name un) (Event ust up uw un ue) -> B.EventM (Name un) (B.Next (UIState ust up uw un ue)))
  }

data PopupReg ust up uw un ue = PopupReg
  { _prDraw :: !(Popup ust up uw un ue -> UIState ust up uw un ue -> B.Widget (Name un))
  , _prEventHandler :: !(Popup ust up uw un ue -> UIState ust up uw un ue -> B.BrickEvent (Name un) (Event ust up uw un ue) -> B.EventM (Name un) (B.Next (UIState ust up uw un ue)))
  }

data PendingAction ust up uw un ue = PendingAction UU.UUID Text (IO (UIState ust up uw un ue -> UIState ust up uw un ue, [UIState ust up uw un ue -> IO (UIState ust up uw un ue)]))
data PendingResponse ust up uw un ue = PendingResponse UU.UUID Text (UIState ust up uw un ue -> UIState ust up uw un ue, [UIState ust up uw un ue -> IO (UIState ust up uw un ue)])

data UIOptions ust up uw un ue = UIOptions
  { -- | Key handler that is always run first, even when the UI is blocked (spinner showing)
    _uioGlobalKeyHandler :: !(UIState ust up uw un ue -> B.BrickEvent (Name un) (Event ust up uw un ue) -> B.EventM (Name un) (B.Next (UIState ust up uw un ue)))
    -- | Key handler for when the UI is not blocked, and no popup is showing
    -- If you are using WindowReg you do not need to change this
  , _uioWindowKeyHandler :: !(UIState ust up uw un ue -> B.BrickEvent (Name un) (Event ust up uw un ue) -> B.EventM (Name un) (B.Next (UIState ust up uw un ue)))
    -- | Key handler for when a popup is visible
  , _uioPopupKeyHandler :: !(Popup  ust up uw un ue -> UIState ust up uw un ue -> B.BrickEvent (Name un) (Event ust up uw un ue) -> B.EventM (Name un) (B.Next (UIState ust up uw un ue)))
    -- | The conainer window that popups are drawn in. You should not need to change this
  , _uioDrawPopupContainer :: !(Popup  ust up uw un ue -> UIState ust up uw un ue -> B.Widget (Name un))
    -- | Draw the popup content. If you are using WindowReg with PopUser, then you don't need to change this
  , _uioDrawPopupContent :: !(Popup  ust up uw un ue -> UIState ust up uw un ue -> B.Widget (Name un))
    -- | Draw the spinner. You don't need to change this
  , _uioDrawSpinner :: !(UIState ust up uw un ue -> B.Widget (Name un))
    -- | Draw a container for the current window. You do not need to change this
  , _uioDrawWindowContainer :: !(Window ust up uw un ue -> UIState ust up  uw un ue -> B.Widget (Name un) -> B.Widget (Name un))
    -- | Draw the current window. If you are using WindowReg with WUser, then you don't need to change this
  , _uioDrawWindow :: !(Window ust up uw un ue -> UIState ust up  uw un ue -> B.Widget (Name un))
    -- | Draw the status bar. This is drawn at the bottom of the window. If you do not want a bottom status bar, set this to `const B.emptyWidget`
  , _uioDrawStatusBar :: !(UIState ust up  uw un ue -> B.Widget (Name un))
    -- | Window reg for the errors window
  , _uioErrorsWindowReg :: !(WindowReg ust up uw un ue)
    -- | First window to show
  , _uioStartWindow :: !(Window ust up uw un ue)
    -- | Initialisation code, runs in background thread at startup
  , _uioAppInit :: !(UIState ust up uw un ue -> PendingAction ust up uw un ue)
    -- | How long to display status messages for
  , _uioDefaultMessageTimeoutSecs :: !Int
    -- | Application name
  , _uioAppName :: !Text
    -- | Application name
  , _uioAppVersion :: !Text
    -- | Number of log messages to keep in history
  , _uioKeepLogEntries :: !Int
    -- | Called when an error message was sent. Can be used for logging etc
  , _uioOnError :: !(Text -> Maybe Text -> UIState ust up uw un ue -> IO (UIState ust up uw un ue))
    -- | Handler for user events
  , _uioHandleUserEvents :: !(ue -> UIState ust up uw un ue -> IO (UIState ust up uw un ue))
    -- | User defined attributes, overrides the default ones
  , _uioUserAttrs :: ![(BA.AttrName, V.Attr)]
    -- | What to display for help
  , _uioHelpPopupReg :: !(PopupReg ust up uw un ue)
    -- | What to display for error popups
  , _uioErrorPopupReg :: !(PopupReg ust up uw un ue)
    -- | What to display for text popups
  , _uioTextPopupReg :: !(PopupReg ust up uw un ue)
  }

data UIErrors un = UIErrors
  { _uieErrors :: !(BL.List (Name un) (UU.UUID, UTCTime, Text, Maybe Text) )
  , _uieInfos :: !(BL.List (Name un) (UU.UUID, UTCTime, Text, Maybe Text) )
  , _uieFocus :: !(BF.FocusRing (Name un))
  , _uieDetail :: !(BE.Editor Text (Name un))
  }


data UIState ust up uw un ue = UIState
  { _uiTickCount :: !Int
  , _uiWindow :: !(Window ust up uw un ue)
  , _uiPopup :: !(Maybe (Popup  ust up uw un ue))
  , _uiOptions :: !(UIOptions ust up uw un ue)
  , _uiChan :: !(BCh.BChan (Event ust up uw un ue))
  , _uiBlockingActions :: !(Map UU.UUID Text)
  , _uiSt :: !ust
  , _uiStatusMessage :: !(Maybe (StatusLevel, Text, UTCTime))
  , _uiTime :: !DT.LocalTime
  , _uiTimeZone :: !DT.TimeZone
  , _uiWindowErrors :: !(UIErrors un)
  , _uiErrorMessageHistory :: ![(UU.UUID, UTCTime, Text, Maybe Text)]
  , _uiInfoMessageHistory :: ![(UU.UUID, UTCTime, Text, Maybe Text)]
  , _uiPopText :: !(BE.Editor Text (Name un))
  }

makeLenses ''UIState
makeLenses ''UIOptions
makeLenses ''UIErrors
makeLenses ''WindowReg
makeLenses ''PopupReg
