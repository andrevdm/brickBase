{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BrickBedrock.Controls.ReadOnlyEdit
    ( handleReadOnlyEditorEvent
    ) where

import           Protolude
import qualified Brick as B
import qualified Brick.Widgets.Edit as BE
import           Control.Lens ((%~))
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K


handleReadOnlyEditorEvent
  :: forall t n. (Monoid t)
  => K.Event
  -> BE.Editor t n
  -> B.EventM n (BE.Editor t n)
handleReadOnlyEditorEvent (V.EvKey k ms) e = do
  let fn =
       case (k, ms) of
         (K.KUp, []) -> Z.moveUp
         (K.KDown, []) -> Z.moveDown
         (K.KLeft, []) -> Z.moveLeft
         (K.KRight, []) -> Z.moveRight
         (K.KHome, []) -> Z.gotoBOL
         (K.KEnd, []) -> Z.gotoEOL
         (K.KChar 'k', []) -> Z.moveUp
         (K.KChar 'j', []) -> Z.moveDown
         (K.KChar 'h', []) -> Z.moveLeft
         (K.KChar 'l', []) -> Z.moveRight
         (K.KChar 'G', []) -> Z.gotoEOF
         (K.KChar 'g', []) -> Z.gotoBOF
         _ -> identity

  pure $ e & BE.editContentsL %~ fn

handleReadOnlyEditorEvent _ e = pure e
