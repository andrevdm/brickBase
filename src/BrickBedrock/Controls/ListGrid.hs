{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module BrickBedrock.Controls.ListGrid
    ( GridFormatter (..)
    , listGridHeader
    , listGridItemFromMap

    ) where

import           Protolude
import           Brick ((<+>))
import qualified Brick as B
import qualified Brick.AttrMap as BA
import           Control.Lens (non, at, (^.))
import qualified Data.Aeson as Ae
import qualified Data.Text as Txt

import qualified BrickBedrock.Formatting as Bbf

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Grid style lists
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data GridFormatter n
  = GFmtText
  | GFmtTextStrip
  | GFmtByteString
  | GFmtByteStringRange [(Int, Text)]
  | GFmtTextFn (Text -> Text)
  | GFmtCustom (Ae.Value -> B.Widget n)
  | GFmtCustomRoot (Map Text Ae.Value -> B.Widget n)


listGridHeader :: [(Text, Text, Int, GridFormatter n)] -> B.Widget n
listGridHeader cols =
  let cs =
        cols <&> \(name, _, width, _) ->
          if width >= 0
            then B.hLimit width (B.txt name <+> B.fill ' ')
            else B.txt name <+> B.fill ' '
   in B.vLimit 1 . B.hBox $ [B.txt "| "] <> intersperse (B.txt " | ") cs <> [B.txt " |"]


listGridItemFromMap :: Map Text Ae.Value -> [(Text, Text, Int, GridFormatter n)] -> B.Widget n
listGridItemFromMap vs cols =
  let cs =
        cols <&> \(_, lookup, width, fmt) ->
          if width >= 0
            then B.hLimit width (format vs fmt (vs ^. at lookup . non Ae.Null) <+> B.fill ' ')
            else format vs fmt (vs ^. at lookup . non Ae.Null) <+> B.fill ' '
   in B.vLimit 1 . B.hBox $ [B.txt "| "] <> intersperse (B.txt " | ") cs <> [B.txt " |"]

   where
     format r f v =
       case f of
         GFmtText -> B.txt $ Bbf.showValue v
         GFmtTextStrip -> B.txt . Txt.replace "\t" " " . Txt.replace "\n" " " . Txt.replace "\r" " " . Txt.take 2000 . Bbf.stripAnsi $ Bbf.showValue v
         GFmtTextFn fn -> B.txt . fn $ Bbf.showValue v
         GFmtCustom fn -> fn v
         GFmtCustomRoot fn -> fn r
         GFmtByteString -> B.txt . Bbf.tryFormatByteString $ Bbf.showValue v
         GFmtByteStringRange rs ->
           let
             bs = fromMaybe 0 . readMaybe . Txt.unpack . Bbf.showValue $ v
             t = Bbf.tryFormatByteString $ Bbf.showValue v
           in
           case headMay $ snd <$> filter (\(i,_) -> bs > i) rs of
             Just attr -> B.withAttr (BA.attrName . Txt.unpack $ attr) $ B.txt t
             Nothing -> B.txt t
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

