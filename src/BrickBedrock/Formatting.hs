{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module BrickBedrock.Formatting
    ( showValue
    , stripAnsi
    , unescape
    , stripJSONAnsi
    , tryFormatByteString
    , formatBytes
    ) where

import           Protolude
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as Hm
import qualified Data.Scientific as Sci
import qualified Data.String.AnsiEscapeCodes.Strip.Text as Stp
import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import           Text.Printf (printf)


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Formatting
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
showValue :: Ae.Value -> Text
showValue = \case
  Ae.Array as -> "[" <> Txt.intercalate "," (Vec.toList $ showValue <$> as) <> "]"
  Ae.String s -> s
  Ae.Bool b -> show b
  Ae.Null -> "<null>"
  Ae.Object o -> show $ Hm.toList o <&> \(k,v) -> (k, showValue v)
  Ae.Number n -> case Sci.floatingOrInteger n of
                   Right i -> show i
                   Left f -> Txt.pack $ printf "%0.2f" (f::Double)


stripAnsi :: Text -> Text
stripAnsi = Stp.stripAnsiEscapeCodes . Txt.replace "\\u001B" "\ESC"


unescape :: Text -> Text
unescape = Txt.replace "\\n" "\n" . Txt.replace "\\t" "\t"


stripJSONAnsi :: Ae.Value -> Ae.Value
stripJSONAnsi = \case
  Ae.Array as -> Ae.Array $ stripJSONAnsi <$> as
  Ae.String s -> Ae.String $ stripAnsi s
  Ae.Bool b -> Ae.Bool b
  Ae.Null -> Ae.Null
  Ae.Number n -> Ae.Number n
  Ae.Object o -> Ae.Object $ Hm.map stripJSONAnsi o


tryFormatByteString :: Text -> Text
tryFormatByteString s = maybe s formatBytes (readMaybe (Txt.unpack s))


formatBytes :: Int -> Text
formatBytes bytes =
  go (fromIntegral bytes) ["b ", "kb", "Mb", "Gb"]

  where
    go :: Double -> [Text] -> Text
    go v [] = show v <> "?"
    go v [h] = Txt.pack $ printf "%.1f %s" v h
    go v (h : t) =
      if v >= 1024
         then go (v / 1024) t
         else go v [h]
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
