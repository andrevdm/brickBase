{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BrickBedrock.Utils
    ( findVisibleIndex
    ) where

import           Protolude
import qualified Data.List as Lst


-- | Find next visible index when navigating a list with hidden items
findVisibleIndex :: [a] -> (a -> Bool) -> Int -> Int -> Int
findVisibleIndex [] _ ixFrom _ = ixFrom
findVisibleIndex is' isVis ixFrom ixTo =
  let
    isVis' i = maybe False isVis (atMay is' i)
    isAll = zip [0..] is'
    is = filter (isVis . snd) isAll
    movingUp = ixFrom - ixTo >= 0
  in

  if isVis' ixTo
  then ixTo
  else
    if movingUp
    then
      let
        us = take ixTo isAll
        us' = take ixFrom isAll
        firstVis = maybe ixFrom fst (Lst.find (isVis . snd) is)
      in
      --Find the first item above the index that is visible
      case fst <$> Lst.find (isVis . snd) (reverse us) of
        Just i -> i
        -- Nothing visible above the desired dest, so find the topmost visible item
        Nothing -> maybe firstVis fst (Lst.find (isVis . snd) us')
    else
      let
        ds = drop (ixTo + 1) isAll
        ds' = drop (ixFrom + 1) isAll
        lastVis = maybe ixFrom fst (Lst.find (isVis . snd) (reverse is))
      in
      case fst <$> Lst.find (isVis . snd) ds of
        Just i -> i
        Nothing -> maybe lastVis fst (Lst.find (isVis . snd) (reverse ds'))
