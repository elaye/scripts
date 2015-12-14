#!/usr/bin/env stack
-- stack --resolver lts-3.1 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

import qualified Control.Foldl as Fold
import qualified Data.Text     as Text
import Prelude hiding (FilePath)

parser :: Parser (FilePath, Bool, Maybe FilePath)
parser = (,,) 
  <$> argPath "video" "The video to convert"
  <*> switch "optimize" 'o' "Optimize GIF"
  <*> optional (argPath "output" "The output GIF")

main = do
  (vid, bOptimize, mOut) <- options "A simple video to GIF converter" parser

  shell "mkdir -p .tmp_gif" empty

  -- Extract frames from video
  proc
    "ffmpeg"
    [ "-t", "5"
    , "-ss", "00:00:00"
    , "-i", format fp vid
    , ".tmp_gif/out%04d.gif"
    ]
    empty

  -- Make animated GIF with frames

  proc
    "convert"
    [ "-delay", "1x20"
    , "-loop", "0"
    , ".tmp_gif/out*.gif"
    , "animation.gif"
    ]
    empty

  case bOptimize of
    True -> shell "convert -layers Optimize animation.gif animation_small.gif" empty
    False -> return ExitSuccess

  shell "rm -rf .tmp_gif" empty
