#!/usr/bin/env stack
-- stack --resolver lts-3.1 --install-ghc runghc --package turtle

-- When you run this script your cursor will turn into a crosshairs.
-- Click on the window you want to record to begin recording.

{-# LANGUAGE OverloadedStrings #-}

import Turtle

import qualified Control.Foldl as Fold
import qualified Data.Text     as Text

main = do
    lines  <- fold (inproc "xwininfo" [] empty) Fold.list

    let lineMatching :: Text -> IO Text
        lineMatching t = do
            m <- fold (select lines & grep (has (text t))) Fold.head
            case m of
                Nothing -> die (format ("`lineMatching "%w%"` failed") t)
                Just l  -> return l

    let token :: Int -> Text -> IO Int
        token n line = do
            m <- fold (select (Text.words line)) (Fold.index n)
            case m of
                Nothing -> die (format ("`token "%w%" "%w%"` failed") n line)
                Just t  -> case match decimal t of
                    x:_ -> return x
                    _   -> die (format ("`match decimal "%w%"` failed") t)

    line1  <- lineMatching "Width"
    width  <- token 1 line1
    line2  <- lineMatching "Height"
    height <- token 1 line2
    line3  <- lineMatching "Absolute upper-left X"
    x      <- token 3 line3
    line4  <- lineMatching "Absolute upper-left Y"
    y      <- token 3 line4

    -- Because H.264 seems to require even dimensions
    let toEven n = if even n then n else pred n
    
    proc
        "ffmpeg"
        [ "-f", "x11grab"
        , "-s", format (d%"x"%d) (toEven width) (toEven height)
        , "-i", format (":0.0+"%d%","%d) x y
        , "-b", "1m"
        , "output.mp4"
        ]
        empty
