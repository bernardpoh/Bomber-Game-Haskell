{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Lens
import System.Random

import Logic

data Canvas = Canvas {
    _windowSize::(Int,Int)
    , _mousePos::(Float,Float)
    , _world::World
}

makeLenses ''Canvas

canvas::(Float,Float)
canvas = (1000,700)

instance Game Canvas where
    draw c = c^.world & draw <&> Translate (-500) (-350) <&> Scale sf sf <&> (rect <>)
        where
            r = canvas^._2/canvas^._1
            (w, h) = c^.windowSize & both %~ fromIntegral
            (nw, nh) = if h / w > r then (w,w*r) else (h/r,h)
            sf = if h / w > r then w/canvas^._1 else h/canvas^._2
            rect = rectangleSolid nw nh & color white
    onEvent (EventResize p) = return . (windowSize.~p)
    onEvent (EventMotion p) = return . (mousePos.~p)
    onEvent k = world . onEvent $ k
    process = world . process

startGame::StdGen->Canvas
startGame gen = Canvas (0,0) (0,0) (makeworld gen)

main :: IO ()
main = do
        seed <- getStdGen
        let initState = startGame seed
        playIO screenDisplay bgColour maxFPS initState draw onEvent process
        where
            screenDisplay = InWindow "Bomber" (500, 500) (0,0)
            bgColour = black
            maxFPS = 60
        