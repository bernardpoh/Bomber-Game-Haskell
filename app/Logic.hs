{-# LANGUAGE TemplateHaskell #-}

module Logic where

import Data.Maybe
import Control.Arrow
import Control.Lens
import Linear.V2
import System.Random
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

class Game g where
    draw::g -> IO Picture
    onEvent::Event -> g -> IO g
    process::Float -> g -> IO g

data World = World {
    _plane :: V2 Float
    , _bomb :: Maybe Bomb
    , _bombCoolDown :: Float
    , _buildings :: [Building]
    , _gameOver :: Bool
    , _score:: Int
}

data Bomb = Bomb {
    _position :: V2 Float
    , _velocity :: V2 Float
}

data Building = Building {
    _height:: Int
    , _number:: Int
}

makeLenses ''World
makeLenses ''Building
makeLenses ''Bomb

makeworld::StdGen->World
makeworld gen = World (V2 1000 700) Nothing 1 b False 0
    where
        (xs, _) = uniformListR 10 (1,6) gen
        b = map (uncurry Building) $ zip xs [0..]

translated::Picture->V2 Float->Picture
translated p (V2 x y) = Translate x y p

drawPlane::V2 Float->Picture
drawPlane = shape & Scale 1 (-1) & translated
    where
        bodyShape = Polygon [(0, 28), (20, 16), (120, 16), (94, 32), (12, 32)] & color red
        wingShape1 = Polygon [(40, 28), (76, 28), (94, 48), (80, 48)] & color grey
        wingShape2 = Polygon [(52, 16), (78, 8), (94, 8), (81, 16)] & color grey
        tailShape = Polygon [(90, 16), (110, 0), (124, 0), (116, 16)] & color grey
        grey = greyN 0.5
        shape = bodyShape <> wingShape1 <> wingShape2 <> tailShape

drawBomb::V2 Float->Picture
drawBomb = Polygon [(0, 0), (10, 0), (5, 5), (10, 10), (10, 20), (5, 22), (0, 20), (0, 10), (5, 5)] 
        & color black & Scale 1 (-1) & translated

drawTimer::Float->Picture
drawTimer t = rectangleSolid (500 * t) 100 & Translate (250*t + 100) 600 & color (greyN 0.8)

drawBuilding::Building->Picture
drawBuilding (Building h n) = rectangleSolid 80 h' & color (dark red) & Translate x (h'/2)
    where
        h' = fromIntegral h * 80
        x = fromIntegral n * 80 + 120

leftEdge, rightEdge, topEdge ::Building->Float
leftEdge (Building _ n) = fromIntegral n * 80 + 80
rightEdge (Building _ n) = fromIntegral n * 80 + 160
topEdge (Building h _) = fromIntegral h * 80

instance Game World where
    draw w = return (timer' <> plane' <> bomb' <> buildings' <> screenText)
            where
                plane' = if w^.gameOver 
                    then Blank 
                    else drawPlane (w^.plane)
                bomb' = w^.bomb <&> view position <&> drawBomb & fromMaybe Blank
                buildings' = Pictures $ map drawBuilding $ w^.buildings
                timer' = drawTimer (w^.bombCoolDown)
                screenText = if w^.gameOver
                    then Text "Game over!" 
                    else Text $ "Score: " ++ show (w^.score)

    onEvent (EventKey (SpecialKey KeySpace) Down _ _) w = return $ w
            & bomb . filtered isNothing .~ Just (Bomb (w^.plane) (V2 0 0))
    onEvent _ w = return w
    process dt w = return $ w
            & plane._x -~ 400 * dt
            & filtered (view (plane._x) >>> (<(-124))).plane +~ V2 1124 (-80)
            & (if any (collided (w^.plane)) (w^.buildings) then gameOver .~ True else id)
            & bomb._Just.velocity -~ V2 0 (150 * dt)
            & bomb._Just %~ (\b -> b & position +~ b^.velocity)
            & foldr (.) id [hitBuilding b | b <- w^.buildings]
            & hitFloor
        where
            collided (V2 x y) b = y < topEdge b && x > leftEdge b && x < rightEdge b
            hitBuilding b@(Building _ n) = if w^.bomb <&> view position <&> (`collided` b) & or
                then (buildings.ix n.height-~1) . (bomb .~ Nothing) . (score +~ 1) else id
            hitFloor = if w^?bomb._Just.position._y <&> (<0) & or then bomb .~ Nothing else id