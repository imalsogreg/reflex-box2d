{-# language FlexibleContexts  #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes        #-}
{-# language RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid            ((<>))
import qualified Data.Text as T
import Data.Traversable
import Data.Time
import GHCJS.DOM.Types        (castToHTMLCanvasElement)
import Reflex.Dom
import Reflex.Box2D
import Reflex.Box2D.Types
import System.Random
import Text.Read

-- temporary for test:
import Reflex.Box2D.Foreign

main = m'

m' :: IO ()
m' = mainWidget run

------------------------------------------------------------------------------
run :: forall t m.SupportsReflexBox2D t m => m ()
run = do
  t0 <- liftIO getCurrentTime
  pb <- getPostBuild
  running <- toggle False =<< button "Running"
  printW  <- button "Printing World"
  lowerFloor <- button "LowerFloor"
  angI <- textInput def { _textInputConfig_attributes = constDyn
                          ("type" =: "range" <> "min" =: "-20" <> "max" =: "20")
                        }
  -- let ang = fmapMaybe id $ (readMaybe . T.unpack) <$> updated (value angI)
  floorY <- foldDyn (\e acc -> acc - e) (10 :: Int) ((-1) <$ lowerFloor)
  moveBall <- foldDyn (\e acc -> acc + e) (0 :: Int) . (1 <$) =<< button "MoveBall"
  bumps <- tickLossy 2 t0
  (canv, _) <- elAttr' "canvas"
    ("id"     =: "canvas" <> "width"  =: "500"    <> "height" =: "500") blank
  let canvEl = castToHTMLCanvasElement $ _element_raw canv

  let wConf = def { worldConfig_physicsRunning = current running
                  , worldConfig_drawRunning    = current running
                  }

  (w,bs) <- world' (Just canvEl) wConf $ \wrl -> mdo

    let floorDef = BodyConfig BodyTypeStatic (Vec2 5 10, 0)
                   (ffor (updated $ body_position ball) $ \(Vec2 ballX ballY, ballR) ->
                             (\_ -> (Vec2 5 10, negate (ballX - 10) / 50)))
                   -- (ffor ang $ \a -> \_ -> (Vec2 5 10, a))
                   never
    (floor,_) <- body' (world_draw_ticks wrl) (world_token wrl) floorDef $ \b -> do
      fixture b def { fixtureConfig_initialShape = ShapeBox 20 1
                    }

    let ballDef = BodyConfig BodyTypeDynamic (Vec2 10 3, 0)
                  never
                  (ffor bumps $ \_ -> \(Vec2 x y,_) -> (Vec2 0.1 (-10), Vec2 x y))
    (ball, _) <- body' (world_draw_ticks wrl) (world_token wrl) ballDef $ \b -> do
      fixture b def { fixtureConfig_initialShape = ShapeCircle 0.6 }
    -- display $ body_position ball
    return ()

  liftIO (drawSetup (world_token w ) canvEl)
  performEvent $ liftIO (showWorld $ world_token w) <$ printW

  return ()

