{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Traversable
import Data.Time
import GHCJS.DOM.Types (castToHTMLCanvasElement)
import Reflex.Dom
import Reflex.Box2D
import Reflex.Box2D.Types
import System.Random

-- temporary for test:
import Reflex.Box2D.Foreign

main :: IO ()
main = setup >>= \w -> mainWidget (run w)

setup :: IO WorldToken
setup = do
  w <- makeWorld (Vec2 0 10) True
  floorFix <- makeFixture
  fixtureDefSetDensity floorFix 1
  fixtureDefSetFriction floorFix 0.5
  fixtureDefSetRestitution floorFix 0.2
  b <- makeBodyDef
  bodyDefSetType b BodyTypeStatic
  bodyDefSetX b 9
  bodyDefSetY b 13
  fixtureDefSetShape floorFix (ShapeBox 10 0.5)
  worldCreateBodyAndFixture w b floorFix
  return w

addSomething :: WorldToken -> IO BodyDefToken
addSomething w = do
  someFix <- makeFixture
  bod <- makeBodyDef
  [x,y,i,a,b] <- forM [0..4] $ \_ -> randomRIO (0,1)
  let shape | i < 0.5   = ShapeBox    (a + 0.1) (b + 0.1)
            | otherwise = ShapeCircle (a + 0.1)
  fixtureDefSetShape someFix shape
  fixtureDefSetDensity someFix 1
  fixtureDefSetDensity someFix 0.5
  fixtureDefSetDensity someFix 0.2
  bodyDefSetType bod BodyTypeDynamic
  bodyDefSetX bod (x*10)
  bodyDefSetY bod (y*10)
  worldCreateBodyAndFixture w bod someFix
  return bod

recenterSomething :: BodyDefToken -> IO ()
recenterSomething t = do
  print "Hi"
  bodyDefSetX t 0
  bodyDefSetY t 0

run :: MonadWidget t m => WorldToken -> m ()
run w = do
  b <- button "Grav"
  d <- button "Recenter"
  canv <- fst <$> elAttr' "canvas" ("id" =: "canvas"
                                   <> "width" =: "500"
                                   <> "height" =:  "600") blank
  pb <- getPostBuild
  t0 <- liftIO getCurrentTime
  ts <- tickLossy (1/60) t0
  ps  <- liftIO newStdGen >>= \g -> poissonLossy g 0.1 t0
  performEvent_ $ (liftIO (randomRIO (-10,10) >>= \g -> worldSetGravity w (Vec2 0 g))) <$ b
  -- performEvent_ (liftIO (js_dirtyDrawSetup w) <$ pb)
  performEvent_ (liftIO (drawSetup w (castToHTMLCanvasElement $ _element_raw canv)) <$ pb)
  performEvent_ (liftIO (js_dirtyUpdate w) <$ ts)
  performEvent_ (liftIO (addSomething w >> return ()) <$ ps)
  c <- liftIO $ addSomething w
  cs <- performEvent_ (liftIO (recenterSomething c) <$ d)
  e <- fmap fst $ el' "div" $ text "Hello"
  performEvent_ (liftIO (js_showWorld w) <$ domEvent Click e)
  display . fmap (id :: Int -> Int) =<< count (domEvent Click e)
