module Reflex.Box2D.Foreign where

import GHCJS.DOM.Types (HTMLCanvasElement)
import Reflex.Box2D.Types

type WorldToken = ()
type FixtureToken = ()
type BodyToken = ()
type BodyDefToken = ()

-------------------------------------------------------------------------------
-- COMMON
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- WORLD
-------------------------------------------------------------------------------

makeWorld :: Vec2 -> Bool -> IO WorldToken
makeWorld = error "makeWorld only available to ghcjs"

worldSetGravity :: WorldToken -> Vec2 -> IO ()
worldSetGravity w v = error "worldSetGravity only available to ghcjs"

drawSetup :: WorldToken -> HTMLCanvasElement -> IO ()
drawSetup = undefined

worldStep :: WorldToken -> Double -> Int -> Int -> IO ()
worldStep = undefined

drawDebugData :: WorldToken -> IO ()
drawDebugData = undefined

showWorld :: WorldToken -> IO ()
showWorld = undefined

worldCreateBodyAndFixture = undefined

dirtyUpdate = undefined

-------------------------------------------------------------------------------
-- FIXTURE
-------------------------------------------------------------------------------


makeFixture :: BodyToken -> FixtureDef -> IO FixtureToken -- Double -> Double -> Double -> Shape -> IO FixtureToken
makeFixture = error "makeFixture only available to ghcjs"


fixtureDefSetDensity :: FixtureToken -> Double -> IO ()
fixtureDefSetDensity = undefined

fixtureDefSetFriction :: FixtureToken -> Double -> IO ()
fixtureDefSetFriction = undefined

fixtureDefSetRestitution :: FixtureToken -> Double -> IO ()
fixtureDefSetRestitution = undefined

fixtureDefSetShape :: FixtureToken -> Shape -> IO ()
fixtureDefSetShape  = undefined


-------------------------------------------------------------------------------
-- BODY
-------------------------------------------------------------------------------

makeBody :: IO BodyToken -- BodyType -> Double -> Double -> IO BodyToken
makeBody = error "makeBody only available to ghcjs"

makeBodyDef :: IO BodyDefToken
makeBodyDef = error "makeBody only available to ghcjs"

bodyDefSetX :: BodyToken -> Double -> IO ()
bodyDefSetX = undefined
bodyDefSetY :: BodyToken -> Double -> IO ()
bodyDefSetY = undefined
bodyDefGetX :: BodyToken -> IO Double
bodyDefGetX = undefined
bodyDefGetY :: BodyToken -> IO Double
bodyDefGetY = undefined

bodyDefSetTransform :: BodyToken -> Vec2 -> Double -> IO ()
bodyDefSetTransform = undefined

bodyDefSetType = undefined

createBody :: WorldToken -> BodyDef -> IO BodyToken
createBody = undefined

bodyGetPosition :: BodyToken -> IO Vec2
bodyGetPosition = undefined

bodySetPositionAndAngle :: BodyToken -> Vec2 -> Double -> IO ()
bodySetPositionAndAngle  = undefined

bodyApplyImpulse :: BodyToken -> Vec2 -> Vec2 -> IO ()
bodyApplyImpulse  = undefined
