module Reflex.Box2D.Foreign where

import Reflex.Box2D.Types

type WorldToken = ()
type FixtureToken = ()
type BodyToken = ()
type BodyDefToken = ()

makeWorld :: Vec2 -> Bool -> IO WorldToken
makeWorld = error "makeWorld only available to ghcjs"

worldSetGravity :: WorldToken -> Vec2 -> IO ()
worldSetGravity w v = error "worldSetGravity only available to ghcjs"

makeFixture :: IO FixtureToken -- Double -> Double -> Double -> Shape -> IO FixtureToken
makeFixture = error "makeFixture only available to ghcjs"

makeBody :: IO BodyToken -- BodyType -> Double -> Double -> IO BodyToken
makeBody = error "makeBody only available to ghcjs"

makeBodyDef :: IO BodyDefToken
makeBodyDef = error "makeBody only available to ghcjs"

fixtureDefSetDensity :: FixtureToken -> Double -> IO ()
fixtureDefSetDensity = undefined

fixtureDefSetFriction :: FixtureToken -> Double -> IO ()
fixtureDefSetFriction = undefined

fixtureDefSetRestitution :: FixtureToken -> Double -> IO ()
fixtureDefSetRestitution = undefined

fixtureDefSetShape :: FixtureToken -> Shape -> IO ()
fixtureDefSetShape  = undefined

bodyDefSetX = undefined
bodyDefSetY = undefined
bodyDefSetType = undefined
