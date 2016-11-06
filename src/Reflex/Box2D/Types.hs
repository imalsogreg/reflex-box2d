{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module Reflex.Box2D.Types where

import Data.Map
import Data.Default
import GHC.Generics
import Reflex


data ClockSource =
  ClockNever
  | ClockRequestAnimationFrame
  | ClockRegularFrequency Double


data Vec2 = Vec2 { x :: !Double, y :: !Double }
  deriving (Eq, Ord, Show, Generic)


data BodyType = BodyTypeStatic
              | BodyTypeKinematic
              | BodyTypeDynamic
  deriving (Eq, Ord, Enum, Show, Generic)

data Shape = ShapeBox Double Double
           | ShapeCircle Double
           deriving (Eq, Ord, Show, Generic)

data BodyDef = BodyDef
  { bd_type     :: BodyType
  , bd_position :: Vec2
  , bd_rotation :: Double
  } deriving (Eq, Show)

data FixtureDef = FixtureDef
  { fd_density     :: Double
  , fd_friction    :: Double
  , fd_restitution :: Double
  , fd_shape       :: Shape
  } deriving (Eq, Show)
