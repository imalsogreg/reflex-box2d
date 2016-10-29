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


data BodyType = BodyTypeStatic | BodyTypeDynamic
  deriving (Eq, Ord, Enum, Show, Generic)

data Shape = ShapeBox Double Double
           | ShapeCircle Double
           deriving (Eq, Ord, Show, Generic)
