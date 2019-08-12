{-# LANGUAGE Arrows #-}

module Main where

import           Control.Concurrent
import           Data.IORef
import           FRP.Yampa
import qualified FRP.Yampa as Y

------------------------------------------------------------------------------
-- Auxiliary definitions (type aliases)
------------------------------------------------------------------------------

type Position = Double
type Velocity = Double


gravity :: Double
gravity = -9.81

-- The Attenuation type just guarantees that the attenuation lies between
-- 0% and 100%, because only this is physically plausible
newtype Attenuation = Att { getAtt :: Double }

-- Use this attenuation constructor to guarantee sane limits
mkAttenuation :: Double -> Attenuation
mkAttenuation a
  | a < 0.0   = Att 0
  | a > 1.0   = Att 1.0
  | otherwise = Att a


fallingBall :: Position
            -- ^ Initial height
            -> Velocity
            -- ^ Initial velocity
            -> SF () (Position,Velocity)
fallingBall y0 v0 = proc () -> do
  v <- (v0+) ^<< integral -< gravity
  y <- (y0+) ^<< integral -< v
  returnA -< (y,v)


bouncingBall :: Position
             -- ^ Initial height
             -> Velocity
             -- ^ Initial velocity
             -> Attenuation
             -- ^ Loss of kinetic energy in percent (0-100%)
             -> SF () (Position, Velocity)
bouncingBall y0 v0 att =
  switch (fallingBall y0 v0 >>> (Y.identity &&& hitBottom))
         (\(y,v) -> bouncingBall y (attenuate att v) att)


hitBottom :: SF (Position,Velocity) (Event (Position,Velocity))
hitBottom = arr (\(y,v) -> if y<0 && not (epsEq _eps v 0)
                           then Event (0.0,v)
                           else NoEvent)


attenuate :: Attenuation -> Velocity -> Velocity
attenuate (Att att) v = (att-1.0)*v


epsEq eps x y = abs(x-y) < eps

_eps = 1e-6


main :: IO ()
main = do
  putStrLn "Hello FRP!"
  -- The reactimate function provides an event loop that continuously feeds
  -- new events and time increments in our reactive system and sends the output
  -- to a consumer
  reactimate
    (return ())
    (\_ -> do
      threadDelay 1000
      return (0.001, Nothing))
    (\_ (pos,vel) -> do
      putStrLn ("pos: " ++ (show pos) ++ ", vel: " ++ (show vel))
      if epsEq 1e-4 pos 0 && epsEq 1e-2 vel 0
      then return True
      else return False)
    (bouncingBall 10.0 0.0 (mkAttenuation 0.3))
