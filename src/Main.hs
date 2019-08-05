module Main where

import           Control.Concurrent
import           Control.Monad      (forM_)
import           FRP.Yampa

type Pos = Double
type Vel = Double

-- Type signature of our signal function.
-- It takes an initial position (s0) and the intial velocity (v0) and then
-- animates the ball falling to the ground, accelerated by gravity.
fallingBall :: Pos -> SF () (Pos,Vel)
fallingBall y0 = (constant (-9.81) >>> integral) >>> ((integral >>^ (+y0)) &&& identity)
--fallingBall y0 = constant 1 >>> integral >>> (arr (\t -> y0 - 4.905 * t**2) &&& arr (*(-9.81)))

main :: IO ()
main = do
  putStrLn "Hello FRP!"
  -- The reactimate function provides an event loop that continuously feeds
  -- new events and time increments in our reactive system and sends the output
  -- to a consumer
  reactimate
    (return ())
    (\_ -> threadDelay 100000 >> return (0.1, Nothing))
    (\_ (pos,vel) -> putStrLn ("pos: " ++ (show pos) ++ ", vel: " ++ (show vel)) >> return False)
    (fallingBall 10.0)
