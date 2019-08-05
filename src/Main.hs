module Main where

import           Control.Monad (forM_)
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
  -- The embed function calculates the outputs (effects) of a signal function
  -- for a list of concrete times in the conceptually continous time line
  forM_ (embed (fallingBall 10.0)
                ((),
                [ (0.1, Nothing)
                , (0.1, Nothing)
                , (0.1, Nothing)
                , (0.1, Nothing)
                , (0.1, Nothing)
                , (0.1, Nothing)
                , (0.1, Nothing)
                , (0.1, Nothing)
                , (0.1, Nothing)
                , (0.1, Nothing)
                ])) print
