module Main where

import           FRP.Yampa

type Pos = Double
type Vel = Double

-- Type signature of our signal function.
-- It takes an initial position and then animates the ball falling to ground,
-- accelerated by gravity.
fallingBall :: Pos -> SF () (Pos,Vel)
-- First iteration: The ball does not change position (no gravity)
fallingBall pos = constant pos &&& (constant (-9.81) >>> integral)

main :: IO ()
main = do
  putStrLn "Hello FRP!"
  -- The embed function calculates the outputs (effects) of a signal function
  -- for a list of concrete times in the conceptually continous time line
  print $ embed (fallingBall 10.0) ((), [ (0.0, Nothing)
                                        , (0.5, Nothing)
                                        , (1.0, Nothing)
                                        , (1.5, Nothing)
                                        , (2.0, Nothing)
                                        , (2.5, Nothing)
                                        , (3.0, Nothing)
                                        ])
