{-# LANGUAGE Arrows #-}

module Main where

import           Control.Concurrent
import           Data.IORef
import           FRP.Yampa
import qualified FRP.Yampa           as Y
import qualified System.Console.ANSI as T

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


determineRow :: Int -> Position -> Position -> Int
determineRow rowMax pos posMax = truncate (fromIntegral rowMax * pos / posMax) 


push :: a -> [a] -> [a]
push x xs = init (x:xs)


allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs


main :: IO ()
main = do
  (ttyH, _) <- maybe (10, 10) id <$> T.getTerminalSize
  let
    -- Tweak these parameters to change the behavior
    h = ttyH-5
    y0 = 10.0
    v0 = 0.0
    thread_delay = 20000
    time_increment = 0.005
    att = mkAttenuation 0.25
  history <- newIORef [1..30]
  putStrLn "Hello FRP!"
  reactimate
    (return ())
    (\_ -> do
      threadDelay thread_delay
      return (time_increment, Nothing))
    (\_ (pos,vel) -> do
      let
        row = determineRow h pos y0
        before = replicate (h-row) ""
        line = ["     o"]
        after = replicate row ""
        ground = ["TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT"]
        buffer = unlines (before ++ line ++ after ++ ground)
      T.clearScreen
      putStrLn buffer
      xs <- atomicModifyIORef' history (push row &&& id)
      if allEqual (0:xs)
      then do
        putStrLn "Finished"
        return True
      else return False)
    (bouncingBall y0 v0 att)
