{-# LANGUAGE Arrows, RankNTypes #-}
module POSP(
    POSP
  , sometime
  , oneOf
  , onceSwitch
  , forever
  , stamp
  , dupe
  ) where

import Control.Wire.Unsafe.Event
import Control.Wire.Core
import FRP.Netwire
import System.Random

type POSP v v' = (HasTime t s, Fractional t) => Wire s () IO v v'

dupe :: a -> (a, a)
dupe v = (v, v)

sometime :: (HasTime t s, Fractional t)
  => t -> t -> 
  Wire s e IO a (Event a)
sometime t0 t1 =
  mkGen $ \_ _ -> do
    te <- randomRIO (realToFrac t0 :: Double, realToFrac t1)
    return (Right NoEvent, at (fromRational $ toRational te))       

oneOf :: (Monoid e) => [b] -> Wire s e IO a b
oneOf [] = inhibit mempty
oneOf vals = mkGen_ $ \_ -> do
  i <- randomRIO (0, length vals - 1)
  return $ let val = vals !! i in val `seq` Right val
     
onceSwitch :: (Monad m) => Wire s e m a (Event b) -> Wire s e m a (Event b) -> Wire s e m a (Event b)
onceSwitch w1' w2' = 
  WGen $ \ds mx' -> do
    (mx, w1) <- stepWire w1' ds mx'
    mx `seq` return $ case mx of
      Right (Event _) -> (mx, w2')
      _               -> (mx, onceSwitch w1 w2')

forever :: (Monad m) => Wire s e m a (Event b) -> Wire s e m a (Event b) 
forever w = onceSwitch w (forever w)

stamp :: (HasTime t s, Fractional t) => Wire s () IO (Event (Double -> a)) (Event a)
stamp = proc tostamp -> do
  t <- timeF -< ()
  returnA -< fmap (\f -> f t) tostamp