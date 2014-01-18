{-# LANGUAGE Arrows, LambdaCase #-}
module Main where

import Debug.Trace (trace)
import FRP.Netwire
import Prelude hiding ((.), id, null)
import POSP

import Data.Dequeue as DQ

data Light = Green | Yellow | Red           deriving (Show)   
data Car = Passenger Double | Truck Double  deriving (Show)
data PassState = Free | Blocked             deriving (Show, Eq, Ord) 
data PassWay = PassWay PassState Int Double deriving (Show)

light :: POSP a Light
light = for 5 . pure Green --> for 2 . pure Yellow --> for 5 . pure Red --> light 
  
stream :: POSP a (Event Car)
stream = stamp . forever (sometime 1.0 9.0 . oneOf [Passenger, Truck])

queue :: POSP a (BankersDequeue Car)
queue = (hold . accumE pushBack DQ.empty . stream) <|> pure DQ.empty

pass :: POSP (BankersDequeue Car) (Maybe Car)
pass = for 5 . arr (fst . popFront) --> pass . arr (\q -> if not $ null q then snd $ popFront q else q)

wire :: POSP a Car
wire = after 40 . pure undefined <|> holdFor 2 . stream

main :: IO ()
main = testWireM id clockSession_ (pass . queue)