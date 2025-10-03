module Main where

import Player
import Tile

main :: IO ()

t1 = Tile One Zero

t2 = Tile Six Four

p1 = Hand [t1, t2]

main = print p1
