module Main where

import Example1
import Example2
import Example3
import ShortestPath


main :: IO ()
main = do
    print exampleGraph1
    print (shortestPaths exampleGraph2)
    print (shortestPaths exampleGraph3)
    print (shortestPaths exampleGraph3')
