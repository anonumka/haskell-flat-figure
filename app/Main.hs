module Main where

import System.IO ()

import Shapes ( ShapesList(Empty) )
import CommandList ( processCommands )

main :: IO ()
main = do
  putStrLn "Welcome to the shapes program!"
  commandsList <- readFile "input.txt"
  let commands = lines commandsList
  let figureList = processCommands commands Empty
  return ()
