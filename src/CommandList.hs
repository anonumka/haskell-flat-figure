module CommandList where

import Shapes ( Shape, ShapesList )
import List ( addShape, printShapes, removeShapeColor, removeShapeByCoordinates)

executeCommand :: Maybe (ShapesList -> ShapesList) -> ShapesList -> ShapesList
executeCommand Nothing shapes = shapes
executeCommand (Just command) shapes = command shapes

processCommands :: [String] -> ShapesList -> ShapesList
processCommands commands shapes
  = foldl
      (\ shapes command -> executeCommand (parseCommand command) shapes)
      shapes commands

parseCommand :: String -> Maybe (ShapesList -> ShapesList)
parseCommand command =
  case words command of
    ["add", shapeType, args] -> Just (addShape (shapeType ++ " " ++ args))
    ["rem", figureColor] -> Just (removeShapeColor figureColor)
    ["rem", "*", x, y] -> Just (removeShapeByCoordinates (read x) (read y))
    ["print"] -> just (printShapes -> return ())
    _ -> Nothing