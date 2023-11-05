{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module List where

import Shapes ( Shape(..), parseShape, color, x, y, ShapesList )

addShape :: String -> ShapesList -> ShapesList
addShape command shapes = parseShape command : shapes

removeShapeColor :: String -> ShapesList -> ShapesList
removeShapeColor figureColor
  = filter
      (\case
  Circle _ _ base -> color base /= figureColor
  Rectangle _ _ base -> color base /= figureColor
  Triangle _ _ _ base -> color base /= figureColor)

removeShapeByCoordinates :: Double -> Double -> ShapesList -> ShapesList
removeShapeByCoordinates x_t y_t
  = filter
      (\case
  Circle center _ _ -> x_t /= x center || y_t /= y center
  Rectangle topLeft bottomRight _ -> x_t < x topLeft || x_t > x bottomRight || y_t < y topLeft || y_t > y bottomRight
  Triangle point1 point2 point3 _ -> x_t == x point1 || x_t == x point2 || x_t == x point3)

printShapes :: ShapesList -> ShapesList
printShapes = mapM_ printShape

printShape :: Shape -> ShapesList
printShape = print
