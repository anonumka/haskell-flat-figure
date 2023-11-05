module Shapes where

data Point = Point { x :: Double, y :: Double } deriving (Show)
data BaseInfo = BaseInfo { color :: String, date :: String } deriving (Show)

data Shape = Circle { center :: Point, radius :: Double, base :: BaseInfo }
           | Rectangle { topLeft :: Point, bottomRight :: Point, base :: BaseInfo }
           | Triangle { point1 :: Point, point2 :: Point, point3 :: Point, base :: BaseInfo }
           deriving (Show)

type ShapesList = [Shape]

parseShape :: String -> Shape
parseShape str =
  case words str of
    ["add", "circle", xStr, yStr, radiusStr, colorStr, dateStr] ->
      Circle { center  = Point (read xStr) (read yStr), 
                radius = read radiusStr, 
                base   = BaseInfo (read $ show colorStr :: String) (read $ show dateStr :: String) 
                }

    ["add", "rectangle", x1Str, y1Str, x2Str, y2Str, colorStr, dateStr] ->
      Rectangle { topLeft   = Point (read x1Str) (read y1Str),
                bottomRight = Point (read x2Str) (read y2Str),
                base        = BaseInfo (read $ show colorStr :: String) (read $ show dateStr :: String) 
                }

    ["add", "triangle", x1Str, y1Str, x2Str, y2Str, x3Str, y3Str, colorStr, dateStr] ->
      Triangle { point1 = Point (read x1Str) (read y1Str), 
                 point2 = Point (read x2Str) (read y2Str), 
                 point3 = Point (read x3Str) (read y3Str), 
                 base    = BaseInfo (read $ show colorStr :: String) (read $ show dateStr :: String) 
               }

    _ -> error "Invalid shape"