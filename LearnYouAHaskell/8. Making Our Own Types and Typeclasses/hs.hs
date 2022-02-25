data Point  = Point Float Float deriving (Show)
data Shape  = Circle Point Float | Rectangle Point Point deriving (Show)

module Shapes (
    Point(..), --Point 
    Shape(..), --Shape(Rectangle, Circle)
    surface,
    nudge,
    baseCircle,
    baseRect
              ) where
                    surface :: Shape -> Float
                    surface (Circle _ r) = pi * r ^ 2
                    surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

                    nudge :: Shape -> Float -> Float -> Shape
                    nudge (Circle (Point x y) r) a b                    = Circle (Point (x + a) (y + b)) r
                    nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b   = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

                    baseCircle :: Float -> Shape
                    baseCircle radius = Circle (Point 0 0) radius

                    baseRect :: Float -> Float -> Shape
                    baseRect width height = Rectangle (Point 0 0) (Point width height)






main = do
    --let k   = map (Circle 10 20) [4, 5, 6, 7]
    let j   = nudge (baseCircle 7) 
    --print $ surface $ Circle 10 20 10
    print $ j 8 9
