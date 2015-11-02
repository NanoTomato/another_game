module Math where

import qualified Graphics.Gloss.Data.Point as P (pointInBox)

type Point = (Float, Float)
type Vector = (Float, Float)
type Circle = (Point, Float)
type Line = (Point, Point)


etalonResolution@(etalonWidth, etalonHeight) = (1920,1080)


pvMakeLine :: Point -> Vector -> Line
pvMakeLine f@(fx,fy) (dx,dy) = (f,(fx+dx,fy+dy))

pvAddVector :: Point -> Vector -> Point
pvAddVector (fx,fy) (dx,dy) = (fx+dx,fy+dy)
                               
ppMakeVector :: Point -> Point -> Vector
ppMakeVector (fx,fy) (tx,ty) = (tx-fx,ty-fy)
    
ppDistance :: Point -> Point -> Float
ppDistance (fx,fy) (tx,ty) = sqrt ((fx-tx)^2 + (fy-ty)^2)

cpContaining :: Circle -> Point -> Bool
cpContaining (center, radius) point = radius > ppDistance center point

extendBox :: (Point,Point) -> Float -> (Point,Point)
extendBox ((fx,fy),(tx,ty)) r = let dx = if fx<tx then r else -r
                                    dy = if fy<ty then r else -r
                                in ((fx-dx,fy-dy),(tx+dx,ty+dy))

makeBox :: Point -> (Float, Float) -> (Point, Point)
makeBox (x,y) (w,h) = ((x+w/2,y+h/2),(x-w/2,y-h/2))

lcIntersection :: Line -> Circle -> Bool
lcIntersection box@((fx,fy),(tx,ty)) ((x,y),r) = (uncurry (pointInBox (x,y))) (extendBox box r) && 
                                                 ((fx-x)^2 + (fy-y)^2 < r^2 ||  -- optimized cpContaining
                                                  (tx-x)^2 + (ty-y)^2 < r^2 ||
                                                  r^2 * (a^2 + b^2) > c^2) where  -- algorithm from emaxx
                                                      c = - ((fx-x)*a + (fy-y)*b)
                                                      a = (ty-fy)
                                                      b = (fx-tx)
worldToPage :: Point -> (Int, Int) -> Point
worldToPage (x,y) (rx,ry) = (x*(fromIntegral rx)/etalonWidth,y*(fromIntegral ry)/etalonHeight)

pageToWorld :: Point -> (Int, Int) -> Point
pageToWorld (x,y) (rx,ry) = (x*etalonWidth/(fromIntegral rx),y*etalonHeight/(fromIntegral ry))

pointInBox = P.pointInBox
