module Math where

import qualified Graphics.Gloss.Data.Point as P (pointInBox)
import qualified Graphics.Gloss.Data.Vector as V (normalizeV, mulSV, rotateV, angleVV, magV)

type Point = (Float, Float)
type Vector = (Float, Float)
type Circle = (Point, Float)
type Line = (Point, Point)


etalonResolution@(etalonWidth, etalonHeight) = (1920,1080)
aVeryLittleValue = 0.00001

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

cbIntersectionVector :: Circle -> (Point,Point) -> Vector
cbIntersectionVector c ((xf,yf),(xt,yt))
    | lcIntersection ((min xt xf,min yt yf),(max xt xf,min yf yt)) c = (0,-aVeryLittleValue)
    | lcIntersection ((min xt xf,max yt yf),(max xt xf,max yf yt)) c = (0,aVeryLittleValue)
    | lcIntersection ((min xt xf,max yt yf),(min xt xf,min yf yt)) c = (-aVeryLittleValue,0)
    | lcIntersection ((max xt xf,max yt yf),(max xt xf,min yf yt)) c = (aVeryLittleValue,0)
    | otherwise = (0,0)

ccIntersectionVector :: Circle -> Circle -> Vector
ccIntersectionVector (c1,r1) (c2,r2) = if ppDistance c1 c2 < r1 + r2
                                       then V.mulSV c $ V.normalizeV n
                                       else (0,0) where
                                           c = sqrt $ (r1+r2)^2 - (V.magV n)^2
                                           n = ppMakeVector c1 c2

vvScalarMul :: Vector -> Vector -> Vector
vvScalarMul (x1,y1) (x2,y2) = (x1*x2,y1*y2)

vvScalarSum :: Vector -> Vector -> Vector
vvScalarSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)

vvMagicAdd :: Vector -> Vector -> Vector
vvMagicAdd v1 v2 = vvScalarSum v1 $ vvReflectVector v1 v2

vvReflectVector :: Vector -> Vector -> Vector
vvReflectVector (0,0) _ = (0,0)
vvReflectVector s (0,0) = (0,0)
vvReflectVector s@(xs,ys) n@(xn,yn) = V.rotateV (pi-alpha) $ (\x->(x,0)) $ fst $ V.rotateV alpha s where -- mulSV for leaving intersection area
    alpha = signum (maybeZero (-yn) ys) * V.angleVV n (1,0)
    maybeZero 0 y = y
    maybeZero x _ = x

negateV :: Vector -> Vector
negateV (x,y) = (-x,-y)

lcIntersection :: Line -> Circle -> Bool
lcIntersection box@((fx,fy),(tx,ty)) ((x,y),r) = uncurry (pointInBox (x,y)) (extendBox box r) &&
                                                 ((fx-x)^2 + (fy-y)^2 < r^2 ||  -- optimized cpContaining
                                                  (tx-x)^2 + (ty-y)^2 < r^2 ||
                                                  r^2 * (a^2 + b^2) > c^2) where  -- algorithm from emaxx
                                                      c = - ((fx-x)*a + (fy-y)*b)
                                                      a = ty-fy
                                                      b = fx-tx
worldToPage :: Point -> (Int, Int) -> Point
worldToPage (x,y) (rx,ry) = (x*fromIntegral rx/etalonWidth,y*fromIntegral ry/etalonHeight)

pageToWorld :: Point -> (Int, Int) -> Point
pageToWorld (x,y) (rx,ry) = (x*etalonWidth/fromIntegral rx,y*etalonHeight/fromIntegral ry)

notZero x = if x == 0 then aVeryLittleValue else x
notZeroSum x y = if x == -y then signum x * aVeryLittleValue else x + y

pointInBox = P.pointInBox
