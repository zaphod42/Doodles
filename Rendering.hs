module Rendering (increase, repeatedly, forceDisplay, rotateVector, red, green, blue, yellow, purple, cyan, white, xAxis, yAxis, zAxis, vertexAt, vectorTo) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Applicative

rotateVector (Vector3 xAngle yAngle zAngle) = do
  rotate xAngle xAxis
  rotate yAngle yAxis
  rotate zAngle zAxis

forceDisplay = postRedisplay Nothing

bottomLeftCorner = Vertex4 (-1.0) (-1.0) 0.0 1.0

red = rgb 1 0 0
green = rgb 0 1 0
blue = rgb 0 0 1
yellow = rgb 1 1 0
purple = rgb 1 0 1
cyan = rgb 0 1 1
white = rgb 1 1 1

rgb :: Float -> Float -> Float -> Color3 Float
rgb = Color3

xAxis = vectorTo 1 0 0
yAxis = vectorTo 0 1 0
zAxis = vectorTo 0 0 1

vectorTo :: Float -> Float -> Float -> Vector3 Float
vectorTo x y z = Vector3 x y z

vertexAt :: Float -> Float -> Float -> Vertex3 Float
vertexAt x y z = Vertex3 x y z

repeatedly milliseconds callback = do
  addTimerCallback milliseconds $ again milliseconds callback

again milliseconds callback = do
  callback
  addTimerCallback milliseconds $ again milliseconds callback

increase :: Float -> (Vector3 Float) -> (Vector3 Float)
increase amount vector =
  (amount +) <$> vector
