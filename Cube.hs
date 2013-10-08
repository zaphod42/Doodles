module Cube (renderCube) where
import Rendering

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

renderCube size =
  let posOff = size / 2
      negOff = (-posOff)
      topRightFront    = vertexAt posOff posOff posOff
      bottomRightFront = vertexAt negOff posOff posOff
      topLeftFront     = vertexAt posOff negOff posOff
      bottomLeftFront  = vertexAt negOff negOff posOff
      topRightBack     = vertexAt posOff posOff negOff
      bottomRightBack  = vertexAt negOff posOff negOff
      topLeftBack      = vertexAt posOff negOff negOff
      bottomLeftBack   = vertexAt negOff negOff negOff
  in do
    renderPrimitive Quads $ do
      color red
      vertex topLeftFront
      vertex topRightFront
      vertex bottomRightFront
      vertex bottomLeftFront
    renderPrimitive Quads $ do
      color blue
      vertex topLeftBack
      vertex topRightBack
      vertex bottomRightBack
      vertex bottomLeftBack
    renderPrimitive Quads $ do
      color yellow
      vertex topLeftBack
      vertex topRightBack
      vertex topRightFront
      vertex topLeftFront
    renderPrimitive Quads $ do
      color green
      vertex bottomLeftBack
      vertex bottomRightBack
      vertex bottomRightFront
      vertex bottomLeftFront
    renderPrimitive Quads $ do
      color purple
      vertex topRightFront
      vertex topRightBack
      vertex bottomRightBack
      vertex bottomRightFront
    renderPrimitive Quads $ do
      color cyan
      vertex topLeftFront
      vertex topLeftBack
      vertex bottomLeftBack
      vertex bottomLeftFront
