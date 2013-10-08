module Omniscient (renderUniverse, interestingPoint) where

import Graphics.Rendering.OpenGL
import Rendering
import Life
import Cube
import Data.Foldable

renderUniverse universe = Data.Foldable.mapM_ (cubeAt) universe

cubeAt (Location x y) = preservingMatrix $ do 
    translate (vectorTo (toFloat x) (toFloat y) 0)
    renderCube 1.0 

interestingPoint (Location x y) = (Vertex3 (toFloat x) (toFloat y) 0)

toFloat x = fromIntegral x
