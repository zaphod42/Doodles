module Omniscient (renderUniverse, interestingPoint) where

import Graphics.Rendering.OpenGL
import Rendering
import Life
import Cube
import Data.Foldable
import Data.Set

renderUniverse universe = Data.Foldable.mapM_ (cubeAt) universe

cubeAt (Location x y) = preservingMatrix $ do 
    translate (vectorTo (toFloat x) (toFloat y) 0)
    renderCube 1.0 

interestingPoint universe = toVertex3 $ (Data.Set.fold (+) (fromInteger 0) universe) `Life.div` (fromInteger $ toInteger $ Data.Set.size universe)

toVertex3 (Location x y) = (Vertex3 (toFloat x) (toFloat y) 0)

toFloat x = fromIntegral x
