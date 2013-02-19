module PointCloud (prepPoints) where
import Data.Foldable (toList)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word (Word8, Word32)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Linear (V3, M44)

import HeatPalette (heatTexture)
import MyPaths

toGLMat :: Real a => M44 a -> [[GLfloat]]
toGLMat = toList . fmap (toList . fmap realToFrac)

prepPoints :: Vector (V3 Float) -> IO (Vector (V3 Word8), M44 Float -> IO ())
prepPoints v = 
  do vs <- getDataFileName "etc/cloud.vert"
     fs <- getDataFileName "etc/cloud.frag"
     sp <- loadShaderProgram vs fs Nothing
     currentProgram $= Just (program sp)
     let heat = setUniform sp "heat"
         cam = uniformMat (getUniform sp "cam")
         vpos = setAttrib sp "vertexCoord"
     vb <- fromVector ArrayBuffer v
     let iv = V.enumFromN 0 (V.length v) :: V.Vector Word32
     ib <- fromVector ArrayBuffer iv
     bindBuffer ArrayBuffer $= Just vb
     bindBuffer ElementArrayBuffer $= Just ib
     enableAttrib sp "vertexCoord"
     vpos ToFloat vad
     (heatVec, t) <- heatTexture 1024
     let draw mat = 
           do currentProgram $= Just (program sp)
              vertexProgramPointSize $= Enabled
              pointSmooth $= Enabled
              activeTexture $= TextureUnit 0
              heat $ Index1 (0::GLint)
              textureBinding Texture1D $= Just t
              bindBuffer ArrayBuffer $= Just vb
              bindBuffer ElementArrayBuffer $= Just ib
              vpos ToFloat vad
              cam $= toGLMat mat
              drawElements Points (fromIntegral $ V.length v)
                           UnsignedInt offset0
     return (heatVec, draw)
  where vad = VertexArrayDescriptor 3 Float 0 offset0
