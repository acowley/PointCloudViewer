{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- |Support for points annotated with normal vectors and color.
module Internal.PlySurface where
import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as B
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Linear (V3(..))
import qualified PLY.Data as PLY
import qualified PLY.Types as PLY

-- | A surface point has position, normal, and color.
data SurfacePoint = SurfacePoint (V3 Float) (V3 Float) (V3 Word8)

instance Storable SurfacePoint where
  sizeOf _ = 27
  alignment _ = 27
  peek ptr = SurfacePoint <$> peek (castPtr ptr)
                          <*> peekByteOff (castPtr ptr) 12
                          <*> peekByteOff (castPtr ptr) 24
  poke ptr (SurfacePoint pos nor col) = do poke (castPtr ptr) pos
                                           pokeByteOff (castPtr ptr) 12 nor
                                           pokeByteOff (castPtr ptr) 24 col

loadPLYfancy :: FilePath -> IO (Vector SurfacePoint)
loadPLYfancy f = do Right h <- PLY.loadHeader f
                    putStrLn $ "Loaded PLY header: "
                    print h
                    let Right vs = PLY.loadElements "vertex" h
                    return $ aux vs
  where aux :: Vector (Vector PLY.Scalar) -> Vector SurfacePoint
        aux = B.map (componentsToPt . B.toList)
        componentsToPt :: [PLY.Scalar] -> SurfacePoint
        componentsToPt cpts = 
          let [!x,!y,!z,!nx,!ny,!nz] = map PLY.unsafeUnwrap (take 6 cpts)
              [!r,!g,!b] = map PLY.unsafeUnwrap (drop 6 cpts)
          in SurfacePoint (V3 x y z) (V3 nx ny nz) (V3 r g b)