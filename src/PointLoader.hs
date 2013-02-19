{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module PointLoader where
import Control.Applicative
import Control.Lens (view)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Linear.V3
import qualified PCD.Data as PCD
import qualified PLY as PLY
import System.FilePath (takeExtension)
import System.IO (withBinaryFile, IOMode(ReadMode), hFileSize, hGetBuf)

-- | Front-end for loading point data. Supported formats are:
--
--   * PCD files containing XYZ points or XYZW points (the W
--   coordinate is discarded)
--
--   * @.conf@ files referencing several @.ply@ files. This format is
--   used by the Stanford 3D scanning repository.
--
--   * PLY files.
-- 
--   * Raw binary files containing triples of single-precision
--   floating point coordinates. This is a raw dump of a 3D point
--   cloud, and is the fastest to load.
loadPoints :: FilePath -> IO (V.Vector (V3 Float))
loadPoints ptFile = aux $ takeExtension ptFile
  where aux ".pcd" = do pts <- PCD.loadXyz ptFile
                        if V.null pts
                          then V.map (view _xyz) <$> PCD.loadXyzw ptFile
                          else return pts
        aux ".conf" = either error id <$> PLY.loadConfV3 "vertex" ptFile
        aux ".ply"  = either error id <$> PLY.loadElementsV3 "vertex" ptFile
        aux _       = loadRaw3D ptFile

loadRaw3D :: FilePath -> IO (Vector (V3 Float))
loadRaw3D f = withBinaryFile f ReadMode $ \h ->
                 do sz <- fromIntegral `fmap` hFileSize h
                    fp <- mallocForeignPtrBytes sz
                    _ <- withForeignPtr fp $ \ptr ->
                           hGetBuf h ptr sz
                    return $ V.unsafeFromForeignPtr0 fp (sz `quot` 12)
