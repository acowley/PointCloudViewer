{-# LANGUAGE OverloadedStrings, TupleSections #-}
-- |Small helper for saving "cleaned up" point cloud data. The idea is
-- that a PCD file is loaded, some processig is done to it, and we
-- want to save the new point set.
module PCDCleaner where
import qualified Data.Vector.Storable as V
import Linear.V3 (V3(..))
import PCD.Data (saveBinaryPcd)
import PCD.Header (Header(..), DimType(..), DataFormat(..))
import System.FilePath (splitExtension, addExtension)

-- |@saveCleanPCD originalFile newPts@ saves @newPts@ as a new PCD
-- file with name @\"originalFile_clean.pcd\"@. Only XYZ coordinates
-- are saved!
saveCleanPCD :: FilePath -> V.Vector (V3 Float) -> IO ()
saveCleanPCD f v = saveBinaryPcd f' h v
  where f' = let (fn,e) = splitExtension f
             in addExtension (fn++"_clean") e
        n = fromIntegral $ V.length v
        h = Header "0.7" ["x","y","z"] [4,4,4] [F,F,F] [1,1,1] n 1 (0,1) n Binary

cleanPts :: V.Vector (V3 Float) -> (V.Vector (V3 Float), Bool)
cleanPts = (,False)
-- cleanPts = (,True) . V.filter bounds
--   where bounds (V3 _ y _) = y >= -0.08 -- black candlestick
--   where bounds (V3 x y z) = x <= 0.11 && x >= -0.11  -- clean_big_basket
--                          && y <= 0.1 && y >= -0.09
--                          && z <= 0.31 && z >= 0
--   where bounds (V3 x y z) = x <= 0.13 && x >= -0.08 -- large_basket
--                          && y <= 0.1 && y >= -0.11
--                          && z <= 0.31 && z >= 0
