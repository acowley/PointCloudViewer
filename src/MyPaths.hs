{-# LANGUAGE CPP #-}
module MyPaths (getDataFileName) where
#ifdef CABAL
import qualified Paths_PointCloudViewer as P
getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
#else
import System.FilePath

projRoot :: FilePath
projRoot = "/Users/acowley/Documents/Projects/PointCloudViewer"

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (projRoot </>)
#endif


