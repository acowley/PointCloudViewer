{-# LANGUAGE CPP #-}
module Internal.MyPaths (getDataFileName) where
#ifdef CABAL
import qualified Paths_PointCloudViewer as P
getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
#else
import System.Directory
import System.FilePath

projRoot :: FilePath
projRoot = "Documents/Projects/PointCloudViewer"

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = (</>projRoot </> f) `fmap` getHomeDirectory 
#endif


