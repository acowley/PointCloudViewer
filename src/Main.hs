{-# LANGUAGE RankNTypes #-}
module Main where
import Control.Lens (view, (^.), (.~))
import Control.Monad (when)
import Data.IORef (newIORef, writeIORef, readIORef)
import qualified Data.Vector.Storable as V
import qualified Renderer as R
import Graphics.Rendering.OpenGL hiding (scale)
import Linear hiding (trace, translation)
import System.Console.GetOpt
import System.Directory (canonicalizePath, createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import AppState
import Camera
-- import PCDCleaner
import PointCloud (prepPoints)
import GroundGrid (groundPlane, EuclideanGround(Z))
import FrameGrabber
import PointLoader

handler :: Float -> AppState -> Double -> R.UIEvents -> IO (Bool, AppState)
handler scale s dt ui = keyActions s (fst (R.keys ui)) >>= 
                        return . cameraControl scale dt ui

-- |Build a projection matrix.
buildMat :: Float -> Float -> Float -> M44 Float
buildMat s near far = V4 (V4 s 0 0 0)
                         (V4 0 s 0 0)
                         (V4 0 0 (-2/(far-near)) ((near-far)/(far-near)))
                         (V4 0 0 (-s) 0)

printExtents :: (Show a, Ord a, V.Storable a) => V.Vector (V3 a) -> IO ()
printExtents v = mapM_ aux [("X", _x), ("Y", _y), ("Z", _z)]
  where aux (name,dim) = let d = V.map (view dim) v
                         in do putStr $ name++": min  = "++show (V.minimum d)
                               putStrLn $ "; max = "++show (V.maximum d)

-- Configures OpenGL and returns a drawing function.
setup :: Float -> FilePath -> IO (FilePath -> IO (), AppState -> IO ())
setup scale ptFile = do clearColor $= Color4 1 1 1 0
                        depthFunc $= Just Lequal
                        --textureFunction $= Decal
                        lighting $= Disabled
                        gp <- groundPlane 5 0.1
                        v <- loadPoints ptFile
                        let proj = buildMat scale 0.01 100.0
                            --v' = cleanPts v
                            v' = v
                        -- saveCleanPCD ptFile v'
                        printExtents v'
                        (heatVec, drawPoints) <- prepPoints v'
                        let draw st = 
                              let c = st^.cam
                              in do when (st^.showGround)
                                         (gp Z (V3 1 0 0) $
                                             fmap (fmap realToFrac)
                                                  (proj !*! toMatrix c))
                                    drawPoints $ proj !*! toMatrix c
                        return (saveFloatFrame heatVec, draw)

preDraw :: IO ()
preDraw = clear [ColorBuffer, DepthBuffer]

makeFrameSaver :: FilePath -> (FilePath -> IO ()) -> IO (AppState -> IO ())
makeFrameSaver pcdRoot dump = 
  do cnt <- newIORef (1::Int)
     let dir = pcdRoot </> "depthmaps"
         baseName = dir </> "depths"
     dirExists <- doesDirectoryExist dir 
     when (not dirExists)
          (createDirectory dir)
     let f s = do n <- readIORef cnt
                  writeIORef cnt (n+1)
                  dump $ baseName++show n++".bin"
                  writeFile (baseName++show n++"pose.txt")
                            (writePose (_cam s))
     return f

runDisplay :: Float -> FilePath -> IO ()
runDisplay scale pcdFile = 
  do loop <- R.setup
     (dumpDepth, drawCloud) <- setup scale pcdFile
     dumper <- makeFrameSaver (takeDirectory pcdFile) dumpDepth
     occasionally <- R.onlyEvery 3
     rate <- R.rateLimitHz 60
     (incFrame,getFPS) <- R.fps
     let renderLoop = loop (handler scale) (\s -> preDraw >> drawCloud s)
         go frame c = 
           do incFrame
              (shouldExit,c') <- renderLoop c
              occasionally $ putStr "FPS: " >> getFPS >>= print
              if shouldExit
                then R.shutdown
                else rate >> go (frame+1) c'
         startCam = (translation._y .~ -1)
                  . (translation._z .~ 0.2) 
                  $ fpsDefault
     go (0::Int) $ AppState startCam Nothing dumper True

cliOptions :: [OptDescr Float]
cliOptions = [ Option ['s'] ["scale"] (ReqArg read "scale") 
                          "Scale applied to all geometry." ]

main :: IO ()
main = getArgs >>= aux . getOpt Permute cliOptions
  where aux ([],[f],_) = canonicalizePath f >>= runDisplay 1 
        aux ([s],[f],_) = canonicalizePath f >>= runDisplay s
        aux (_,_,errs) = error $
                         concat errs ++ 
                         usageInfo "Usage: pcview [OPTION..] pointFile" cliOptions
