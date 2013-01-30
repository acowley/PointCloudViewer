{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Main where
import Control.Applicative
import Control.Lens.TH (makeLenses)
import Control.Lens.Getter (view, (^.))
import Control.Lens.Setter ((.~), (%~), set)
import Control.Monad (when)
import Data.Foldable (toList)
import Data.IORef (newIORef, writeIORef, readIORef)
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import qualified Renderer as R
import Graphics.Rendering.OpenGL hiding (scale)
import Graphics.GLUtil
import Camera
import Linear hiding (trace, translation)
import qualified PCD.Data as PCD
import PCDCleaner
import PointsGL
import VizMarkers
import MyPaths
import HeatPalette
import FrameGrabber
import PointLoader
import System.Console.GetOpt
import System.Directory (canonicalizePath, createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory, takeExtension)

data AppState = AppState { _cam          :: Camera 
                         , _prevMouse    :: Maybe (V2 Int)
                         , _saveDepthmap :: AppState -> IO ()
                         , _showGround   :: Bool }
makeLenses ''AppState

keyActions :: AppState -> [(R.Key, Bool)] -> IO AppState
keyActions s keys 
  | (R.CharKey 'F', True) `elem` keys = (s^.saveDepthmap) s >> return s
  | (R.CharKey 'C', True) `elem` keys = print (s^.cam) >> return s
  | (R.CharKey 'G', True) `elem` keys = return (showGround %~ not $ s)
  | otherwise = return s

cameraControl :: Float -> Double -> R.UIEvents -> AppState -> (Bool, AppState)
cameraControl scale dt (R.UIEvents{..}) st = 
  (stop, ((cam.~c').(prevMouse.~prev')) $ st)
  where c = st^.cam 
        prev = st^.prevMouse
        stop = R.KeyEsc `elem` map fst (fst keys)
        c' = auxKey (go (inc*^forward c)) R.KeyUp
           . auxKey (go ((-inc)*^forward c)) R.KeyDown
           . auxKey (go ((-inc)*^right c)) R.KeyLeft
           . auxKey (go (inc*^right c)) R.KeyRight
           -- . auxKeyOnce (roll (pi*0.5)) R.KeyPageup
           -- . auxKeyOnce (roll (-pi * 0.5)) R.KeyPagedown
           . auxKey (roll 0.04) R.KeyPageup
           . auxKey (roll (-0.04)) R.KeyPagedown
           . maybe id (pan . negate . (^._x)) dMouse
           . maybe id (tilt . negate . (^._y)) dMouse
           . slow 0.9 
           $ update dt c 
        s = 15.0 / scale  -- max speed
        inc = 1.0 / scale -- 0.1
        go = (clampSpeed s .) . deltaV
        auxKey f k = if S.member k (snd keys) then f else id
        auxKeyOnce f k = if (k, True) `elem` fst keys then f else id
        dMouse = (\old -> (fromIntegral <$> mousePos ^-^ old) ^* 0.01) <$> prev
        prev' = maybe (const mousePos <$> prev) 
                      (bool (Just mousePos) Nothing)
                      (lookup R.MouseButton0 mouseButtons)

handler :: Float -> AppState -> Double -> R.UIEvents -> IO (Bool, AppState)
handler scale s dt ui = keyActions s (fst (R.keys ui)) >>= 
                        return . cameraControl scale dt ui

bool :: a -> a -> Bool -> a
bool t _ True = t
bool _ f False = f

data ShaderArgs = ShaderArgs { camMat    :: UniformLocation
                             , heatTex   :: UniformLocation
                             , vertexPos :: AttribLocation
                             , cloudProg :: Program }

initShader :: IO ShaderArgs
initShader = do vs <- loadShader =<< getDataFileName "etc/cloud.vert"
                fs <- loadShader =<< getDataFileName "etc/cloud.frag"
                p <- linkShaderProgram [vs] [fs]
                currentProgram $= Just p
                ShaderArgs <$> get (uniformLocation p "cam")
                           <*> get (uniformLocation p "heat")
                           <*> get (attribLocation p "vertexCoord")
                           <*> pure p

-- |Build a projection matrix.
buildMat :: Float -> Float -> Float -> M44 Float
buildMat s near far = V4 (set _x s 0)
                         (set _y s 0)
                         (V4 0 0 (-2/(far-near)) ((near-far)/(far-near)))
                         (set _z (-s) 0)

loadPoints :: FilePath -> IO (V.Vector (V3 Float))
loadPoints ptFile = aux (takeExtension ptFile)
  where aux ".pcd"  = do pts <- PCD.loadXyz ptFile
                         if V.null pts
                           then V.map (view _xyz) <$> PCD.loadXyzw ptFile
                           else return pts
        aux ".conf" = loadConf ptFile
        aux ".ply"  = V.map (\(SurfacePoint p _ _) -> p) <$> loadPLYfancy ptFile
        aux _       = load3DVerts ptFile

cleanPts :: V.Vector (V3 Float) -> V.Vector (V3 Float)
--cleanPts = id
cleanPts = V.filter bounds
  where bounds (V3 x y z) = x <= 0.13 && x >= -0.08
                         && y <= 0.1 && y >= -0.11
                         && z <= 0.31 && z >= 0

-- Configures OpenGL and returns a drawing function.
setup :: Float -> FilePath -> IO (FilePath -> IO (), AppState -> IO ())
setup scale ptFile = do clearColor $= Color4 1 1 1 0
                        depthFunc $= Just Lequal
                        vertexProgramPointSize $= Enabled
                        pointSmooth $= Enabled
                        --textureFunction $= Decal
                        lighting $= Disabled
                        s <- initShader
                        activeTexture $= TextureUnit 0
                        uniform (heatTex s) $= Index1 (0::GLint)
                        (heatVec, t) <- heatTexture 1024
                        gp <- groundPlane 5 0.1
                        v <- loadPoints ptFile
                        let m = uniformMat (camMat s)
                            proj = buildMat scale 0.01 100.0
                            --v' = V.filter ((< 0.009) . quadrance . view _xy) v
                            -- goodPt (V3 x y z) = let q = quadrance (V2 x y)
                            --                     in q < 0.02 || 
                            --                        (z > 0.1 && q < 0.07)
                            -- v' = V.filter goodPt v
                            v' = cleanPts v
                        saveCleanPCD ptFile v'
                        let printExtents (msg,dim) = 
                              let d = V.map (view dim) v'
                                  low = V.minimum d
                                  high = V.maximum d
                              in do putStr $ "Min "++msg
                                    putStr $ " = "++show low
                                    putStr $ "; Max "++msg
                                    putStrLn $ " = "++show high
                        mapM_ printExtents [("X", _x), ("Y",_y), ("Z",_z)]
                        drawPoints <- prepPoints v' (vertexPos s)
                        let draw st = 
                              let c = st^.cam
                              in do when (st^.showGround)
                                         (gp Z (V3 1 0 0) $
                                             fmap (fmap realToFrac)
                                                  (proj !*! toMatrix c))
                                    currentProgram $= Just (cloudProg s)
                                    m $= (toList . fmap (toList . fmap realToFrac) $
                                          proj !*! toMatrix c)
                                    activeTexture $= TextureUnit 0
                                    uniform (heatTex s) $= Index1 (0::GLuint)
                                    textureBinding Texture1D $= Just t
                                    drawPoints
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
     let renderLoop = loop (handler scale)
                           (\s -> preDraw >> drawCloud s)
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
