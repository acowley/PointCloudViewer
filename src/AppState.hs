{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module AppState where
import Control.Applicative
import Control.Lens
import qualified Data.Set as S
import Linear

import Camera
import qualified Renderer as R

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
        -- auxKeyOnce f k = if (k, True) `elem` fst keys then f else id
        dMouse = (\old -> (fromIntegral <$> mousePos ^-^ old) ^* 0.01) <$> prev
        prev' = maybe (const mousePos <$> prev) 
                      (bool (Just mousePos) Nothing)
                      (lookup R.MouseButton0 mouseButtons)

bool :: a -> a -> Bool -> a
bool t _ True = t
bool _ f False = f
