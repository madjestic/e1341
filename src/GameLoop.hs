module GameLoop where

import Data.MonadicStreamFunction
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.MSF as TMSF
import Foreign.C.Types
import GHC.Float
import Lens.Micro
import Lens.Micro.Extras
import Linear.Metric
import Linear.Matrix
import Linear.V3
import SDL hiding (Texture, normalize)
import Unsafe.Coerce

import Graphics.RedViz.Camera as C
import Graphics.RedViz.Game
import Graphics.RedViz.Object as O
import Graphics.RedViz.Solvable
import Graphics.RedViz.Transformable
import Graphics.RedViz.Uniforms
import Graphics.RedViz.Widget

gameLoop :: MSF (MaybeT (ReaderT GameSettings (ReaderT Double (StateT Game IO)))) () Bool
gameLoop = runGame `untilMaybe` gameQuit `catchMaybe` exit
  where
    runGame  = arrM (\_ -> (lift . lift) gameLoopStep)
    gameQuit = arrM (\_ -> (lift . lift . lift) gameQuit')

    gameQuit' :: StateT Game IO Bool
    gameQuit' = TMSF.get >>= \s -> return $ quitGame s

    gameLoopStep :: ReaderT Double (StateT Game IO) Bool
    gameLoopStep = do
      TMSF.ask >>= \r ->  liftIO $ delay $ fromIntegral(double2Int $ r * 100)
      --lift updateGame
      TMSF.ask >>= \r -> lift $ updateGame r

    updateGame :: Double -> StateT Game IO Bool
    updateGame r = do
      updateObjects
      updateWidgets
      updateCameras
      updateTick r
      handleEvents
        where
          updateCameras :: StateT Game IO ()
          updateCameras = modify gameCameras
            where
              gameCameras :: Game -> Game
              gameCameras g0 = g0 { cameras = updateCamera <$> cameras g0 }
                where
                  updateCamera :: Camera -> Camera
                  updateCamera cam0 =
                    cam0 { ctransform = solveTransformable (ctransform cam0) }
                      where
                        solveTransformable :: Transformable -> Transformable
                        solveTransformable t0 =
                          --DT.trace ("xform : " ++ show (xform t0))
                          t0 { xform  = foldr1 (!*!) $ solveXform (xform t0) <$> tslvrs t0
                             , tslvrs = updateSolver <$> tslvrs t0 }
                          where
                            updateSolver :: Solvable -> Solvable
                            updateSolver slv =
                              case slv of
                                Identity                  -> slv
                                --Movable  _ pos  vel  ss -> slv { txyz   = pos  + vel  `applySolver` s
                                Movable  _ pos  vel  ss -> slv { txyz   = pos  + foldr (applySolver Identity) vel  ss
                                                                 , kinslv = updateSolver <$> ss }
                                Turnable _ _ _ rxyz avel ss -> slv { rxyz   = rxyz + foldr (applySolver Identity) avel ss
                                                                , kinslv  = updateSolver <$> ss }
                                Fadable l a inc _ _         -> slv { age    = min (a + inc) l}
                                _ -> slv
                                
                            applySolver :: Solvable -> Solvable -> V3 Double -> V3 Double
                            applySolver _ slv1 v0 =
                              case slv1 of
                                Fadable l a _ amp f -> amp * f (l-a) *^ v0
                                _ -> v0
                         
                            solveXform :: M44 Double -> Solvable -> M44 Double
                            solveXform mtx0 slv =
                              case slv of
                                Identity -> identity
                                Movable cs pos _ _ ->
                                  case cs of
                                    WorldSpace  -> identity & translation .~ pos
                                    ObjectSpace -> undefined
                                Turnable _ _ rord rxyz _ _ -> transform' identity
                                  where
                                    transform' :: M44 Double -> M44 Double
                                    transform' mtx0' = mtx
                                      where
                                        mtx =
                                          mkTransformationMat
                                          rot
                                          tr
                                          where
                                            rot    = 
                                              identity !*!
                                              case rord of
                                                XYZ ->
                                                      fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (rxyz^._x)) -- pitch
                                                  !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (rxyz^._y)) -- yaw
                                                  !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (rxyz^._z)) -- roll
                                            tr     = (identity::M44 Double)^.translation
                         
                                Controllable cvel0 ypr0 _ ->
                                  mkTransformationMat rot tr
                                  where
                                    rot = 
                                      (mtx0^._m33) !*!
                                          fromQuaternion (axisAngle (mtx0^.(_m33._x)) (ypr0^._x)) -- pitch
                                      !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (ypr0^._y)) -- yaw
                                      !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (ypr0^._z)) -- roll
                                    tr  = mtx0^.translation + inv33 (mtx0^._m33) !* cvel0
                                Parentable -> identity -- TODO: add inheriting transform from sertParent object
                                ParentableToPlayer -> identity -- TODO: add inheriting transform to current camera
                                _ -> identity
                    
          updateWidgets :: StateT Game IO ()
          updateWidgets = do
            modify solveWidgets
            return ()
              where
                solveWidgets :: Game -> Game
                solveWidgets g0 =
                  g0 {wgts = widgetSolver <$> wgts g0}
                  where
                    widgetSolver :: Widget -> Widget
                    widgetSolver wgt =
                      case wgt of
                        Selector {} ->
                          wgt { objects = filter selected $ objs g0 }
                        _ -> wgt

          updateObjects :: StateT Game IO () -- TODO: finish refactor
          updateObjects = modify gameObjects
            where
              lookedAt :: Camera -> V3 Double -> Double -> Bool
              lookedAt cam centroid radius = s
                where
                  cxform          = xform . ctransform $ cam
                  camera_position = (xform . ctransform $ cam)^.translation
                  camera_lookat   = V3 0 0 (-1) *! cxform^._m33
                  ivec            = normalize $ centroid - camera_position :: V3 Double
                  s               = dot ivec camera_lookat > 1.0 - atan (radius / distance centroid camera_position) / pi

              gameObjects :: Game -> Game
              gameObjects g0 = g0 { objs = updateObject <$> objs g0 }
                where
                  updateObject :: Object -> Object
                  updateObject obj0 =
                    obj0 { transform = solveTransformable (transform obj0)
                         , selected  = lookedAt (head $ cameras g0) (xform (transform obj0)^.translation) 0.1 }
                    where
                      solveTransformable :: Transformable -> Transformable
                      solveTransformable t0 =
                        --DT.trace ("xform : " ++ show (xform t0))
                        t0 { xform  = foldr1 (!*!) $ solveXform (xform t0) <$> tslvrs t0
                           , tslvrs = updateSolver <$> tslvrs t0 }
                        where
                          updateSolver :: Solvable -> Solvable
                          updateSolver slv =
                            case slv of
                              Identity                  -> slv
                              Movable _ pos vel ss ->
                                slv { txyz   = pos  + foldr (applySolver slv) vel  ss
                                    , kinslv = updateSolver <$> ss}
                              Turnable _ _ _ rxyz avel ss ->
                                slv { rxyz   = rxyz + foldr (applySolver slv) avel ss
                                    , kinslv  = updateSolver <$> ss }
                              Fadable l a inc _ _ ->
                                slv { age    = min (a + inc) l}
                              Pullable m0 _  -> slv { acc = gravity }
                                where 
                                  gravity :: V3 Double
                                  gravity = 
                                    foldr (attract obj0) (V3 0 0 0) (filter (\obj' -> O.uuid obj' /= O.uuid obj0) $ objs g0)
                                    where
                                      attract :: Object -> Object -> V3 Double -> V3 Double
                                      attract obj0 obj1 acc0 = acc0 + acc
                                        where
                                          Pullable m1 _  = head ([ x | x@(Pullable _ _ ) <- tslvrs (transform obj1) ])
                                          p0   = (xform.transform $ obj0)^.translation
                                          p1   = (xform.transform $ obj1)^.translation
                                          dir  = p1 ^-^ p0                 :: V3 Double
                                          dist = norm dir                  :: Double
                                          g    = 6.673**(-11.0)            :: Double
                                          f    = g * m0 * m1 / dist**2.0   :: Double
                                          acc  = (f / m0) *^ (dir ^/ dist) :: V3 Double
                                        -- | F = G*@mass*m2/(dist^2);       // Newton's attract equation
                                        -- | a += (F/@mass)*normalize(dir); // Acceleration

                              _ -> slv
                              
                          applySolver :: Solvable -> Solvable -> V3 Double -> V3 Double
                          applySolver _     (Fadable l a _ amp f) v0 = amp * f (l-a) *^ v0
                          applySolver (Movable {}) (Pullable _ a) v0 = v0 + a -- a*dt?
                          applySolver _ _ v0 = v0
                       
                          solveXform :: M44 Double -> Solvable -> M44 Double
                          solveXform mtx0 slv =
                            case slv of
                              Identity -> identity
                              Movable cs pos _ _ ->
                                case cs of
                                  WorldSpace  -> identity & translation .~ pos
                                  ObjectSpace -> undefined
                              Turnable _ _ rord rxyz _ _ -> transform' identity
                                where
                                  transform' :: M44 Double -> M44 Double
                                  transform' mtx0' = mtx
                                    where
                                      mtx =
                                        mkTransformationMat
                                        rot
                                        tr
                                        where
                                          rot    = 
                                            identity !*!
                                            case rord of
                                              XYZ ->
                                                    fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (rxyz^._x)) -- pitch
                                                !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (rxyz^._y)) -- yaw
                                                !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (rxyz^._z)) -- roll
                                          tr     = (identity::M44 Double)^.translation
                       
                              Controllable cvel0 ypr0 _ ->
                                mkTransformationMat rot tr
                                where
                                  rot = 
                                    (mtx0^._m33) !*!
                                        fromQuaternion (axisAngle (mtx0^.(_m33._x)) (ypr0^._x)) -- pitch
                                    !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (ypr0^._y)) -- yaw
                                    !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (ypr0^._z)) -- roll
                                  tr  = mtx0^.translation + inv33 (mtx0^._m33) !* cvel0
                              Pullable mass acc -> identity -- & translation .~ pos
                              Parentable -> identity -- TODO: add inheriting transform from sertParent object
                              ParentableToPlayer -> identity -- TODO: add inheriting transform to current camera
                              _ -> identity

          handleEvents :: StateT Game IO Bool
          handleEvents = do
            events <- SDL.pollEvents
            updateMouse events
            updateKeyboard mapKeyEvents events
            let result = any isQuit $ fmap eventPayload events :: Bool
            --get >>= (liftIO . print)
            return result
              where
                isQuit :: EventPayload -> Bool
                isQuit ev =
                  case ev of
                    KeyboardEvent keyboardEvent -> 
                      keyboardEventKeyMotion keyboardEvent                  == Pressed
                      && keysymScancode (keyboardEventKeysym keyboardEvent) == ScancodeQ
                    QuitEvent -> True
                    _         -> False

                updateMouse  :: [Event] -> StateT Game IO ()
                updateMouse = mapM_ processEvent 
                  where
                    processEvent :: Event -> StateT Game IO ()
                    processEvent e =
                      let mk = case eventPayload e of
                            MouseMotionEvent mouseEvent -> Just (mouseMotionEventRelMotion mouseEvent)
                            _ -> Nothing
                      in case mk of
                        Nothing   -> return ()
                        Just vpos ->
                          mmove (unsafeCoerce vpos)
                          where
                            mmove :: Point V2 CInt -> StateT Game IO ()
                            mmove pos = do
                              modify $ mmove'
                              where
                                mmove' :: Game -> Game
                                mmove' g0 = g0 { cameras = (updateCam $ head (cameras g0)) : tail (cameras g0) }
                                  where
                                    updateCam :: Camera -> Camera
                                    updateCam cam =
                                      cam { ctransform = updateTransformable pos (ctransform cam)}
                                      where
                                        updateTransformable :: Point V2 CInt -> Transformable -> Transformable
                                        updateTransformable _ t0@(Transformable mtx0 _) =
                                          t0
                                          { xform = 
                                              mkTransformationMat
                                              rot
                                              tr
                                          }
                                          where
                                            tr = view translation mtx0
                                            rot = 
                                              (mtx0^._m33)
                                              !*! fromQuaternion (axisAngle (mtx0^.(_m33._x)) (mouseS cam^._x * (fromIntegral $ pos^._y))) -- pitch
                                              !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (mouseS cam^._x * (fromIntegral $ pos^._x))) -- yaw

                updateKeyboard :: (Monad m) => [((Scancode, InputMotion), m ())] -> [Event] -> m ()
                updateKeyboard emap = mapM_ (processEvent emap)
                  where
                    processEvent :: (Monad m) => [((Scancode, InputMotion) , m ())] -> Event -> m ()
                    processEvent mapping e =
                      let
                        mk :: Maybe (Scancode, InputMotion)
                        mk = case eventPayload e of
                               KeyboardEvent keyboardEvent -> Just
                                 ( keysymScancode (keyboardEventKeysym keyboardEvent)
                                 , keyboardEventKeyMotion keyboardEvent )
                               _ -> Nothing
                      in case mk of
                        Nothing     -> return ()
                        Just (sc, km) -> case lookup (sc, km) mapping of
                                          Nothing -> return ()
                                          Just k'  -> k'
                
                mapKeyEvents :: [((Scancode, InputMotion), StateT Game IO ())]
                mapKeyEvents =
                  [ ((ScancodeEscape, Pressed)  , quitE True      )
                  , ((ScancodeW     , Pressed)  , camDolly    (-1))
                  , ((ScancodeW     , Released) , camDolly      0 )
                  , ((ScancodeS     , Pressed)  , camDolly      1 )
                  , ((ScancodeS     , Released) , camDolly      0 )
                  , ((ScancodeQ     , Pressed)  , camRoll       1 )
                  , ((ScancodeQ     , Released) , camRoll       0 )
                  , ((ScancodeE     , Pressed)  , camRoll     (-1))
                  , ((ScancodeE     , Released) , camRoll       0 )
                  , ((ScancodeA     , Pressed)  , camTruck    (-1))
                  , ((ScancodeA     , Released) , camTruck      0 )
                  , ((ScancodeD     , Pressed)  , camTruck      1 )
                  , ((ScancodeD     , Released) , camTruck      0 )
                  , ((ScancodeZ     , Pressed)  , camPedestal ( 1))
                  , ((ScancodeZ     , Released) , camPedestal   0 )
                  , ((ScancodeC     , Pressed)  , camPedestal (-1))
                  , ((ScancodeC     , Released) , camPedestal   0 )
                  ]
                  where
                    updateController :: Camera
                                     -> V3 Double
                                     -> V3 Double
                                     -> Solvable
                                     -> Solvable
                    updateController cam vel0 ypr0 slv0 =
                      case slv0 of
                        ctrl@(Controllable _ _ cyprS) ->
                          ctrl { cvel  = keyboardTS cam * vel0
                               , cypr  = keyboardRS cam * ypr0
                               , cyprS = keyboardRS cam * ypr0 + cyprS } 
                        _ -> slv0

                    camDolly :: Integer -> StateT Game IO ()
                    camDolly n = modify camDolly'
                      where
                        camDolly' :: Game -> Game
                        camDolly' g0 = g0 { cameras = updateCam (head $ cameras g0) : tail (cameras g0) }
                          where
                            updateCam :: Camera -> Camera
                            updateCam cam =
                              cam { ctransform = updateSolvers (ctransform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 0 0 (fromIntegral n)) (V3 0 0 0) <$> tslvrs t0}
                                    
                    camTruck :: Integer -> StateT Game IO ()
                    camTruck n = modify $ camTruck'
                      where
                        camTruck' :: Game -> Game
                        camTruck' g0 = g0 { cameras = (updateCam $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCam :: Camera -> Camera
                            updateCam cam =
                              cam { ctransform = updateSolvers (ctransform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 (fromIntegral n) 0 0) (V3 0 0 0) <$> tslvrs t0}
                                    
                    camPedestal :: Integer -> StateT Game IO ()
                    camPedestal n = modify $ camPedestal'
                      where
                        camPedestal' :: Game -> Game
                        camPedestal' g0 = g0 { cameras = (updateCam $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCam :: Camera -> Camera
                            updateCam cam =
                              cam { ctransform = updateSolvers (ctransform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 0 (fromIntegral n) 0) (V3 0 0 0) <$> tslvrs t0}

                    camRoll :: Integer -> StateT Game IO ()
                    camRoll n = modify $ camRoll'
                      where
                        camRoll' :: Game -> Game
                        camRoll' g0 = g0 { cameras = (updateCam $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCam :: Camera -> Camera
                            updateCam cam =
                              cam { ctransform = updateSolvers (ctransform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 0 0 0) (V3 0 0 (fromIntegral n)) <$> tslvrs t0}
                                               
                    quitE :: Bool -> StateT Game IO ()
                    quitE b = modify $ quit' b
                      where
                        quit' :: Bool -> Game -> Game
                        quit' b' gameLoopDelay' = gameLoopDelay' { quitGame = b' }

          updateTick :: Double -> StateT Game IO ()
          updateTick n = modify $ inc'
            where
              inc' :: Game -> Game
              inc' g0 = g0
                { tick = tick g0 + n 
                , uniforms = incUnis (uniforms g0) }
                where
                  incUnis :: Uniforms -> Uniforms
                  incUnis unis0 = 
                    unis0 { u_time = tick g0 }
