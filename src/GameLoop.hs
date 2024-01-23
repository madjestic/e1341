module GameLoop where

import Data.MonadicStreamFunction
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.MSF as TMSF
import Data.UUID (nil)
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
import Graphics.RedViz.Solvable as S
import Graphics.RedViz.Transformable
import Graphics.RedViz.Uniforms
import Graphics.RedViz.Widget

import Debug.Trace as DT

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
                          t0 { xform  = foldr1 (!*!) $ solveXform (parentXform $ xform t0) <$> tslvrs t0
                             , tslvrs = updateSolver <$> tslvrs t0 }
                          where
                            updateSolver :: Solvable -> Solvable
                            updateSolver slv = -- DT.trace ("updateSolver :" ++ show slv) $
                              case slv of
                                Identity
                                  -> -- DT.trace ("Identity") $
                                  slv
                                Movable  _ pos  vel  ss -> -- DT.trace ("Movable") $
                                  slv
                                  { txyz   = pos  + foldr (updateVel Identity) vel  ss
                                  , kinslv = updateSolver <$> ss }
                                Turnable _ _ _ rxyz avel ss -> -- DT.trace ("Turnable") $
                                  slv
                                  { rxyz   = rxyz + foldr (updateVel Identity) avel ss
                                  , kinslv  = updateSolver <$> ss }
                                Fadable l a inc _ _         -> -- DT.trace ("Foldable") $
                                  slv { age    = min (a + inc) l}
                                Parentable _ -> -- DT.trace ("Parentable :" ++ show slv) $
                                  slv { S.parent = if not . null $ activeObjs then O.uuid . head $ activeObjs else nil }
                                  where
                                    activeObjs = [x | x <- filter (O.active) $ objs g0]
                                --Controllable {} -> undefined
                                _ -> -- DT.trace ("_ :") $
                                  slv
                                
                            updateVel :: Solvable -> Solvable -> V3 Double -> V3 Double
                            updateVel _ slv1 v0 =
                              case slv1 of
                                Fadable l a _ amp f -> amp * f (l-a) *^ v0
                                _ -> v0
                         
                            parentXform :: M44 Double -> M44 Double
                            parentXform mtx0 =
                              if (not . null $ parentables) && (not . null $ parents) && (C.parent cam0 /= nil)
                              then (xform . ctransform $ cam0)&translation .~ (xform . transform . head $ parents)^.translation
                              else mtx0
                              where
                                parents :: [Object]
                                parents = filter (\o -> O.uuid o == C.parent cam0) (objs g0)

                                parentables :: [Solvable]
                                parentables = ([ x | x@(Parentable {} ) <- tslvrs (ctransform cam0) ])

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
                                Parentable _ -> identity
  
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

          updateObjects :: StateT Game IO ()
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
              gameObjects g0 =
                g0 { objs = updateObject <$> objs g0 }
                where
                  updateObject :: Object -> Object
                  updateObject obj0 = -- DT.trace ("obj0 :" ++ show obj0) $
                    obj0 { transform = solveTransformable (transform obj0)
                         , selected  = lookedAt (head $ cameras g0) (xform (transform obj0)^.translation) 0.1 }
                    where
                      (<>) :: Solvable -> Solvable -> Solvable
                      (<>) slv0@(Movable _ _ v0 _)       (Fadable l a _ amp f) = slv0 { tvel = amp * f (l-a) *^ v0 }
                      (<>) slv0@(Turnable _ _ _ _ av0 _) (Fadable l a _ amp f) = slv0 { avel = amp * f (l-a) *^ av0 }
                      (<>) slv0@(Movable _ _ v0 _)       (Attractable _ a)     = slv0 { tvel = v0 + a }  -- a*dt?
                      (<>) slv0 _ = slv0

                      solveTransformable :: Transformable -> Transformable
                      solveTransformable t0 = -- DT.trace ("tslvrs t0 :" ++ show (tslvrs t0)) $
                        t0 { xform  = foldr1 (!*!) $ solveXform (xform t0) <$> tslvrs t0
                           , tslvrs = updateSolver <$> tslvrs t0 }
                        where
                          updateSolver :: Solvable -> Solvable
                          updateSolver slv = -- DT.trace ("slv :" ++ show slv) $
                            case slv of
                              Identity             -> slv
                              Movable _ pos vel0 ss   ->
                                slv { txyz   = pos  + vel0
                                    , tvel   = tvel (foldl (<>) slv (ss ++ (tslvrs.transform $ obj0)))
                                    , kinslv = updateSolver <$> ss}
                              Turnable _ _ _ rxyz avel0 ss ->
                                slv { rxyz   = rxyz + avel0
                                    , avel   = avel (foldr (<>) slv (ss ++ (tslvrs.transform $ obj0)))
                                    , kinslv  = updateSolver <$> ss }
                              Fadable l a inc _ _ ->
                                slv { age    = min (a + inc) l}
                              Attractable m0 _ -> slv { acc = gravity }
                                where 
                                  gravity :: V3 Double
                                  gravity =
                                    if not.null $ attractors then foldr (attract obj0) (V3 0 0 0) attractors else V3 0 0 0
                                    where
                                      attractors :: [Object]
                                      attractors = filter (\obj' -> O.uuid obj' /= O.uuid obj0) $ objs g0

                                      attract :: Object -> Object -> V3 Double -> V3 Double
                                      attract obj0 obj1 acc0 = -- DT.trace ("obj0 :" ++ show obj0) $
                                        acc0 + acc'
                                        where
                                          attractables = ([ x | x@(Attractable {} ) <- tslvrs (transform obj1) ])
                                          Attractable m1 _  = if not.null $ attractables then head attractables else Attractable 0 (V3 0 0 0)
                                          p0   = (xform.transform $ obj0)^.translation
                                          p1   = (xform.transform $ obj1)^.translation
                                          dir  = p1 ^-^ p0                 :: V3 Double
                                          dist = norm dir                  :: Double
                                          g    = 6.673**(-11.0)            :: Double
                                          f    = g * m0 * m1 / dist**2.0   :: Double
                                          acc' = case compare dist 0 of
                                            EQ -> 0
                                            _  -> (f / m0) *^ (dir ^/ dist)
                                        -- | F = G*@mass*m2/(dist^2);       // Newton's attract equation
                                        -- | a += (F/@mass)*normalize(dir); // Acceleration

                              _ -> slv                                                        
                       
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
                              --Attractable mass acc -> identity & translation .~ V3 0 0 0.1 -- acc -- identity -- & translation .~ pos
                              Parentable {} -> identity
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
                                mmove' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : tail (cameras g0) }
                                  where
                                    updateCamera :: Camera -> Camera
                                    updateCamera cam =
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

                updateKeyboard :: (Monad m) => [((Scancode, InputMotion, Bool), m ())] -> [Event] -> m ()
                updateKeyboard emap = mapM_ (processEvent emap)
                  where
                    processEvent :: (Monad m) => [((Scancode, InputMotion, Bool) , m ())] -> Event -> m ()
                    processEvent mapping e =
                      let
                        mk :: Maybe (Scancode, InputMotion, Bool)
                        mk = case eventPayload e of
                               KeyboardEvent keyboardEvent -> Just
                                 ( keysymScancode (keyboardEventKeysym keyboardEvent)
                                 , keyboardEventKeyMotion keyboardEvent
                                 , keyboardEventRepeat keyboardEvent )
                               _ -> Nothing
                      in case mk of
                        Nothing     -> return ()
                        Just (sc, km, b) -> case lookup (sc, km, b) mapping of
                                          Nothing -> return ()
                                          Just k'  -> k'
                
                mapKeyEvents :: [((Scancode, InputMotion, Bool), StateT Game IO ())]
                mapKeyEvents =
                  [ ((ScancodeEscape, Pressed,  False)  , quitE True )
                  , ((ScancodeEscape, Pressed,  True )  , quitE True )

                  , ((ScancodeW     , Pressed,  False)  , camDolly    (-1))
                  , ((ScancodeW     , Pressed,  True)   , camDolly    (-1))
                  , ((ScancodeW     , Released, True)   , camDolly      0 )

                  , ((ScancodeS     , Pressed,  False)  , camDolly      1 )
                  , ((ScancodeS     , Pressed,  True)   , camDolly      1 )
                  , ((ScancodeS     , Released, False)  , camDolly      0 )

                  , ((ScancodeQ     , Pressed,  False)  , camRoll       1 )
                  , ((ScancodeQ     , Pressed,  True)   , camRoll       1 )
                  , ((ScancodeQ     , Released, False)  , camRoll       0 )

                  , ((ScancodeE     , Pressed,  False)  , camRoll     (-1))
                  , ((ScancodeE     , Pressed,  True)   , camRoll     (-1))
                  , ((ScancodeE     , Released, False)  , camRoll       0 )

                  , ((ScancodeA     , Pressed,  False)  , camTruck    (-1))
                  , ((ScancodeA     , Pressed,  True)   , camTruck    (-1))
                  , ((ScancodeA     , Released, False)  , camTruck      0 )

                  , ((ScancodeD     , Pressed,  False)  , camTruck      1 )
                  , ((ScancodeD     , Pressed,  True)   , camTruck      1 )
                  , ((ScancodeD     , Released, False)  , camTruck      0 )

                  , ((ScancodeZ     , Pressed,  False)  , camPedestal ( 1))
                  , ((ScancodeZ     , Pressed,  True)   , camPedestal ( 1))
                  , ((ScancodeZ     , Released, False)  , camPedestal   0 )

                  , ((ScancodeC     , Pressed,  False)  , camPedestal (-1))
                  , ((ScancodeC     , Pressed,  True)   , camPedestal (-1))
                  , ((ScancodeC     , Released, False)  , camPedestal   0 )

                  , ((ScancodeV     , Pressed,  False)  , camParent False )
                  , ((ScancodeV     , Pressed,  True)   , camParent True  )
                  , ((ScancodeV     , Released, False)  , camUnParent     )
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
                        camDolly' g0 = g0 { cameras = updateCamera (head $ cameras g0) : tail (cameras g0) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam =
                              cam { ctransform = updateSolvers (ctransform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 0 0 (fromIntegral n)) (V3 0 0 0) <$> tslvrs t0}
                                    
                    camTruck :: Integer -> StateT Game IO ()
                    camTruck n = modify $ camTruck'
                      where
                        camTruck' :: Game -> Game
                        camTruck' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam =
                              cam { ctransform = updateSolvers (ctransform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 (fromIntegral n) 0 0) (V3 0 0 0) <$> tslvrs t0}
                                    
                    camPedestal :: Integer -> StateT Game IO ()
                    camPedestal n = modify $ camPedestal'
                      where
                        camPedestal' :: Game -> Game
                        camPedestal' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam =
                              cam { ctransform = updateSolvers (ctransform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 0 (fromIntegral n) 0) (V3 0 0 0) <$> tslvrs t0}

                    camRoll :: Integer -> StateT Game IO ()
                    camRoll n = modify $ camRoll'
                      where
                        camRoll' :: Game -> Game
                        camRoll' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam =
                              cam { ctransform = updateSolvers (ctransform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 0 0 0) (V3 0 0 (fromIntegral n)) <$> tslvrs t0}

                    camParent :: Bool -> StateT Game IO ()
                    camParent justPressed = modify $ camFollow'
                      where
                        camFollow' :: Game -> Game
                        camFollow' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam = -- DT.trace ("ctransform cam : " ++ show (ctransform cam)) $
                              cam { C.parent = uid
                                  , ctransform = if not justPressed then ((ctransform cam) { xform = rotY90 !*! (xform.ctransform $ cam)}) else (ctransform cam) }
                              where
                                rotY90 :: M44 Double
                                rotY90 = mtx
                                  where                                    
                                    mtx =
                                      mkTransformationMat
                                      rot
                                      tr
                                      where
                                        rot = (identity :: M33 Double) !*! 
                                              fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (0))     -- pitch
                                          !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (-pi/2)) -- yaw
                                          !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (0))     -- roll
                                        tr  = mtx0'^.translation
                                        mtx0' = identity :: M44 Double

                                Parentable uid = if not . null $ parentables then head parentables else Parentable nil
                                  where parentables = ([ x | x@(Parentable {} ) <- tslvrs (ctransform cam) ])                                  

                    camUnParent :: StateT Game IO ()
                    camUnParent = modify $ camFollow'
                      where
                        camFollow' :: Game -> Game
                        camFollow' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam = -- DT.trace ("ctransform cam : " ++ show (ctransform cam)) $
                              cam { C.parent = nil }
                                               
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

stepOnce :: State Game ()
stepOnce = modify gameObjects
  where
    gameObjects :: Game -> Game
    gameObjects g0 = g0 { objs = updateObject <$> objs g0 }
      where
        updateObject :: Object -> Object
        updateObject obj0 =
          obj0 { transform = solveTransformable (transform obj0) }
          where
            solveTransformable :: Transformable -> Transformable
            solveTransformable t0 =
              t0 { xform  = foldr1 (!*!) $ solveXform (xform t0) <$> tslvrs t0
                 , tslvrs = updateSolver <$> tslvrs t0 }
              where
                updateSolver :: Solvable -> Solvable
                updateSolver slv =
                  case slv of
                    Identity             -> slv
                    Movable _ pos _ ss   ->
                      slv { txyz   = pos }
                    Turnable _ _ _ rxyz _ ss ->
                      slv { rxyz   = rxyz }
                    _ -> slv                                                        
             
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
                    _ -> identity
