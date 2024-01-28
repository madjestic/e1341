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

import Graphics.RedViz.Entity as E
import Graphics.RedViz.Game
import Graphics.RedViz.Solvable as S
import Graphics.RedViz.Transformable
import Graphics.RedViz.Uniforms
import Graphics.RedViz.Widget as W

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
      updateEntities
      updateWidgets
      updateTick r
      handleEvents
        where
          updateEntities :: StateT Game IO ()
          updateEntities = modify gameEntities
            where
              lookedAt :: Camera -> V3 Double -> Double -> Bool
              lookedAt cam centroid radius = s
                where
                  cxform          = xform . transform $ cam
                  camera_position = (xform . transform $ cam)^.translation
                  camera_lookat   = V3 0 0 (-1) *! cxform^._m33
                  ivec            = normalize $ centroid - camera_position :: V3 Double
                  s               = dot ivec camera_lookat > 1.0 - atan (radius / distance centroid camera_position) / pi

              gameEntities :: Game -> Game
              gameEntities g0 =
                g0 { objs    = updateObject <$> objs g0
                   , cameras = updateCamera <$> cameras g0 }
                where
                  updateObject :: Object -> Object
                  updateObject obj0 = -- DT.trace ("obj0 :" ++ show obj0) $
                    obj0 { transform = solveTransformable obj0 (transform obj0)
                         , selected  = lookedAt (head $ cameras g0) (xform (transform obj0)^.translation) 0.1 }
                  updateCamera :: Camera -> Camera
                  updateCamera cam0 =
                    cam0 { transform = solveTransformable cam0 (transform cam0) }

                  (<>) :: Solvable -> Solvable -> Solvable
                  (<>) slv0@(Movable _ _ v0 _)       (Fadable l a _ amp f) = slv0 { tvel = amp * f (l-a) *^ v0 }
                  (<>) slv0@(Turnable _ _ _ _ av0 _) (Fadable l a _ amp f) = slv0 { avel = amp * f (l-a) *^ av0 }
                  (<>) slv0@(Movable _ _ v0 _)       (Attractable _ a)     = slv0 { tvel = v0 + a }  -- a*dt?
                  (<>) slv0 _ = slv0

                  solveTransformable :: Entity -> Transformable -> Transformable
                  solveTransformable obj0 t0 = -- DT.trace ("tslvrs t0 :" ++ show (tslvrs t0)) $
                    t0 { xform  = foldl1 (!*!) $ solveXform (parentXform $ xform t0) <$> tslvrs t0
                       , tslvrs = updateSolver <$> tslvrs t0 }
                    where
                      parentXform :: M44 Double -> M44 Double
                      parentXform mtx0 =
                        --if (not . null $ parentables) && (not . null $ parents) && (E.parent obj0 /= nil)
                        if (not . null $ parentables) && (not . null $ parents) && parented obj0

                        then (xform . transform $ obj0)&translation .~ (xform . transform . head $ parents)^.translation
                        else mtx0
                        where
                          parents :: [Object]
                          parents = filter (\o -> uuid o == E.parent obj0) (objs g0)

                          parentables :: [Solvable]
                          parentables = ([ x | x@(Parentable {} ) <- tslvrs (transform obj0) ])
                                         
                      solveXform :: M44 Double -> Solvable -> M44 Double
                      solveXform mtx0 slv =
                        case slv of
                          Identity -> identity
                          Constant -> mtx0
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
                                , avel   = avel (foldl (<>) slv (ss ++ (tslvrs.transform $ obj0)))
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
                                  attractors = filter (\obj' -> uuid obj' /= uuid obj0) $ objs g0

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
                          Parentable _ -> -- DT.trace ("Parentable :" ++ show slv) $
                            slv { S.parent = if not . null $ activeObjs then uuid . head $ activeObjs else nil }
                            where
                              activeObjs = [x | x <- filter E.active $ objs g0]

                          _ -> slv                  
 
                      updateVel :: Solvable -> Solvable -> V3 Double -> V3 Double
                      updateVel _ slv1 v0 =
                        case slv1 of
                          Fadable l a _ amp f -> amp * f (l-a) *^ v0
                          _ -> v0

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
                                      cam { transform = updateTransformable pos (transform cam)}
                                      where
                                        updateTransformable :: Point V2 CInt -> Transformable -> Transformable
                                        updateTransformable pos t0@(Transformable _ slvrs0) = t0 { tslvrs = updateControllable <$> tslvrs t0 }
                                          where
                                            updateControllable :: Solvable -> Solvable
                                            updateControllable slv0 = case slv0 of
                                              Controllable cvel0 cypr0 cyprS0 ->
                                                Controllable
                                                { cvel  = 0.01 * keyboardTS cam * cvel0 -- TODO: replace with mouse-specific acceleration sensitivity, rename to "innertia" or smth.
                                                , cypr  = 0.01 * keyboardRS cam * V3 (fromIntegral $ pos^._y) (fromIntegral $ pos^._x) 0
                                                , cyprS = 0.01 * keyboardRS cam * V3 (fromIntegral $ pos^._y) (fromIntegral $ pos^._x) 0 + cyprS0
                                                }
                                              _ -> slv0

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

                  , ((ScancodeW     , Pressed,  False)  , ctrlDolly    (-1))
                  , ((ScancodeW     , Pressed,  True)   , ctrlDolly    (-1))
                  , ((ScancodeW     , Released, False)  , ctrlDolly      0 )

                  , ((ScancodeS     , Pressed,  False)  , ctrlDolly      1 )
                  , ((ScancodeS     , Pressed,  True)   , ctrlDolly      1 )
                  , ((ScancodeS     , Released, False)  , ctrlDolly      0 )

                  , ((ScancodeQ     , Pressed,  False)  , ctrlRoll       1 )
                  , ((ScancodeQ     , Pressed,  True)   , ctrlRoll       1 )
                  , ((ScancodeQ     , Released, False)  , ctrlRoll       0 )

                  , ((ScancodeE     , Pressed,  False)  , ctrlRoll     (-1))
                  , ((ScancodeE     , Pressed,  True)   , ctrlRoll     (-1))
                  , ((ScancodeE     , Released, False)  , ctrlRoll       0 )

                  , ((ScancodeA     , Pressed,  False)  , ctrlTruck    (-1))
                  , ((ScancodeA     , Pressed,  True)   , ctrlTruck    (-1))
                  , ((ScancodeA     , Released, False)  , ctrlTruck      0 )

                  , ((ScancodeD     , Pressed,  False)  , ctrlTruck      1 )
                  , ((ScancodeD     , Pressed,  True)   , ctrlTruck      1 )
                  , ((ScancodeD     , Released, False)  , ctrlTruck      0 )

                  , ((ScancodeZ     , Pressed,  False)  , ctrlPedestal ( 1))
                  , ((ScancodeZ     , Pressed,  True)   , ctrlPedestal ( 1))
                  , ((ScancodeZ     , Released, False)  , ctrlPedestal   0 )

                  , ((ScancodeC     , Pressed,  False)  , ctrlPedestal (-1))
                  , ((ScancodeC     , Pressed,  True)   , ctrlPedestal (-1))
                  , ((ScancodeC     , Released, False)  , ctrlPedestal   0 )

                  , ((ScancodeV     , Pressed,  False)  , ctrlParent False )
                  , ((ScancodeV     , Pressed,  True)   , ctrlParent True  )
                  , ((ScancodeV     , Released, False)  , ctrlUnParent     )
                  ]
                  where
                    updateController :: Entity
                                     -> V3 Double
                                     -> V3 Double
                                     -> Solvable
                                     -> Solvable
                    updateController obj vel0 ypr0 slv0 = 
                        case slv0 of
                          ctrl@(Controllable _ _ cyprS) ->
                            ctrl { cvel  = keyboardTS obj * vel0
                                 , cypr  = keyboardRS obj * ypr0
                                 , cyprS = keyboardRS obj * ypr0 + cyprS } 
                          _ -> slv0


                    ctrlDolly :: Integer -> StateT Game IO ()
                    ctrlDolly n = modify camDolly'
                      where
                        camDolly' :: Game -> Game
                        camDolly' g0 = g0 { cameras = updateCamera (head $ cameras g0) : tail (cameras g0) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam =
                              cam { transform = updateSolvers (transform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController (cam) (V3 0 0 (fromIntegral n)) (V3 0 0 0) <$> tslvrs t0}
                                    
                    ctrlTruck :: Integer -> StateT Game IO ()
                    ctrlTruck n = modify $ camTruck'
                      where
                        camTruck' :: Game -> Game
                        camTruck' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam =
                              cam { transform = updateSolvers (transform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 (fromIntegral n) 0 0) (V3 0 0 0) <$> tslvrs t0}
                                    
                    ctrlPedestal :: Integer -> StateT Game IO ()
                    ctrlPedestal n = modify $ camPedestal'
                      where
                        camPedestal' :: Game -> Game
                        camPedestal' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam =
                              cam { transform = updateSolvers (transform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 0 (fromIntegral n) 0) (V3 0 0 0) <$> tslvrs t0}

                    ctrlRoll :: Integer -> StateT Game IO ()
                    ctrlRoll n = modify $ camRoll'
                      where
                        camRoll' :: Game -> Game
                        camRoll' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam =
                              cam { transform = updateSolvers (transform cam)}
                              where
                                updateSolvers :: Transformable -> Transformable
                                updateSolvers t0 =
                                  t0 { tslvrs = updateController cam (V3 0 0 0) (V3 0 0 (fromIntegral n)) <$> tslvrs t0}

                    ctrlParent :: Bool -> StateT Game IO ()
                    ctrlParent False = modify $ camParent'
                      where
                        camParent' :: Game -> Game
                        camParent' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam  = -- DT.trace ("transform cam : " ++ show (transform cam)) $
                              cam { E.parent  = uid
                                  , parented  = not $ parented cam
                                  , transform = if not $ parented cam then (transform cam) *!* (transform $ head parents) else transform cam}
                              where
                                parents :: [Object]
                                parents = filter (\o -> uuid o == uid) (objs g0)

                                (*!*) :: Transformable -> Transformable -> Transformable
                                (*!*) t0 t1 = t0 {xform = rotY (xform t1) } -- !*! (inv44 . xform $ t1)}

                                rotY :: M44 Double -> M44 Double
                                rotY mtx0 = mtx !*! mtx0
                                  where                                    
                                    mtx =
                                      mkTransformationMat
                                      rot
                                      tr
                                      where
                                        rot = (identity :: M33 Double) !*! 
                                              fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (0))  -- pitch
                                          !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (pi)) -- yaw
                                          !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (0))  -- roll
                                        tr  = mtx0'^.translation
                                        mtx0' = identity :: M44 Double

                                Parentable uid = if not . null $ parentables then head parentables else Parentable nil
                                  where parentables = ([ x | x@(Parentable {} ) <- tslvrs (transform cam) ])                                  

                    ctrlParent True = modify $ camParent'
                      where
                        camParent' :: Game -> Game
                        camParent' g0 = g0 { cameras = (updateCamera $ head (cameras g0)) : (tail (cameras g0)) }
                          where
                            updateCamera :: Camera -> Camera
                            updateCamera cam = -- DT.trace ("transform cam : " ++ show (transform cam)) $
                              cam { E.parent = uid
                                  , parented = True }
                              where
                                parents :: [Object]
                                parents = filter (\o -> uuid o == uid) (objs g0)

                                Parentable uid = if not . null $ parentables then head parentables else Parentable nil
                                parentables = ([ x | x@(Parentable {} ) <- tslvrs (transform cam) ])                  

                    ctrlUnParent :: StateT Game IO ()
                    ctrlUnParent = TMSF.gets $ const () --modify $ camUnParent'
                                               
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
