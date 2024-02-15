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
import Linear.Metric
import Linear.Matrix
import Linear.V3
import SDL hiding (Texture, normalize)
import Unsafe.Coerce

import Graphics.RedViz.Entity as E
import Graphics.RedViz.Component as C
import Graphics.RedViz.Game as G
import Graphics.RedViz.Uniforms
import Graphics.RedViz.Widget as W
import Data.Aeson (Value(Object))

import Debug.Trace as DT

gameLoop :: MSF (MaybeT (ReaderT GameSettings (ReaderT Double (StateT Game IO)))) () Bool
gameLoop = runGame `untilMaybe` gameQuit `catchMaybe` exit
  where
    runGame  = arrM (\_ -> (lift . lift) gameLoopStep)
    gameQuit = arrM (\_ -> (lift . lift . lift) gameQuit')

    gameQuit' :: StateT Game IO Bool
    gameQuit' = TMSF.get >>= \s -> return $ G.quit s

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
                  cxform          = xform  . transformable $ cam
                  camera_position = (xform . transformable $ cam)^.translation
                  camera_lookat   = V3 0 0 (-1) *! cxform^._m33
                  ivec            = normalize $ centroid - camera_position :: V3 Double
                  s               = dot ivec camera_lookat > 1.0 - atan (radius / distance centroid camera_position) / pi

              gameEntities :: Game -> Game
              gameEntities g0 = -- DT.trace ("cams g0 : " ++ show (cams g0)) $
                g0 { objs = updateEntity <$> objs g0
                   , cams = updateEntity <$> cams g0 }
                where
                  updateEntity :: Entity -> Entity
                  updateEntity e0 = -- DT.trace ("e0 :" ++ show e0) $
                    e0{ cmps = updateComponent <$> cmps e0}
                    where
                      updateComponent cmp = case cmp of
                        Transformable{}   -> solveTransformable e0 cmp
                        s@Selectable{}    -> s {selected = lookedAt (head $ cams g0) ((xform . transformable $ e0)^.translation) 0.1}
                        _ -> cmp

                  (<++>) :: Component -> Component -> Component
                  (<++>) cmp0@(Movable _ _ v0 _)       (Fadable l a _ amp f) = cmp0 { tvel = amp * f (l-a) *^ v0 }
                  (<++>) cmp0@(Turnable _ _ _ _ av0 _) (Fadable l a _ amp f) = cmp0 { avel = amp * f (l-a) *^ av0 }
                  (<++>) cmp0@(Movable _ _ v0 _)       (Attractable _ a)     = cmp0 { tvel = v0 + a }  -- a*dt?
                  (<++>) cmp0 _ = cmp0

                  solveTransformable :: Entity -> Component -> Component
                  solveTransformable t0 tr0@(Transformable {}) = --DT.trace ("Entity : " ++ show (lable t0) ++ "xform tr0 :" ++ show (xform tr0)) $
                    tr0 { xform  = foldl (!*!) (xform tr0) $ xformSolver (parentXform $ xform tr0) <$> tslvrs tr0
                        , tslvrs = updateComponent <$> tslvrs tr0 }
                    where
                      parentXform :: M44 Double -> M44 Double
                      parentXform mtx0 = -- TODO: infer parenting xform correction from parent xform
                        if (not . null $ parentables t0) && (not . null $ parents') && (parented . parentable $ t0)
                        then (xform . transformable $ t0)&translation .~ (xform . transformable . head $ parents')^.translation
                        else mtx0
                        where parents' = parents t0 (objs g0) 

                      xformSolver :: M44 Double -> Component -> M44 Double
                      xformSolver mtx0 cmp =
                        case cmp of
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
                   
                          c0@(Controllable cvel0 ypr0 yprS0 _ _ _ _) -> --DT.trace ("Camerable : " ++ show (camerables t0) ) $ --DT.trace ("Controllable : " ++ show c0 ) $
                            (!*!) (inv44 $ xform tr0) $ mkTransformationMat rot tr
                            where
                              rot = 
                                (mtx0^._m33) !*!
                                    fromQuaternion (axisAngle (mtx0^.(_m33._x)) (ypr0^._x)) -- pitch
                                !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (ypr0^._y)) -- yaw
                                !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (ypr0^._z)) -- roll
                              tr  = -- DT.trace ("name : " ++ show (lable t0) ++ " camerables : " ++ show (camerables t0)) $
                                case camerables t0 of
                                  [] -> mtx0^.translation + (mtx0^._m33) !* cvel0
                                  _  -> mtx0^.translation + inv33 (mtx0^._m33) !* cvel0
                          --Attractable mass acc -> identity & translation .~ V3 0 0 0.1 -- acc -- identity -- & translation .~ pos
                          Parentable {} -> identity
                          _ -> identity

                      updateComponent :: Component -> Component
                      updateComponent cmp = -- DT.trace ("cmp :" ++ show cmp) $
                        case cmp of
                          Identity             -> cmp
                          Movable _ pos vel0 ss   ->
                            cmp { txyz   = pos  + vel0
                                , tvel   = tvel (foldl (<++>) cmp (ss ++ (tslvrs.transformable $ t0)))
                                , kinslv = updateComponent <$> ss}
                          Turnable _ _ _ rxyz avel0 ss ->
                            cmp { rxyz   = rxyz + avel0
                                , avel   = avel (foldl (<++>) cmp (ss ++ (tslvrs.transformable $ t0)))
                                , kinslv  = updateComponent <$> ss }
                          Fadable l a inc _ _ ->
                            cmp { age    = min (a + inc) l}
                          Attractable m0 _ -> cmp { acc = gravity }
                            where 
                              gravity :: V3 Double
                              gravity =
                                if not.null $ attractors then foldr (attract t0) (V3 0 0 0) attractors else V3 0 0 0
                                where
                                  attractors :: [Object]
                                  attractors = filter (\obj' -> uuid obj' /= uuid t0) $ objs g0

                                  attract :: Object -> Object -> V3 Double -> V3 Double
                                  attract obj0' obj1' acc0 = -- DT.trace ("t0 :" ++ show t0) $
                                    acc0 + acc'
                                    where
                                      attractables = ([ x | x@(Attractable {} ) <- tslvrs (transformable obj1') ])
                                      Attractable m1 _  = if not.null $ attractables then head attractables else Attractable 0 (V3 0 0 0)
                                      p0   = (xform.transformable $ obj0')^.translation
                                      p1   = (xform.transformable $ obj1')^.translation
                                      dir  = p1 ^-^ p0                 :: V3 Double
                                      dist = norm dir                  :: Double
                                      g    = 6.673**(-11.0)            :: Double
                                      f    = g * m0 * m1 / dist**2.0   :: Double
                                      acc' = case compare dist 0 of
                                        EQ -> 0
                                        _  -> (f / m0) *^ (dir ^/ dist)
                                    -- | F = G*@mass*m2/(dist^2);       // Newton's attract equation
                                    -- | a += (F/@mass)*normalize(dir); // Acceleration
                          Parentable {} -> -- DT.trace ("Parentable :" ++ show (lable t0) ++ " " ++ show cmp) $ 
                            cmp { parent = if not . null $ activeObjs then uuid . head $ activeObjs else nil }
                            where
                              activeObjs = filter (C.active . parentable) $ objs g0

                          _ -> cmp                  
                  solveTransformable _ tr0 = tr0
 
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
                          wgt { objects = filter (selected . selectable) $ objs g0 }
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
                        Just vpos -> callBack (unsafeCoerce vpos)
                          where
                            callBack :: Point V2 CInt -> StateT Game IO ()
                            callBack pos = do
                              modify $ updateControllables
                              where
                                updateControllables :: Game -> Game
                                updateControllables g0 = g0 { cams = updateEntity <$> cams g0
                                                            , objs = updateEntity <$> objs g0
                                                            }
                                  where
                                    updateEntity :: Entity -> Entity
                                    updateEntity e0 =
                                      e0 { cmps = updateTransformable pos <$> cmps e0 }
                                      where
                                        updateTransformable :: Point V2 CInt -> Component -> Component
                                        updateTransformable pos t0@(Transformable _ slvrs0) = t0 { tslvrs = updateControllable <$> tslvrs t0 }
                                          where
                                            updateControllable :: Component -> Component
                                            updateControllable cmp0 = case cmp0 of
                                              Controllable cvel0 cypr0 cyprS0 _ keyboardRS keyboardTS _ ->
                                                cmp0
                                                { cvel  = keyboardTS *^ cvel0
                                                , cypr  = keyboardRS *^ V3 (fromIntegral $ pos^._y) (fromIntegral $ pos^._x) 0
                                                , cyprS = keyboardRS *^ V3 (fromIntegral $ pos^._y) (fromIntegral $ pos^._x) 0 + cyprS0
                                                }
                                              _ -> cmp0
                                        updateTransformable _ cmp = cmp

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
                  --, ((ScancodeV     , Released, False)  , ctrlUnParent     )
                  ]
                  where
                    updateController :: Entity
                                     -> V3 Double
                                     -> V3 Double
                                     -> Component
                                     -> Component
                    updateController obj vel0 ypr0 cmp0 = 
                        case cmp0 of
                          ctrl@(Controllable _ _ cyprS _ keyboardRS keyboardTS _) -> -- Controllable cvel0 cypr0 cyprS0 _ keyboardRS keyboardTS ->
                            ctrl { cvel  = keyboardTS *^ vel0
                                 , cypr  = keyboardRS *^ ypr0
                                 --, cypr  = V3 0 0 0.000001
                                 , cyprS = keyboardRS *^ ypr0 + cyprS } 
                                 --, cyprS = V3 0 0 0 } 
                          _ -> cmp0

                    ctrlDolly :: Integer -> StateT Game IO ()
                    ctrlDolly n = modify camDolly'
                      where
                        camDolly' :: Game -> Game
                        camDolly' g0 = g0 { cams = updateEntity <$> cams g0
                                          , objs = updateEntity <$> objs g0 }
                          where
                            updateEntity :: Entity -> Entity
                            updateEntity t0 =
                              t0 { cmps = updateComponent <$> cmps t0}
                              where
                                updateComponent :: Component -> Component
                                updateComponent tr0@(Transformable{}) =
                                  tr0 { tslvrs = updateController (t0) (V3 0 0 (fromIntegral n)) (V3 0 0 0) <$> tslvrs tr0}
                                updateComponent cmp = cmp
                                    
                    ctrlTruck :: Integer -> StateT Game IO ()
                    ctrlTruck n = modify $ camTruck'
                      where
                        camTruck' :: Game -> Game
                        camTruck' g0 = g0 { cams = updateEntity <$> cams g0
                                          , objs = updateEntity <$> objs g0 }
                          where
                            updateEntity :: Entity -> Entity
                            updateEntity t0 =
                              t0 { cmps = updateComponent <$> cmps t0 }
                              where
                                updateComponent :: Component -> Component
                                updateComponent tr0@(Transformable {}) =
                                  tr0 { tslvrs = updateController t0 (V3 (fromIntegral n) 0 0) (V3 0 0 0) <$> tslvrs tr0}
                                updateComponent cmp = cmp
                                    
                    ctrlPedestal :: Integer -> StateT Game IO ()
                    ctrlPedestal n = modify $ camPedestal'
                      where
                        camPedestal' :: Game -> Game
                        camPedestal' g0 = g0 { cams = updateEntity <$> cams g0
                                             , objs = updateEntity <$> objs g0 }
                          where
                            updateEntity :: Entity -> Entity
                            updateEntity t0 =
                              t0 { cmps = updateComponent <$> cmps t0}
                              where
                                updateComponent :: Component -> Component
                                updateComponent tr0@(Transformable {}) =
                                  tr0 { tslvrs = updateController t0 (V3 0 (fromIntegral n) 0) (V3 0 0 0) <$> tslvrs tr0}
                                updateComponent cmp = cmp

                    ctrlRoll :: Integer -> StateT Game IO ()
                    ctrlRoll n = modify $ camRoll'
                      where
                        camRoll' :: Game -> Game
                        camRoll' g0 = g0 { cams = updateEntity <$> cams g0
                                         , objs = updateEntity <$> objs g0 }
                          where
                            updateEntity :: Entity -> Entity
                            updateEntity t0 =
                              t0 { cmps = updateComponent <$> cmps t0 }
                              where
                                updateComponent :: Component -> Component
                                updateComponent tr0@(Transformable {}) =
                                  tr0 { tslvrs = updateController t0 (V3 0 0 0) (V3 0 0 (fromIntegral n)) <$> tslvrs tr0}
                                updateComponent cmp = cmp

                    ctrlParent False = modify $ camParent'
                      where
                        camParent' :: Game -> Game
                        camParent' g0 = --DT.trace ("camParent' objs: " ++ show (objs g0)) $ DT.trace ("camParent' cams: " ++ show (cams g0)) $
                          g0 { cams = updateEntity <$> cams g0
                             , objs = updateEntity <$> objs g0
                             }
                          where
                            updateEntity :: Entity -> Entity
                            updateEntity t0 =
                              t0 { cmps = updateComponent <$> cmps t0 }
                              where
                                parentable2controllable :: Component
                                parentable2controllable = controllable . fromComponent $ parentable t0

                                controllable2parentable :: Component
                                controllable2parentable = parentable . fromComponent $ controllable t0

                                fromComponent :: Component -> Entity
                                fromComponent p0 = head $ filter (\obj' -> uuid obj' /= parent p0) $ objs g0

                                updateComponent :: Component -> Component
                                updateComponent p@(Parentable {})    = parentable2controllable
                                updateComponent p@(Controllable {})  = controllable2parentable
                                updateComponent t@(Transformable {}) = --DT.trace ("entity: " ++ show (lable t0) ++ " parentable: " ++ show (parentable t0)) $
                                  t { xform = if (not . null $ parents') && (not . parented . parentable $ t0) -- if camera is orphan, but has potential parents
                                              then rotY (xform . transformable $ head parents')
                                              else xform t
                                    , tslvrs = updateComponent <$> (tslvrs . transformable $ t0) --}
                                    }
                                  where parents' = parents t0 (objs g0) :: [Entity]
                                  
                                updateComponent cmp = cmp
  
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
                                          !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (pi/2)) -- yaw
                                          !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (0))  -- roll
                                        tr  = mtx0'^.translation
                                        mtx0' = identity :: M44 Double
    
                                Parentable uid p a = if not . null $ parentables t0 then head (parentables t0) else Parentable nil p a

                    -- ctrlUnParent :: StateT Game IO ()
                    -- ctrlUnParent = TMSF.gets $ const ()
                                               
                    quitE :: Bool -> StateT Game IO ()
                    quitE b = modify $ quit' b
                      where
                        quit' :: Bool -> Game -> Game
                        quit' b' gameLoopDelay' = gameLoopDelay' { quit = b' }

          updateTick :: Double -> StateT Game IO ()
          updateTick n = modify $ inc'
            where
              inc' :: Game -> Game
              inc' g0 = g0
                { tick = tick g0 + n 
                , unis = incUnis (unis g0) }
                where
                  incUnis :: Uniforms -> Uniforms
                  incUnis unis0 = 
                    unis0 { u_time = tick g0 }
