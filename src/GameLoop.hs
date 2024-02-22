module GameLoop where

import Data.MonadicStreamFunction
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.MSF as TMSF
import Data.UUID (nil, fromText)
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

import Control.Concurrent
import Control.Concurrent.MVar

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
                      updateComponent cmp =
                          case cmp of
                            Transformable{}   -> solveTransformable e0 cmp
                            s@Selectable{}    -> s {selected = lookedAt (head $ cams g0) ((xform . transformable $ e0)^.translation) 0.1}
                            _ -> cmp

                  solveTransformable :: Entity -> Component -> Component
                  solveTransformable t0 tr0@(Transformable {}) = --DT.trace ("Entity : " ++ show (lable t0) ++ "xform tr0 :" ++ show (xform tr0)) $
                    tr0 { xform  = case isParented t0 of 
                            False -> foldl (!*!) (xform tr0) $ xformSolver (xform tr0) <$> tslvrs tr0
                            True  -> foldl (!*!) (identity)  $ xformSolver (xform tr0) <$> tslvrs tr0
                        , tslvrs = updateComponent <$> tslvrs tr0 }
                    where
                      isParented   t0 = (parent . parentable $ t0) /= nil

                      xformSolver :: M44 Double -> Component -> M44 Double
                      xformSolver mtx0 cmp =
                        case cmp of
                          Identity -> identity
                          Constant -> mtx0
                          Movable cs vel0 _ ->
                            case cs of
                              WorldSpace  -> identity & translation .~ vel0
                              ObjectSpace -> undefined
                          Turnable _ rord cxyz rxyz avel _ ->  -- TODO: add Object/World rotation distinction
                            (!*!) (inv44 $ xform tr0) $ mkTransformationMat rot tr
                            where
                              rot = 
                                --identity !*! -- TODO: WorldSpace - ObjectSpace
                                (mtx0^._m33) !*!
                                case rord of
                                  XYZ ->
                                        fromQuaternion (axisAngle (mtx0^.(_m33._x)) (avel^._x)) -- pitch
                                    !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (avel^._y)) -- yaw
                                    !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (avel^._z)) -- roll
                              tr = cxyz
                   
                          c0@(Controllable cvel0 ypr0 yprS0 _ _ _ parent0 ) -> --DT.trace ("Camerable : " ++ show (camerables t0) ) $ --DT.trace ("Controllable : " ++ show c0 ) $
                            case parent0 == nil of
                              _ ->
                                (!*!) (inv44 $ xform tr0) $ mkTransformationMat rot tr
                                where
                                  rot = 
                                    case camerables t0 of
                                      [] -> (mtx0^._m33) !*!
                                            fromQuaternion (axisAngle (mtx0^.(_m33._x)) (ypr0^._x)) -- pitch
                                        !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (-ypr0^._y)) -- yaw
                                        !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (ypr0^._z)) -- roll
                                      _ ->  (mtx0^._m33) !*!
                                            fromQuaternion (axisAngle (mtx0^.(_m33._x)) (ypr0^._x)) -- pitch
                                        !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (ypr0^._y)) -- yaw
                                        !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (ypr0^._z)) -- roll

                                  tr  = -- DT.trace ("name : " ++ show (lable t0) ++ " camerables : " ++ show (camerables t0)) $
                                    case camerables t0 of
                                      [] -> mtx0^.translation + (mtx0^._m33) !* cvel0
                                      _  -> mtx0^.translation + inv33 (mtx0^._m33) !* cvel0

                          -- Attractable _ acc -> identity - it's solved
                            --(identity :: M44 Double) & translation .~ acc -- identity -- & translation .~ pos -- TODO:
                            --(inv44 mtx0 & translation .~ V3 0 0 0 :: M44 Double) & translation .~ acc -- sort of works
                            --((inv44 mtx0 & translation .~ V3 0 0 0 :: M44 Double) & translation .~ acc)
                            where mtx' = mtx0 & translation .~ V3 0 0 0 :: M44 Double
                            
                          Parentable uuid0 ->
                            case uuid0 == nil of
                              True -> identity :: M44 Double
                              --_ ->  xformSolver mtx0 (controllable . head . filter (\t0' -> uuid t0' == uuid0 ) . controllabless $ g0)
                              _ -> rotY (xform . transformable . head . filter (\t0' -> uuid t0' == uuid0 ) . controllabless $ g0)
                            where          
                                rotY :: M44 Double -> M44 Double
                                rotY mtx0 = tmtx0 !*! rmtx0
                                -- rotY mtx0 = mtx !*! mtx0
                                  where
                                    rmtx0 =
                                      mkTransformationMat
                                      rot
                                      tr
                                      where
                                        --rot = mtx0^._m33 !*!
                                        rot = (identity :: M44 Double)^._m33 !*!
                                              fromQuaternion (axisAngle (mtx0^.(_m33._x)) (0)) -- pitch
                                          !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (pi/2)) -- yaw
                                          !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (0)) -- roll
                                        tr = V3 0 0 0
                                        --tr = mtx0^.translation
                                    tmtx0 =
                                      mkTransformationMat
                                      rot  
                                      tr
                                      where
                                        rot = identity :: M33 Double
                                        tr  = mtx0^.translation

                          _ -> identity

                      (<++>) :: Component -> Component -> Component
                      (<++>) cmp0@(Movable _ v0 _)       (Fadable l a _ amp f) = cmp0 { tvel = amp * f (l-a) *^ v0 }
                      (<++>) cmp0@(Turnable _ _ _ _ av0 _) (Fadable l a _ amp f) = cmp0 { avel = amp * f (l-a) *^ av0 }
                      (<++>) cmp0@(Movable _ v0 _)       (Attractable _ a)     = cmp0 { tvel = v0 + a }  -- a*dt?
                      (<++>) cmp0 _ = cmp0

                      updateComponent :: Component -> Component
                      updateComponent cmp = -- DT.trace ("cmp :" ++ show cmp) $
                        case cmp of
                          Identity             -> cmp
                          Movable _ vel0 ss   ->
                            cmp { tvel   = tvel (foldl (<++>) cmp (ss ++ (tslvrs.transformable $ t0)))
                                , kinslv = updateComponent <$> ss}
                          Turnable _ _ _ rxyz avel0 ss ->
                            cmp { rxyz   = rxyz + avel0
                                , avel   = avel (foldl (<++>) cmp (ss ++ (tslvrs.transformable $ t0)))
                                , kinslv  = updateComponent <$> ss }
                          Fadable l a inc _ _ ->
                            cmp { age    = min (a + inc) l}
                          --Attractable m0 _ -> cmp { acc = DT.trace ("DEBUG : " ++ show gravity) $ gravity }
                          Attractable m0 _ -> cmp { acc = gravity * 0.001 }
                            where 
                              gravity :: V3 Double
                              gravity = 
                                if not.null $ attractors then foldr (attract t0) (V3 0 0 0) attractors else V3 0 0 0
                                where
                                  attractors :: [Object]
                                  attractors = -- DT.trace ("DEBUG : " ++ show (fmap lable $ filter (\obj' -> uuid obj' /= uuid t0) $ objs g0))
                                    filter (\obj' -> uuid obj' /= uuid t0) $ objs g0

                                  attract :: Object -> Object -> V3 Double -> V3 Double
                                  attract obj0' obj1' acc0 = -- DT.trace ("t0 :" ++ show t0) $
                                    acc0 + acc'
                                    where
                                      attractables = -- DT.trace ("DEBUG : " ++ show (([ x | x@(Attractable {} ) <- tslvrs (transformable obj1') ])))
                                        ([ x | x@(Attractable {} ) <- tslvrs (transformable obj1') ])
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
                          -- Parentable {} -> -- DT.trace ("Parentable :" ++ show (lable t0) ++ " " ++ show cmp) $ 
                          --   cmp { parent = if not . null $ activeObjs then uuid . head $ activeObjs else nil }
                          --   where
                          --     activeObjs = filter (C.active . parentable) $ objs g0

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
                  --, ((ScancodeV     , Released, False)  , ctrlParent' False )
                  ]
                  where
                    updateController :: Entity
                                     -> V3 Double
                                     -> V3 Double
                                     -> Component
                                     -> Component
                    updateController obj vel0 ypr0 cmp0 = 
                        case cmp0 of
                          ctrl@(Controllable _ _ cyprS _ keyboardRS keyboardTS _) ->
                            ctrl { cvel  = keyboardTS *^ vel0
                                 , cypr  = keyboardRS *^ ypr0
                                 , cyprS = keyboardRS *^ ypr0 + cyprS } 
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
                                  tr0 { tslvrs = updateController t0 (V3 0 (fromIntegral n) 0) (V3 0 0 0) <$> tslvrs tr0 }
                                updateComponent cmp = cmp

                    ctrlRoll :: Integer -> StateT Game IO ()
                    ctrlRoll n = modify camRoll'
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
                                  tr0 { tslvrs = updateController t0 (V3 0 0 0) (V3 0 0 (fromIntegral (-n*50))) <$> tslvrs tr0}
                                updateComponent cmp = cmp

                    -- ctrlParent' False = modify debugEntity
                    --   where
                    --     debugEntity :: Game -> Game
                    --     debugEntity g0 = DT.trace (  "################### RELEASED ######################" ++ "\n"
                    --                                 ++ "cams g0 : " ++ show (cams g0) ++ "\n"
                    --                                 ++ "objs g0 : " ++ show (objs g0) ++ "\n"
                    --                                 ++ "#################################################" ++ "\n"
                    --                               ) g0
                    
                    ctrlParent False = do
                      modify parentEntity
                      where
                        parentEntity :: Game -> Game
                        parentEntity g0 = 
                          g0 { cams = updateEntity  <$> cams g0
                             , objs = updateEntity  <$> objs g0
                             }
                          where
                            updateEntity :: Entity -> Entity
                            updateEntity t0 = 
                              t0 { cmps = updateComponent <$> cmps t0 }
                              where
                                updateComponent :: Component -> Component
                                updateComponent c@(Controllable _ _ _ _ _ _ parent0) = 
                                  if parent0 == nil
                                  then c { parent = uuid . head . parentabless $ g0 }
                                  else c { parent = nil }

                                updateComponent p@(Parentable parent0)     =
                                  if parent0 == nil
                                  then p { parent = uuid . head . controllabless $ g0 }
                                  else p { parent = nil }

                                updateComponent tr@(Transformable {}) 
                                  | isControllable t0 && (not . isControllableParented $ t0) = 
                                  -- | isControllable t0 = 
                                      tr { xform  = rotY . xform . transformable . head . parentabless $ g0
                                         , tslvrs = updateComponent <$> (tslvrs . transformable $ t0)
                                         }
                                  | isControllable t0 && isControllableParented  t0 = 
                                      tr { xform  = xform . transformable . head . controllabless $ g0
                                         , tslvrs = updateComponent <$> (tslvrs . transformable $ t0)
                                         }

                                  -- | isControllable t0 && (isControllableParented t0) = 
                                  --     tr { xform  = rotY (xform . transformable . head $ parentabless g0)
                                  --        , tslvrs = updateComponent <$> (tslvrs . transformable $ t0)
                                  --        }
                                  | isParentable t0 = 
                                      tr { xform  = xform tr
                                         , tslvrs = updateComponent <$> (tslvrs . transformable $ t0)
                                         }
                                  | otherwise = tr
                                  where
                                    isControllable t0 = case controllables t0 of [] -> False; _ -> True
                                    isControllableParented t0 = (parent . controllable $ t0) /= nil
                                    isParentable   t0 = case parentables   t0 of [] -> False; _ -> True
                                    isCamerable    t0 = case camerables    t0 of [] -> False; _ -> True
                                  
                                updateComponent cmp = cmp
  
                                rotY :: M44 Double -> M44 Double
                                rotY mtx0 = tmtx0 !*! rmtx0
                                -- rotY mtx0 = mtx !*! mtx0
                                  where
                                    rmtx0 =
                                      mkTransformationMat
                                      rot
                                      tr
                                      where
                                        --rot = mtx0^._m33 !*!
                                        rot = (identity :: M44 Double)^._m33 !*!
                                              fromQuaternion (axisAngle (mtx0^.(_m33._x)) (0)) -- pitch
                                          !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (-pi/2)) -- yaw
                                          !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (0)) -- roll
                                        tr = V3 0 0 0
                                        --tr = mtx0^.translation
                                    tmtx0 =
                                      mkTransformationMat
                                      rot  
                                      tr
                                      where
                                        rot = identity :: M33 Double
                                        tr  = mtx0^.translation
                                --     mtx =
                                --       mkTransformationMat
                                --       rot
                                --       tr
                                --       where -- TODO: rotation must be local to object (Object Space)
                                --         --rot = (mtx0^._m33 :: M33 Double) !*! 
                                --         rot = identity :: M33 Double -- !*! 
                                --           --     fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (0))  -- pitch
                                --           -- !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (0)) -- yaw
                                --           -- !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (0))  -- roll
                                --         --tr  = mtx0'^.translation
                                --         tr  = V3 0 0 0 
                                --         mtx0' = mtx0 :: M44 Double

                                --         -- rot = (identity :: M33 Double) !*! 
                                --         --       fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (0))  -- pitch
                                --         --   !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (pi)) -- yaw
                                --         --   !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (0))  -- roll
                                --         -- tr  = mtx0'^.translation
                                --         -- mtx0' = identity :: M44 Double
    
                                -- Parentable uid = if not . null $ parentables t0 then head (parentables t0) else Parentable nil

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
