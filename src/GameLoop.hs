module GameLoop where

import Data.List (find)
import Data.MonadicStreamFunction
import Data.Maybe (fromMaybe, listToMaybe)
import Data.UUID (nil, fromText)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.MSF as TMSF
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
      --lift updateState
      TMSF.ask >>= \r -> lift $ updateState r

    updateState :: Double -> StateT Game IO Bool
    updateState r = do
      updateEntities
      updateWidgets
      updateGame r
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
              gameEntities g0 = 
                g0 { objs = updateEntity <$> objs g0
                   , cams = updateEntity <$> cams g0 }
                where
                  updateEntity :: Entity -> Entity
                  updateEntity t0 =
                    t0{ cmps = updateComponent <$> cmps t0}
                    where
                      updateComponent cmp =
                          case cmp of
                            Transformable{}   -> solveTransformable t0 cmp
                            s@Selectable{}    -> s {selected = lookedAt cam0 ((xform . transformable $ t0)^.translation) 0.1}
                              where cam0 = fromMaybe (error "cams is empty") (listToMaybe . cams $ g0)
                            _ -> cmp

                  solveTransformable :: Entity -> Component -> Component
                  solveTransformable t0 tr0@(Transformable {}) =
                    tr0 { xform  = case isControllerParented t0 of 
                            False ->
                              foldl (flip (!*!)) (xform tr0) $ xformSolver (xform tr0) <$> tslvrs tr0
                            True  -> inv44 mtx' & translation .~ (mtx'^.translation)
                              where
                                mtx' =
                                  xformC . xform . transformable $ fromMaybe defaultEntity parentControllable
                                   where parentControllable = find (\t0' -> uuid t0' == (parent . controllable $ t0) ) . objs $ g0
                        , tslvrs = updateComponent <$> tslvrs tr0 }
                    where
                      xformC :: M44 Double -> M44 Double
                      xformC mtx0 = mtx0 !*! omtx0
                        where
                          omtx0  = (identity :: M44 Double) & translation .~ V3 0 0.5 (10)                              

                      isControllable t0       = case controllables t0 of [] -> False; _ -> True
                      isControllerParented t0 = isControllable t0 && (parent . controllable $ t0) /= nil

                      xformSolver :: M44 Double -> Component -> M44 Double
                      xformSolver mtx0 cmp =
                        case cmp of
                          Identity -> identity
                          Constant -> mtx0
                          Movable cs vel0  _ ->
                            case cs of
                              WorldSpace  -> identity & translation .~ vel0 
                              ObjectSpace -> undefined
                          Turnable _ rord cxyz rxyz avel _ ->  -- TODO: add Object/World rotation distinction
                            (!*!) (inv44 $ xform tr0) $ mkTransformationMat rot tr
                            where
                              rot = 
                                (mtx0^._m33) !*!
                                case rord of
                                  XYZ ->
                                        fromQuaternion (axisAngle (mtx0^.(_m33._x)) (avel^._x)) -- pitch
                                    !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (avel^._y)) -- yaw
                                    !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (avel^._z)) -- roll
                              tr = cxyz
                   
                          c0@(Controllable cvel0 ypr0 yprS0 _ _ _ parent0 _) -> 
                            flip (!*!) (inv44 $ xform tr0) $ mkTransformationMat rot tr
                                where
                                  rot = 
                                        (mtx0^._m33) !*!
                                            fromQuaternion (axisAngle (mtx0^.(_m33._x)) (ypr0^._x)) -- pitch
                                        !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (ypr0^._y)) -- yaw
                                        !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (ypr0^._z)) -- roll
                                  tr  = 
                                    mtx0^.translation + cvel0
                            
                          Parentable uuid0 ->
                            case uuid0 == nil of
                              True  -> identity :: M44 Double
                              False -> mtx'
                                where
                                  mtx' = xformSolver' (identity :: M44 Double) (controllable $ fromMaybe defaultEntity ctl)
                                    where
                                      ctl = find (\t0' -> uuid t0' == uuid0 ) . controllabless $ g0
                                      xformSolver' :: M44 Double -> Component -> M44 Double
                                      xformSolver' mtx0' cmp = 
                                        case cmp of
                                          c0@(Controllable cvel0 ypr0 yprS0 _ _ _ parent0 _) ->
                                            flip (!*!) (inv44 $ xform tr0) $ mkTransformationMat rot tr
                                            where
                                              rot = 
                                                (mtx0^._m33) !*!
                                                    fromQuaternion (axisAngle (mtx0'^.(_m33._x)) (-ypr0^._x)) -- pitch
                                                !*! fromQuaternion (axisAngle (mtx0'^.(_m33._y)) (-ypr0^._y)) -- yaw
                                                !*! fromQuaternion (axisAngle (mtx0'^.(_m33._z)) (-ypr0^._z)) -- roll
                                              tr  =
                                                mtx0^.translation + cvel0
                                             
                                          _ -> (identity :: M44 Double)                                                
                          _ -> identity

                      (<++>) :: Component -> Component -> Component
                      (<++>) cmp0@(Movable _ v0 _)         (Fadable l a _ amp f) = cmp0 { tvel = amp * f (l-a) *^ v0 }
                      (<++>) cmp0@(Turnable _ _ _ _ av0 _) (Fadable l a _ amp f) = cmp0 { avel = amp * f (l-a) *^ av0 }
                      (<++>) cmp0@(Movable _ v0 _)         (Attractable _ a)     = cmp0 { tvel = v0 + a }  -- a*dt?
                      (<++>) cmp0 _ = cmp0

                      updateComponent :: Component -> Component
                      updateComponent cmp =
                        case cmp of
                          Identity             -> cmp
                          Movable _ vel0  ss   ->
                            cmp { tvel   = tvel (foldl (<++>) cmp (ss ++ (tslvrs.transformable $ t0)))
                                , kslvrs = updateComponent <$> ss}
                          Turnable _ _ _ rxyz avel0 ss ->
                            cmp { rxyz   = rxyz + avel0
                                , avel   = avel (foldl (<++>) cmp (ss ++ (tslvrs.transformable $ t0)))
                                , kslvrs  = updateComponent <$> ss }
                          Fadable l a inc _ _ ->
                            cmp { age    = min (a + inc) l}
                          Attractable m0 _ -> cmp { acc = gravity * 0.001 }
                            where 
                              gravity :: V3 Double
                              gravity =
                                case attractors of
                                  [] -> V3 0 0 0
                                  _  -> foldr (attract t0) (V3 0 0 0) attractors 
                                where
                                  attractors :: [Object]
                                  attractors =
                                    filter (\obj' -> uuid obj' /= uuid t0) $ objs g0

                                  attract :: Object -> Object -> V3 Double -> V3 Double
                                  attract obj0' obj1' acc0 =
                                    acc0 + acc'
                                    where
                                      attractables =
                                        case listToMaybe . movables $ obj1' of
                                          Nothing -> []
                                          Just movable' -> ([ x | x@(Attractable {} ) <- kslvrs movable' ])
                                            where movable' = fromMaybe (error "movables is empty")(listToMaybe . movables $ obj1')
                                      Attractable m1 _  = fromMaybe (Attractable 0 (V3 0 0 0)) (listToMaybe attractables)
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
                                    updateEntity t0 =
                                      t0 { cmps = updateTransformable pos <$> cmps t0 }
                                      where
                                        updateTransformable :: Point V2 CInt -> Component -> Component
                                        updateTransformable pos t0@(Transformable _ slvrs0) = t0 { tslvrs = updateControllable <$> tslvrs t0 }
                                          where
                                            updateControllable :: Component -> Component
                                            updateControllable cmp0 = case cmp0 of
                                              Controllable cvel0 cypr0 cyprS0 _ keyboardRS keyboardTS _ Static ->
                                                cmp0
                                                { cvel  = keyboardTS *^ cvel0
                                                , cypr  = keyboardRS *^ V3 (fromIntegral $ pos^._y) (fromIntegral $ pos^._x) 0
                                                , cyprS = cyprS0 + cypr cmp0
                                                }
                                              Controllable cvel0 cypr0 cyprS0 _ keyboardRS keyboardTS _ Dynamic ->
                                                cmp0
                                                { cypr  = cypr cmp0 + keyboardRS * ang0 *^ V3 (fromIntegral $ pos^._y) (fromIntegral $ pos^._x) 0
                                                , cyprS = cypr cmp0 + cyprS0
                                                }
                                                where
                                                  fr0  = 1
                                                  m0   = 1
                                                  ang0 = fr0 / m0
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
                    updateController t0 vel0 ypr0 cmp0 = 
                        case cmp0 of
                          Controllable _ _ cyprS0 _ keyboardRS keyboardTS _ Static ->
                            cmp0 { cvel  = keyboardTS *^ vel0 
                                 , cypr  = keyboardRS *^ ypr0
                                 , cyprS = keyboardRS *^ ypr0 + cyprS0 } 
                          Controllable _ _ cyprS0 _ keyboardRS keyboardTS _ Dynamic ->
                            cmp0 { cvel  = cvel cmp0 + keyboardTS * acc0 *^ (inv33 (mtx0^._m33) !* vel0 )
                                 , cypr  = cypr cmp0 + keyboardRS * ang0 *^ ypr0
                                 , cyprS = cypr cmp0 + cyprS0 } 
                                 where
                                   mtx0 = xform . transformable $ t0
                                   ft0 = 1
                                   fr0 = 1
                                   m0  = 1
                                   acc0 = ft0 / m0
                                   ang0 = fr0 / m0
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
                    
                    ctrlParent _ = do 
                      liftIO $ putStrLn "ctrlParent"
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
                                updateComponent c@(Controllable _ _ _ _ _ _ parent0 _) = 
                                  if parent0 == nil
                                  then c { parent = uuid (fromMaybe (error "parentabless is empty") (listToMaybe . parentabless $ g0)) }
                                  else c { parent = nil }

                                updateComponent p@(Parentable parent0)     =
                                  if parent0 == nil
                                  then p { parent = uuid (fromMaybe (error "controllabless is empty") (listToMaybe . controllabless $ g0)) }
                                  else p { parent = nil }

                                updateComponent tr@(Transformable {}) 
                                  | isControllable t0 && (not . isControllableParented $ t0) = 
                                      tr { xform  = xform . transformable $ fromMaybe (error "controlled parentabless is empty") (listToMaybe . parentabless $ g0)
                                         , tslvrs = updateComponent <$> (tslvrs . transformable $ t0)
                                         }
                                  | isControllable t0 && isControllableParented  t0 = 
                                      tr { xform  = xform . transformable $ fromMaybe (error "parented controllabless is empty") (listToMaybe . controllabless $ g0)
                                         , tslvrs = updateComponent <$> (tslvrs . transformable $ fromMaybe (error "parented controllabless is empty") (listToMaybe . controllabless $ g0))
                                         }
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

                    quitE :: Bool -> StateT Game IO ()
                    quitE b = modify $ quit' b
                      where
                        quit' :: Bool -> Game -> Game
                        quit' b' gameLoopDelay' = gameLoopDelay' { quit = b' }

          updateGame :: Double -> StateT Game IO ()
          updateGame n = modify $ inc'
            where
              inc' :: Game -> Game
              inc' g0 = g0
                { tick = tick g0 + n 
                , unis = incUnis (unis g0) }
                where
                  incUnis :: Uniforms -> Uniforms
                  incUnis unis0 = 
                    unis0 { u_time    = tick g0
                          , u_cam_vel = (\t0 -> (\(V3 x y z) -> (x,y,z)) $ cvel . controllable $ t0) $ fromMaybe defaultEntity (listToMaybe $ cams g0) } -- get cvel from first camera
