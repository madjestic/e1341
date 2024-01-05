{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import SDL hiding (Texture, normalize)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.MSF as TMSF
import Data.MonadicStreamFunction  
import Data.Text (Text, unpack)
import Foreign (sizeOf)  
import Foreign.C.Types  
import Unsafe.Coerce
import Graphics.Rendering.OpenGL as GL hiding (Select, normalize)
import Foreign.Ptr (plusPtr, nullPtr, Ptr)
import Foreign.Marshal.Array (withArray)  
import Lens.Micro
import Lens.Micro.Extras
import Data.Vector qualified as V hiding (head, length)
import Data.Foldable as DF
import Data.Word
import GHC.Float
import Data.StateVar as SV
import Geomancy.Vec4 hiding (dot, normalize) 
import Geomancy.Vec3 hiding (dot, normalize)
import Geomancy.Vec2 hiding (dot, normalize)
import Data.Coerce (coerce)
import Data.UUID
import Linear.Projection         as LP        (infinitePerspective)
import Linear.Metric (normalize)
import Data.Maybe (fromMaybe)
import Data.Set as DS ( fromList, toList )
import GHC.Generics
import Data.UUID.V4

import Graphics.RedViz.Backend
import Graphics.RedViz.Camera
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Game
import Graphics.RedViz.GLTF as Gltf
import Graphics.RedViz.LoadShaders
import Graphics.RedViz.Material as R
import Graphics.RedViz.Object
import Graphics.RedViz.Project
import Graphics.RedViz.Rendering 
import Graphics.RedViz.Solvable
import Graphics.RedViz.Texture as T
import Graphics.RedViz.Transformable
import Graphics.RedViz.Widget
import Graphics.RedViz.Uniforms

import Projects.Test

--import Debug.Trace as DT

type DTime = Double
type Time  = Double
type Res   = (CInt, CInt)

-- lookupObject :: Object -> UUID -> Object
-- lookupObject obj0 uuid0 = obj0  

runGame :: MSF (MaybeT (ReaderT GameSettings (ReaderT Double (StateT Game IO)))) () Bool
runGame = gameLoop `untilMaybe` gameQuit `catchMaybe` exit
  where
    gameLoop = arrM (\_ -> (lift . lift) gameLoopDelay)
    gameQuit = arrM (\_ -> (lift . lift . lift) gameQuit')

    gameQuit' :: StateT Game IO Bool
    gameQuit' = TMSF.get >>= \s -> return $ quitGame s

    gameLoopDelay :: ReaderT Double (StateT Game IO) Bool
    gameLoopDelay = do
      TMSF.ask >>= \r ->  liftIO $ delay $ fromIntegral(double2Int $ r * 100)
      lift updateGame

    updateGame :: StateT Game IO Bool
    updateGame = do
      updateObjects
      updateWidgets
      updateCameras
      handleEvents
        where
          solveTransformable :: Transformable -> Transformable
          solveTransformable t0 =
            t0 { xform  = foldr1 (!*!) $ solveXform (xform t0) <$> tslvrs t0
               , tslvrs = updateSolver <$> tslvrs t0 }
            where
              updateSolver :: Solvable -> Solvable
              updateSolver slv =
                case slv of
                  Identity                 -> slv
                  Translate  _ pos  vel  s -> slv { txyz   = pos  + vel  `applySolver` s
                                                  , kinslv = updateSolver s}
                  Rotate _ _ _ rxyz avel s -> slv { rxyz   = rxyz + avel `applySolver` s
                                                  , kinslv = updateSolver s }
                  Speed l a inc _ _        -> slv { age    = min (a + inc) l}
                  _ -> slv

              applySolver :: V3 Double -> Solvable -> V3 Double
              applySolver v slv = 
                case slv of
                  Speed l a _ amp f -> amp * f (l-a) *^ v
                  _ -> v

              solveXform :: M44 Double -> Solvable -> M44 Double
              solveXform mtx0 slv =
                case slv of
                  Identity -> identity
                  Translate cs pos _ _ ->
                    case cs of
                      WorldSpace  -> identity & translation .~ pos
                      ObjectSpace -> undefined
                  Rotate _ _ rord rxyz _ _ -> transform' identity
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

                  Controller cvel0 ypr0 _ ->
                    mkTransformationMat rot tr
                    where
                      rot = 
                        (mtx0^._m33) !*!
                            fromQuaternion (axisAngle (mtx0^.(_m33._x)) (ypr0^._x)) -- pitch
                        !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (ypr0^._y)) -- yaw
                        !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (ypr0^._z)) -- roll
                      tr  = mtx0^.translation + inv33 (mtx0^._m33) !* cvel0
                  Parent -> identity -- TODO: add inheriting transform from sertParent object
                  _ -> identity

          updateCameras :: StateT Game IO ()
          updateCameras = modify gameCameras
            where
              gameCameras :: Game -> Game
              gameCameras g0 = g0 { cameras = updateCamera <$> cameras g0 }
                where
                  updateCamera :: Camera -> Camera
                  updateCamera cam0 =
                    cam0 { ctransform = solveTransformable (ctransform cam0) }
                    
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
              gameObjects :: Game -> Game
              gameObjects g0 = g0 { objs = updateObject <$> objs g0 }
                where
                  updateObject :: Object -> Object
                  updateObject obj0 =
                    obj0 { transform = solveTransformable (transform obj0)
                         , selected  = lookedAt (head $ cameras g0) (xform (transform obj0)^.translation) 0.1 }
                    where
                      lookedAt :: Camera -> V3 Double -> Double -> Bool
                      lookedAt cam centroid radius = s
                        where
                          cxform          = xform . ctransform $ cam
                          camera_position = (xform . ctransform $ cam)^.translation
                          camera_lookat   = V3 0 0 (-1) *! cxform^._m33
                          ivec            = normalize $ centroid - camera_position :: V3 Double
                          s               = dot ivec camera_lookat > 1.0 - atan (radius / distance centroid camera_position) / pi

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
                        ctrl@(Controller _ _ cyprS) ->
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
                    camRoll n = undefined --modify $ camRoll'
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
                                  t0 { tslvrs = updateController cam (V3 0 0 0) (V3 0 (fromIntegral n) 0) <$> tslvrs t0}
                          
                    inc :: Integer -> StateT Game IO ()
                    inc n = modify $ inc' n
                      where
                        inc' :: Integer -> Game -> Game
                        inc' k g0 = g0 { uniforms = incUnis (tick g0 + k) (uniforms g0) }
                          where
                            incUnis :: Integer -> Uniforms -> Uniforms
                            incUnis tick' unis0 = 
                              unis0 { u_time = fromInteger tick' }
                     
                    quitE :: Bool -> StateT Game IO ()
                    quitE b' = modify $ quit' b'
                      where
                        quit' :: Bool -> Game -> Game
                        quit' b'' gameLoopDelay' = gameLoopDelay' { quitGame = b'' }         
              
animate :: Window
        -> DTime
        -> GameSettings
        -> Game
        -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
        -> IO ()
animate window dt gs g sf = do
  reactimateB $ input >>> sfIO >>> output window
  quit
  where
    input    = arr (const (dt, (gs, ())))                            :: MSF IO b (DTime, (GameSettings, ()))
    sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) g :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output w = arrM (renderOutput w gs)                              :: MSF IO   (Game, Maybe Bool) Bool

main :: IO ()
main = do
  let
    (resX', resY') =
      (\opts ->
          ( unsafeCoerce $ fromIntegral $ resX opts
          , unsafeCoerce $ fromIntegral $ resY opts))
      initSettings
  parentTestProject' <- setProjectUUID $ parentTestProject resX' resY'

  let
    initProject'= parentTestProject'
    models'     = models     initProject' :: [FilePath]
    fonts'      = fontModels initProject' :: [FilePath]
    icons'      = iconModels initProject' :: [FilePath]    

  initializeAll
  window <- openWindow "Mandelbrot + SDL2/OpenGL" (resX', resY')

  _ <- setMouseLocationMode RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX'`div`2) (resY'`div`2)))
  _ <- cursorVisible $= True
  
  putStrLn "Compiling Materials"
  dms  <- mapM toDescriptorMat models' :: IO [[(Descriptor, R.Material)]]
  fdms <- mapM toDescriptorMat fonts'  :: IO [[(Descriptor, R.Material)]]
  idms <- mapM toDescriptorMat icons'  :: IO [[(Descriptor, R.Material)]]
    
  let -- this basically collects all the materials, reads textures from them and uniquely binds
    txs   = concatMap (\(_,m) -> R.textures m) $ concat dms
    uuids = fmap T.uuid txs
    txord = DS.toList . DS.fromList $ zip uuids [0..] -- this guarantees unique texture (uuid) bindings

    ftxs   = concatMap (\(_,m) -> R.textures m) $ concat fdms
    fuuids = fmap T.uuid ftxs
    ftxord = DS.toList . DS.fromList $ zip fuuids [0..] -- this guarantees unique texture (uuid) bindings

    itxs   = concatMap (\(_,m) -> R.textures m) $ concat idms
    iuuids = fmap T.uuid itxs
    itxord = DS.toList . DS.fromList $ zip iuuids [0..] -- this guarantees unique texture (uuid) bindings
    prj    = initProject'
        
  putStrLn "Binding Textures..."
  txTuples  <- mapM (bindTexture  txord) txs  :: IO [(Texture, TextureObject)]
  ftxTuples <- mapM (bindTexture ftxord) ftxs :: IO [(Texture, TextureObject)]
  itxTuples <- mapM (bindTexture itxord) itxs :: IO [(Texture, TextureObject)]

  objs'  <- mapM (toObject txTuples dms)   (concatMap flatten $ preObjects prj) --toObjects     initProject' txTuples  dms
  --print $ "+++DEBUG+++" ++ show objs'
  fobjs' <- mapM (toObject ftxTuples fdms) (preFontObject prj) --toFontObjects initProject' ftxTuples fdms
  iobjs' <- mapM (toObject itxTuples idms) (preIconObject prj) --toFontObjects initProject' itxTuples idms

  animate
    window
    (1.0/60.0 :: Double) -- 60 fps?
    initSettings
    initGame
      { 
        objs      = objs'
      , uniforms =
          defaultUniforms
          { u_res   = (resX', resY')
          , u_cam_a = apt defaultCam
          , u_cam_f = foc defaultCam
          }
      , wgts =
        [ Cursor
          { active = True
          , icons  = iobjs'
          , cpos   = P (V2 0 0)
          , optionsW = defaultBackendOptions
          }
        , TextField
          { active = True
          , text   =
              [" TextField :"
              ,"lorem ipsum"
              ," dolor sit amet?.."]
          , fonts  = fobjs'
          , format = Format
            {
              alignment = CC
            , xres      = resX'
            , yres      = resY'
            , xoffset   = 0.0
            , yoffset   = 0.0
            , zoffset   = 0.0
            , soffset   = 0.0
            , ssize     = 0.0
            }
          , optionsW = defaultBackendOptions
          }
        , Selector
          { active  = True
          , icons   = iobjs'
          , objects = []
          }
        ]
      }
    runGame
  
  putStrLn "Exiting Game"
