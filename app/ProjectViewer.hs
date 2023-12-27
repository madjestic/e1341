{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Text.GLTF.Loader as Gltf hiding (Texture, Material)
import Codec.GlTF.Material as Gltf
import Lens.Micro
import Control.Lens.Combinators (view)
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
import Linear.Matrix as LM  
import Data.Maybe (fromMaybe)
import Data.Set as DS ( fromList, toList )
import GHC.Generics

import Load_glTF (loadMeshPrimitives)
import Model_glTF

import Graphics.RedViz.Texture as T
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Backend
import Graphics.RedViz.LoadShaders
import Graphics.RedViz.Rendering (bindTexture')  
import Graphics.RedViz.Material as R

--import Debug.Trace as DT

type DTime = Double

data Transformable
  =  Transformable
     { xform  :: M44 Double
     , tslvrs :: [Solver]
     } deriving Show

defaultTransformable :: Transformable
defaultTransformable =
  Transformable
  { xform =  
      (V4
        (V4 1 0 0 0)    -- <- . . . x ...
        (V4 0 1 0 0)    -- <- . . . y ...
        (V4 0 0 1 0)   -- <- . . . z-component of transform
        (V4 0 0 0 1))
  , tslvrs = [Identity]
  }

defaultCamTransformable :: Transformable
defaultCamTransformable =
  Transformable
  { xform =  
      (V4
        (V4 1 0 0 0)    -- <- . . . x ...
        (V4 0 1 0 0)    -- <- . . . y ...
        (V4 0 0 1 10)   -- <- . . . z-component of transform
        (V4 0 0 0 1))
  , tslvrs = [Identity]
  }
  
data Camera
  =  Camera
     { name       :: String
     , apt        :: Double
     , foc        :: Double
     , ctransform :: Transformable
     , mouseS     :: V3 Double -- mouse    "sensitivity"
     , keyboardRS :: V3 Double -- keyboard "rotation sensitivity"
     , keyboardTS :: V3 Double -- keyboard "translation sensitivity"
     , cslvrs     :: [Solver]
     } deriving Show

defaultCam :: Camera
defaultCam =
  Camera
  {
    name       = "PlayerCamera"
  , apt        = 50.0
  , foc        = 100.0
  , ctransform = defaultCamTransformable { tslvrs = [defaultCamSolver]}
  , mouseS     = -0.0025
  , keyboardRS = 0.05
  , keyboardTS = 0.05
  }

defaultCamSolver :: Solver
defaultCamSolver =
  Controller
  { cvel   = (V3 0 0 0) -- velocity
  , cypr   = (V3 0 0 0) -- rotation
  , cyprS  = (V3 0 0 0) -- sum of rotations
  }

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving Show

data Solver =
    Identity
  | Translate
    { space :: CoordSys
    , txyz  :: V3 Double -- offset
    , tvel  :: V3 Double -- velocity
    }
  | Rotate
    { space :: CoordSys
    , cxyz  :: V3 Double -- center of rotation
    , rord  :: RotationOrder
    , rxyz  :: V3 Double
    , avel  :: V3 Double -- angular velocity
    }
  | Select
  | Controller
    { cvel  :: V3 Double  -- velocity
    , cypr  :: V3 Double  -- yaw/pitch/camRoll ~angular velocity
    , cyprS :: V3 Double  -- yaw/pitch/camRoll Sum
    }
  | CameraParent Object
    -- | Parent
  --   { parent :: Object | Camera}
  deriving Show

data RotationOrder =
  XYZ

instance Show RotationOrder where
  show XYZ = "XYZ"

data PType = Default
           | Font
           | Icon

instance Show PType where
  show Default = "Default"
  show Font    = "Font"
  show Icon    = "Icon"

data PreObject
  =  PreObject
     { pname      :: String
     , ptype      :: PType
     , pidx       :: Integer
     , uuid       :: UUID
     , modelIDXs  :: [Int]
     , tsolvers   :: [Solver] -- transformable solvers
     , osolvers   :: [Solver] -- properties solvers
     , options    :: BackendOptions
     } deriving Show

data Object
  =  Object
     { transform :: Transformable
     , drws      :: [Drawable]
     , selected  :: Bool
     , oslvrs    :: [Solver]
     } deriving Show

initObj :: Object
initObj =
  Object
  { transform = defaultTransformable
  , drws     = []
  , oslvrs   = []
  , selected = False
  }
  
testM44 :: M44 Double  
testM44 =
  (V4
    (V4 1 0 0 0.5) -- <- . . . x ...
    (V4 0 1 0 0)   -- <- . . . y ...
    (V4 0 0 1 0)   -- <- . . . z-component of transform
    (V4 0 0 0 1))
  
toObject :: [(Texture, TextureObject)] -> [[(Descriptor, R.Material)]]-> PreObject -> IO Object
toObject txTuples' dms' pobj = do
  --print $ (options pobj)
  let
    dms      = (dms'!!) <$> modelIDXs pobj
    txs      = concatMap (\(_,m) -> R.textures m) $ concat dms :: [Texture]
    txTuples = filter (\(tx,_) -> tx `elem` txs) txTuples'     :: [(Texture, TextureObject)]
    drs =
      toDrawable
      (identity :: M44 Double) -- TODO: add result based on solvers composition
      (options pobj)
      txTuples
      <$> concat dms
      :: [Drawable]
    
    obj =
      Object
      { transform = defaultTransformable {tslvrs = tsolvers pobj}
      , drws      = drs
      , oslvrs    = osolvers pobj
      , selected  = False }

  return obj

data Drawable
  =  Drawable
     { descriptor :: Descriptor
     , material   :: R.Material
     , dtxs       :: [(Int, (Texture, TextureObject))]
     , doptions   :: BackendOptions
     , u_xform    :: M44 Double
     } deriving Show  

data Uniforms
  =  Uniforms
     { u_time  :: Double
     , u_res   :: (CInt, CInt)
     , u_cam   :: M44 Double
     , u_cam_a :: Double
     , u_cam_f :: Double
     , u_cam_ypr   :: (Double, Double, Double)
     , u_cam_yprS  :: (Double, Double, Double)
     , u_cam_vel   :: (Double, Double, Double)
     , u_cam_accel :: (Double, Double, Double)
     } deriving Show

defaultUniforms :: Uniforms
defaultUniforms = 
  Uniforms
  { u_time  = 0.0
  , u_res   = (800,600)
  , u_cam   = identity :: M44 Double
  , u_cam_a = 50.0
  , u_cam_f = 100.0
  , u_cam_ypr   = (0,0,0)
  , u_cam_yprS  = (0,0,0)
  , u_cam_vel   = (0,0,0)
  , u_cam_accel = (0,0,0) }

data Project
  =  Project
     {
       projname       :: String
     , resx           :: Int
     , resy           :: Int
     , camMode        :: String
     , models         :: [FilePath]
     , fontModels     :: [FilePath]
     , iconModels     :: [FilePath]     
     , preObjects     :: [PreObject]
     , preFontObject  :: [PreObject]
     , preIconObject  :: [PreObject]     
     , pcameras        :: [Camera]
     } deriving Show

initProject :: Int -> Int -> Project
initProject resx' resy' =
  Project
  {  
    projname = "Test Project"
  , resx    = resx'
  , resy    = resy'
  , camMode = "AbsoluteLocation"
  , models  =
    [ "models/pighead.gltf"
    , "models/grid.gltf"
    ]
  , fontModels =
    [ "models/fnt_space.gltf"
    , "models/fnt_0.gltf"
    , "models/fnt_1.gltf"
    , "models/fnt_2.gltf"
    , "models/fnt_3.gltf"
    , "models/fnt_4.gltf"
    , "models/fnt_5.gltf"
    , "models/fnt_6.gltf"
    , "models/fnt_7.gltf"
    , "models/fnt_8.gltf"
    , "models/fnt_9.gltf"
    , "models/fnt_a.gltf"
    , "models/fnt_b.gltf"
    , "models/fnt_c.gltf"
    , "models/fnt_d.gltf"
    , "models/fnt_e.gltf"
    , "models/fnt_f.gltf"
    , "models/fnt_g.gltf"
    , "models/fnt_h.gltf"
    , "models/fnt_i.gltf"
    , "models/fnt_j.gltf"
    , "models/fnt_k.gltf"
    , "models/fnt_l.gltf"
    , "models/fnt_m.gltf"
    , "models/fnt_n.gltf"
    , "models/fnt_o.gltf"
    , "models/fnt_p.gltf"
    , "models/fnt_q.gltf"
    , "models/fnt_r.gltf"
    , "models/fnt_s.gltf"
    , "models/fnt_t.gltf"
    , "models/fnt_u.gltf"
    , "models/fnt_v.gltf"
    , "models/fnt_w.gltf"
    , "models/fnt_x.gltf"
    , "models/fnt_y.gltf"
    , "models/fnt_z.gltf"
    , "models/fnt_plus.gltf"
    , "models/fnt_minus.gltf"
    , "models/fnt_equal.gltf"
    , "models/fnt_gt.gltf"
    , "models/fnt_comma.gltf"
    , "models/fnt_dot.gltf"
    , "models/fnt_question.gltf"
    , "models/fnt_exclam.gltf"
    , "models/fnt_asterics.gltf"
    , "models/fnt_slash.gltf"
    , "models/fnt_semicolon.gltf"
    , "models/fnt_quote.gltf"
    , "models/fnt_A.gltf"
    , "models/fnt_B.gltf"
    , "models/fnt_C.gltf"
    , "models/fnt_D.gltf"
    , "models/fnt_E.gltf"
    , "models/fnt_F.gltf"
    , "models/fnt_G.gltf"
    , "models/fnt_H.gltf"
    , "models/fnt_I.gltf"
    , "models/fnt_J.gltf"
    , "models/fnt_K.gltf"
    , "models/fnt_L.gltf"
    , "models/fnt_M.gltf"
    , "models/fnt_N.gltf"
    , "models/fnt_O.gltf"
    , "models/fnt_P.gltf"
    , "models/fnt_Q.gltf"
    , "models/fnt_R.gltf"
    , "models/fnt_S.gltf"
    , "models/fnt_T.gltf"
    , "models/fnt_U.gltf"
    , "models/fnt_V.gltf"
    , "models/fnt_W.gltf"
    , "models/fnt_X.gltf"
    , "models/fnt_Y.gltf"
    , "models/fnt_Z.gltf"
    , "models/fnt_crosshair.gltf"
    ]
  , iconModels =
    [
      "models/fnt_crosshair.gltf"
    , "models/brackets.gltf"
    ]
  , preObjects = 
    [ PreObject
      {
        pname          = "test_object"
      , ptype          = Default
      , pidx           = 0
      , uuid           = nil
      , modelIDXs      = [0,1]
      , tsolvers       =
        [ Identity
        -- , Rotate
        --   { space = ObjectSpace
        --   , cxyz  = V3 0 0 0
        --   , rord  = XYZ
        --   , rxyz  = V3 0 0 (0.5)
        --   , avel  = V3 0 0 0.05 }
        , Translate
          { space = WorldSpace
          , txyz  = V3 1.5 0 0
          , tvel  = V3 0.0 0 0 }
        , Rotate
          { space = ObjectSpace
          , cxyz  = V3 0 0 0
          , rord  = XYZ
          , rxyz  = V3 0 0 (0.5)
          , avel  = V3 0 0 (0.1) }
        -- , Translate
        --  { space = WorldSpace
        --  , txyz  = V3 1.1 0 0
        --  , tvel  = V3 0.0 0 0 }
        ]
      , osolvers        =
        [ Identity
        , Select
        ]
        , options        = defaultBackendOptions
      }
    ]
  , preFontObject =
    [ PreObject
      {
        pname          = "fonts"
      , ptype          = Font
      , pidx           = 0
      , uuid           = nil
      , modelIDXs      = [0..75]
      , osolvers       = [ Identity ]
      , options        = defaultBackendOptions
      }
    ]
  , preIconObject =
    [ PreObject
      {
        pname          = "crosshair"
      , ptype          = Icon
      , pidx           = 0
      , uuid           = nil
      , modelIDXs      = [0]
      , osolvers       = [ Identity ]
      , options        = defaultBackendOptions
      }
    , PreObject
      {
        pname          = "brackets"
      , ptype          = Icon
      , pidx           = 1
      , uuid           = nil
      , modelIDXs      = [1]
      , osolvers       = [ Identity ]
      , options        = defaultBackendOptions'
      }
    ]
  , pcameras    = [ defaultCam ]
  }

data Alignment =
   TL |TC |TR
  |CL |CC |CR
  |BL |BC |BR
  deriving (Generic, Show)


data Format -- move to Format.hs?
  =  Format
     { alignment :: Alignment
     , xres      :: Int
     , yres      :: Int
     , xoffset   :: Double
     , yoffset   :: Double
     , zoffset   :: Double
     , soffset   :: Double -- scale Offset
     , ssize     :: Double -- scale Size
     } deriving (Generic, Show)

data Widget
  =  Empty
  |  TextField
     { active   :: Bool
     , text     :: [String]
     , fonts    :: [Object]
     , format   :: Format
     , optionsW :: BackendOptions
     }
  |  Cursor
     { active   :: Bool
     , icons    :: [Object]     
     , cpos     :: Point V2 CInt
     , optionsW :: BackendOptions
     }
  |  Selector
     { active  :: Bool
     , icons   :: [Object]
     , objects :: [Object]
     }
  deriving (Generic, Show)

data Game = Game
  { tick     :: Integer
  , mpos     :: Point V2 CInt
  , quitGame :: Bool
  , cameras  :: [Camera]
  , uniforms :: Uniforms
  , objs     :: [Object]
  , wgts     :: [Widget]
  } deriving Show

data GameSettings = GameSettings
  { resX :: Int 
  , resY :: Int 
  } deriving Show

initGame :: Game
initGame =
  Game
  { tick     = -1
  , mpos     = P (V2 0 0)
  , quitGame = False
  , cameras  = [defaultCam]
  , uniforms = defaultUniforms
  , objs     = []
  , wgts     = []
  }

initSettings :: GameSettings
initSettings = GameSettings
  { resX = 1280
  , resY = 720 }

type Time = Double
type Res  = (CInt, CInt)

unzipWith :: Eq a => [a] -> [(a,b)] -> [(a,b)]
unzipWith xs xys = xys'
  where
    xys' = filter (\xy -> fst xy `elem` xs) xys

toDrawable
  :: M44 Double
  -> BackendOptions
  -> [(Texture, TextureObject)]
  -> (Descriptor, R.Material)
  -> Drawable
toDrawable xform' opts txos (d, mat') = dr
  where
    txs'   = R.textures mat'
    txos'  = zip [0..] $ unzipWith txs' txos :: [(Int, (Texture, TextureObject))] 
    dr =
      Drawable
      { 
        u_xform    = xform'
      , descriptor = d
      , material   = mat'
      , dtxs       = txos'
      , doptions   = opts
      }

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
            t0 { xform  = foldr1 (!*!) $ solve (xform t0) <$> tslvrs t0
               , tslvrs = updateSolver <$> tslvrs t0 }
            where
              updateSolver :: Solver -> Solver
              updateSolver slv =
                case slv of
                  Identity               -> slv
                  Translate _ pos vel    -> slv { txyz   = pos  + vel }
                  Rotate _ _ _ rxyz avel -> slv { rxyz   = rxyz + avel }
                  _ -> slv

              solve :: M44 Double -> Solver -> M44 Double
              solve mtx0 slv =
                case slv of
                  Identity -> identity
                  Translate cs pos _ ->
                    case cs of
                      WorldSpace  -> identity & translation .~ pos
                      ObjectSpace -> undefined
                  Rotate _ _ rord rxyz _ -> transform' identity
                    where
                      transform' :: M44 Double -> M44 Double
                      transform' mtx0 = mtx
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
                                        fromQuaternion (axisAngle (mtx0^.(_m33._x)) (rxyz^._x)) -- pitch
                                    !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (rxyz^._y)) -- yaw
                                    !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (rxyz^._z)) -- roll
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
                                     -> Solver
                                     -> Solver
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
  
-- < Rendering > ----------------------------------------------------------
openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"

    let config = 
          OpenGLConfig { glColorPrecision     = V4 8 8 8 0
                       , glDepthPrecision     = 24
                       , glStencilPrecision   = 8
                       , glMultisampleSamples = 4
                       , glProfile            = Core Normal 4 5
                       }
     
    window <- SDL.createWindow
              title
              SDL.defaultWindow
              { SDL.windowInitialSize     = V2 sizex sizey
              , SDL.windowGraphicsContext = OpenGLContext config }

    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

type Pos        = (Double, Double)  
data Shape      = Square Pos Double
                deriving Show


data Projection = Planar                
                deriving Show 

type UV         = [TexCoord2 Double] 

toUV :: Projection -> UV
toUV Planar =
  projectPlanar ps
  where
    projectPlanar :: [Pos] -> UV
    projectPlanar = map $ uncurry TexCoord2                                                                   
    ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
         ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)] :: [Pos]

toDescriptorMat :: FilePath -> IO [(Descriptor, R.Material)]
toDescriptorMat file = do
  (stuff, mats) <- loadGltf file -- "models/pighead.gltf"
  mats' <- mapM fromGltfMaterial mats
  print mats'
  ds    <- mapM (\((vs, idx), mat) -> initResources idx vs mat) $ zip (concat stuff) mats'
  return $ zip ds mats'
    where
      fromGltfMaterial :: Gltf.Material -> IO R.Material
      fromGltfMaterial mat =
        R.read 
        $ case Gltf.name mat of
            Nothing -> "./mat/checkerboard/checkerboard"
            Just s  -> "./mat/" ++ unpack s ++ "/" ++ unpack s

fromVertex3 :: Vertex3 Double -> [GLfloat]
fromVertex3 (Vertex3 x y z) = [double2Float x, double2Float y, double2Float z]

initResources :: [GLfloat] -> [GLenum] -> R.Material -> IO Descriptor
initResources vs idx mat =  
  do
    -- print $ mat
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length idx
    withArray idx $ \ptr ->
      do
        let indexSize = fromIntegral $ numIndices * sizeOf (0 :: GLenum)
        bufferData ElementArrayBuffer $= (indexSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 8 * floatSize

    -- | Positions
    let vPosition = AttribLocation 0
        posOffset = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | Colors
    let vaRGBA     = AttribLocation 1
        rgbaOffset = 3 * floatSize
    vertexAttribPointer vaRGBA  $=
        (ToFloat, VertexArrayDescriptor 4 Float stride (bufferOffset rgbaOffset))
    vertexAttribArray vaRGBA    $= Enabled

    -- | UV
    let uvCoords = AttribLocation 2
        uvOffset = 6 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled

    -- || Shaders
    -- print $ "mat : " ++ show mat
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource $ vertShader mat),
        ShaderInfo FragmentShader (FileSource $ fragShader mat)]
    currentProgram $= Just program

    -- || Unload buffers
    bindVertexArrayObject         $= Nothing

    return $ Descriptor triangles (fromIntegral numIndices) program

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
  
renderOutput :: Window -> GameSettings -> (Game, Maybe Bool) -> IO Bool
renderOutput _ _ ( _,Nothing) = quit >> return True

renderOutput window gs (g,_) = do
  let
  clearColor $= Color4 0.0 0.0 0.0 1.0
  GL.clear [ColorBuffer, DepthBuffer]

  GL.pointSize $= 10.0
  GL.blend $= Enabled
  GL.depthMask $= Enabled
  depthFunc $= Just Less
  cullFace  $= Just Back

  mapM_ (renderObject (head $ cameras g) (uniforms g)) (objs g)
  mapM_ (renderWidget (head $ cameras g) (uniforms g)) (wgts g)

  glSwapWindow window >> return False

renderWidget :: Camera -> Uniforms -> Widget -> IO ()
renderWidget cam unis' wgt = case wgt of
  Empty                   -> do return ()
  Cursor  False _ _ _     -> do return ()
  Cursor  {} ->
    (\dr -> do
        bindUniforms cam unis' (formatDrw (format wgt) dr) 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr
        ) (head idrs) -- cursor font index is 75
    where
      idrs = concatMap drws (icons wgt)
  TextField False _ _ _ _   -> do return ()
  TextField _ s _ fmt _ ->
    mapM_
    (\dr -> do
        bindUniforms cam unis' dr 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr
        ) $ formatText fmt wdrs s (0,0)
  Selector _ icons' objs' -> 
    mapM_
    (\obj -> do
        mapM_
          (\dr -> do
              bindUniforms cam unis' dr {u_xform = xform (transform obj)} 
              let (Descriptor triangles numIndices _) = descriptor dr
              bindVertexArrayObject $= Just triangles
              --drawElements (primitiveMode $ doptions dr) numIndices GL.UnsignedInt nullPtr
              drawElements (GL.Lines) numIndices GL.UnsignedInt nullPtr
          ) (drws (icons'!!1))) objs'
  where
    wdrs = concatMap drws (fonts wgt)

type CursorPos = (Integer, Integer)

formatText :: Format -> [Drawable] -> [String] -> CursorPos -> [Drawable]
formatText _ _ [] _  = []
formatText fmt drws [s] (x,y) =
  formatString fmt drws s (x,y)
formatText fmt drws (s:ss) (x,y) =
  formatText fmt drws [s] (x,y) ++ formatText fmt drws ss (x,y+1)

formatString :: Format -> [Drawable] -> String -> CursorPos -> [Drawable]
formatString _ _ [] _ = []
formatString fmt drws [c]    (x,y) = [formatChar fmt drws c (x,y)]
formatString fmt drws (c:cs) (x,y) =  formatChar fmt drws c (x,y) : formatString fmt drws cs (x+1,y)

formatChar :: Format -> [Drawable] -> Char -> CursorPos -> Drawable
formatChar _ drws chr cpos =
  case chr of
    ' ' -> offsetDrw cpos (drws!!0)
    '0' -> offsetDrw cpos (drws!!1)
    '1' -> offsetDrw cpos (drws!!2)
    '2' -> offsetDrw cpos (drws!!3)
    '3' -> offsetDrw cpos (drws!!4)
    '4' -> offsetDrw cpos (drws!!5)
    '5' -> offsetDrw cpos (drws!!6)
    '6' -> offsetDrw cpos (drws!!7)
    '7' -> offsetDrw cpos (drws!!8)
    '8' -> offsetDrw cpos (drws!!9)
    '9' -> offsetDrw cpos (drws!!10)
    'a' -> offsetDrw cpos (drws!!11)
    'b' -> offsetDrw cpos (drws!!12)
    'c' -> offsetDrw cpos (drws!!13)
    'd' -> offsetDrw cpos (drws!!14)
    'e' -> offsetDrw cpos (drws!!15)
    'f' -> offsetDrw cpos (drws!!16)
    'g' -> offsetDrw cpos (drws!!17)
    'h' -> offsetDrw cpos (drws!!18)
    'i' -> offsetDrw cpos (drws!!19)
    'j' -> offsetDrw cpos (drws!!20)
    'k' -> offsetDrw cpos (drws!!21)
    'l' -> offsetDrw cpos (drws!!22)
    'm' -> offsetDrw cpos (drws!!23)
    'n' -> offsetDrw cpos (drws!!24)
    'o' -> offsetDrw cpos (drws!!25)
    'p' -> offsetDrw cpos (drws!!26)
    'q' -> offsetDrw cpos (drws!!27)
    'r' -> offsetDrw cpos (drws!!28)
    's' -> offsetDrw cpos (drws!!29)
    't' -> offsetDrw cpos (drws!!30)
    'u' -> offsetDrw cpos (drws!!31)
    'v' -> offsetDrw cpos (drws!!32)
    'w' -> offsetDrw cpos (drws!!33)
    'x' -> offsetDrw cpos (drws!!34)
    'y' -> offsetDrw cpos (drws!!35)
    'z' -> offsetDrw cpos (drws!!36)
    '+' -> offsetDrw cpos (drws!!37)
    '-' -> offsetDrw cpos (drws!!38)
    '=' -> offsetDrw cpos (drws!!39)
    '>' -> offsetDrw cpos (drws!!40)
    ',' -> offsetDrw cpos (drws!!41)
    '.' -> offsetDrw cpos (drws!!42)
    '?' -> offsetDrw cpos (drws!!43)
    '!' -> offsetDrw cpos (drws!!44)
    '*' -> offsetDrw cpos (drws!!45)
    '/' -> offsetDrw cpos (drws!!46)
    ';' -> offsetDrw cpos (drws!!47)
    '\''-> offsetDrw cpos (drws!!48)
    'A' -> offsetDrw cpos (drws!!49)
    'B' -> offsetDrw cpos (drws!!50)
    'C' -> offsetDrw cpos (drws!!51)
    'D' -> offsetDrw cpos (drws!!52)
    'E' -> offsetDrw cpos (drws!!53)
    'F' -> offsetDrw cpos (drws!!54)
    'G' -> offsetDrw cpos (drws!!55)
    'H' -> offsetDrw cpos (drws!!56)
    'I' -> offsetDrw cpos (drws!!57)
    'J' -> offsetDrw cpos (drws!!58)
    'K' -> offsetDrw cpos (drws!!59)
    'L' -> offsetDrw cpos (drws!!60)
    'M' -> offsetDrw cpos (drws!!61)
    'N' -> offsetDrw cpos (drws!!62)
    'O' -> offsetDrw cpos (drws!!63)
    'P' -> offsetDrw cpos (drws!!64)
    'Q' -> offsetDrw cpos (drws!!65)
    'R' -> offsetDrw cpos (drws!!66)
    'S' -> offsetDrw cpos (drws!!67)
    'T' -> offsetDrw cpos (drws!!68)
    'U' -> offsetDrw cpos (drws!!69)
    'V' -> offsetDrw cpos (drws!!70)
    'W' -> offsetDrw cpos (drws!!71)
    'X' -> offsetDrw cpos (drws!!72)
    'Y' -> offsetDrw cpos (drws!!73)
    'Z' -> offsetDrw cpos (drws!!74)
    _   -> head drws

  
offsetDrw :: CursorPos -> Drawable -> Drawable
offsetDrw cpos drw =
  drw { u_xform = mkTransformationMat rot tr }
  where
    sh  = 0.1
    sv  = -0.15
    rot = identity :: M33 Double
    tr  =
      (identity::M44 Double)^.translation
      +
      V3 (fromIntegral $ fst cpos) (fromIntegral $ snd cpos) 0.0
      *
      V3 sh sv 0.0
      

formatDrw :: Format -> Drawable -> Drawable
formatDrw fmt dr = dr

  
renderObject :: Camera -> Uniforms -> Object -> IO ()
renderObject cam unis' obj = do
  mapM_ (\dr -> do
            bindUniforms cam unis' dr {u_xform = xform (transform obj)} 
            let (Descriptor triangles numIndices _) = descriptor dr
            bindVertexArrayObject $= Just triangles
            drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr
        ) (drws obj)
  
  
bindUniforms :: Camera -> Uniforms -> Drawable -> IO ()  
bindUniforms cam' unis' dr =  
  do
    let
      u_xform'  = u_xform  dr
      d'        = descriptor dr :: Descriptor
      u_cam'    = (xform.ctransform) cam'
      u_mouse'  = (0,0)
      (Uniforms u_time' u_res' _ u_cam_a' u_cam_f' u_ypr' u_yprS' u_vel' u_accel') = unis'
      (Descriptor _ _ u_prog') = d'

    currentProgram $= Just u_prog'

    let u_mouse0      = Vector2 (realToFrac $ fst u_mouse') (realToFrac $ snd u_mouse') :: Vector2 GLfloat
    location0         <- SV.get (uniformLocation u_prog' "u_mouse'")
    uniform location0 $= u_mouse0

    let resX          = fromIntegral $ fromEnum $ fst u_res' :: Double
        resY          = fromIntegral $ fromEnum $ snd u_res' :: Double
        u_res         = Vector2 (realToFrac resX) (realToFrac resY) :: Vector2 GLfloat

    location1         <- SV.get (uniformLocation u_prog' "u_resolution")
    uniform location1 $= u_res
    
    location2         <- SV.get (uniformLocation u_prog' "u_time")
    uniform location2 $= (double2Float u_time' :: GLfloat)

    let apt = u_cam_a' -- aperture
        foc = u_cam_f' -- focal length
        proj =
          LP.infinitePerspective
          (2.0 * atan ( apt/foc/2.0 )) -- FOV
          (resX/resY)                  -- Aspect
          0.01                         -- Near

    persp             <- GL.newMatrix RowMajor $ toList' proj   :: IO (GLmatrix GLfloat)
    location3         <- SV.get (uniformLocation u_prog' "persp")
    uniform location3 $= persp

    camera            <- GL.newMatrix RowMajor $ toList' u_cam' :: IO (GLmatrix GLfloat)
    location4         <- SV.get (uniformLocation u_prog' "camera")
    uniform location4 $= camera

    -- | Compensate world space xform with camera position
    -- = Object Position - Camera Position
    xform             <- GL.newMatrix RowMajor $ toList' (inv44 (identity & translation .~ u_cam'^.translation) !*! u_xform') :: IO (GLmatrix GLfloat)
    location5         <- SV.get (uniformLocation u_prog' "xform")
    uniform location5 $= xform

    let sunP = GL.Vector3 299999999999.0 0.0 0.0 :: GL.Vector3 GLfloat
    location7 <- SV.get (uniformLocation u_prog' "sunP")
    uniform location7 $= sunP
    
    let ypr  =
          Vector3
          (double2Float $ u_ypr'^._1)
          (double2Float $ u_ypr'^._2)
          (double2Float $ u_ypr'^._3)
          :: Vector3 GLfloat
    location8        <- SV.get (uniformLocation u_prog' "ypr")
    uniform location8 $= ypr

    let yprS =
          Vector3
          (double2Float $ u_yprS'^._1)
          (double2Float $ u_yprS'^._2)
          (double2Float $ u_yprS'^._3)
          :: Vector3 GLfloat
    location9        <- SV.get (uniformLocation u_prog' "yprS")
    uniform location9 $= yprS


    let vel  =
          Vector3
          (double2Float $ u_vel'^._1)
          (double2Float $ u_vel'^._2)
          (double2Float $ u_vel'^._3)
          :: Vector3 GLfloat
    location10        <- SV.get (uniformLocation u_prog' "vel")
    uniform location10 $= vel

    let accel  =
          Vector3
          (double2Float $ u_accel'^._1)
          (double2Float $ u_accel'^._2)
          (double2Float $ u_accel'^._3)
          :: Vector3 GLfloat
    location11        <- SV.get (uniformLocation u_prog' "accel")
    uniform location11 $= accel

    -- || Set Transform Matrix
    let tr :: [GLfloat]
        tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ]

    transform <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location12 <- SV.get (uniformLocation u_prog' "transform")
    uniform location12 $= transform

    -- | Allocate Textures
    texture Texture2D        $= Enabled
    mapM_ allocateTextures (dtxs dr) -- TODO: this is ignored, should bind an appropriate texture

    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing
      where        
        toList' = fmap realToFrac.concat.(fmap DF.toList.DF.toList) :: V4 (V4 Double) -> [GLfloat]
          
allocateTextures :: (Int, (Texture, TextureObject)) -> IO ()
allocateTextures (txid, (_, txo)) =
  do
    activeTexture $= TextureUnit (fromIntegral txid)
    textureBinding Texture2D $= Just txo
    return ()

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
    initProject'= Main.initProject resX' resY'
    models'     = models     initProject' :: [FilePath]
    fonts'      = fontModels initProject' :: [FilePath]
    icons'      = iconModels initProject' :: [FilePath]    
  --print $ "fonts' length : " ++ show (length fonts')
  
  -- TODO: if UUIDs are needed, generate like so:
  -- (const nextRandom) ()
  -- 10514e78-fa96-444a-8c3d-0a8445e771ad

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
  txTuples  <- mapM (bindTexture'  txord) txs  :: IO [(Texture, TextureObject)]
  ftxTuples <- mapM (bindTexture' ftxord) ftxs :: IO [(Texture, TextureObject)]
  itxTuples <- mapM (bindTexture' itxord) itxs :: IO [(Texture, TextureObject)]

  objs'  <- mapM (toObject txTuples dms)   (preObjects    prj) --toObjects     initProject' txTuples  dms
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

defaultGltfMat :: Gltf.Material
defaultGltfMat = Gltf.Material
  { emissiveFactor = (0,0,0)
  , alphaMode      = MaterialAlphaMode {unMaterialAlphaMode = "OPAQUE"}
  , alphaCutoff    = 0.5
  , doubleSided    = False
  , pbrMetallicRoughness = Nothing
  , normalTexture        = Nothing
  , occlusionTexture     = Nothing
  , emissiveTexture      = Nothing
  , name                 = Just "test"
  , extensions           = Nothing
  , extras               = Nothing
  } 

loadGltf :: FilePath -> IO ([[([GLenum],[GLfloat])]], [Gltf.Material])
loadGltf fp = do
  (root, meshPrimitives) <- loadMeshPrimitives False False fp
  let
    mgrs = V.toList <$> V.toList meshPrimitives :: [[Model_glTF.MeshPrimitive]]
    positions = (fmap.fmap) (\(_, stuff) -> sPositions stuff) mgrs :: [[V.Vector Packed]]
    indices   = (fmap.fmap) (\(_, stuff) -> sIndices   stuff) mgrs 
    idx       = (fmap.fmap) V.toList indices 
    attrs     = (fmap.fmap) (\(_, stuff) -> sAttrs     stuff) mgrs :: [[V.Vector VertexAttrs]]
    uvs       = (fmap.fmap) vaTexCoord <$> (fmap.fmap) V.toList attrs 
    colors    = (fmap.fmap) vaRGBA     <$> (fmap.fmap) V.toList attrs
    --normals   = (fmap.fmap.fmap) vaNormal   $ (fmap.fmap) V.toList attrs
    matTuples = (fmap.fmap) (\(maybeMatTuple, _) -> fromMaybe (0, defaultGltfMat) maybeMatTuple) mgrs :: [[(Int, Gltf.Material)]]
    mats      = (fmap.fmap) snd matTuples :: [[Gltf.Material]]

    ps = (fmap.fmap) (fromVec3' . unPacked) <$> ((fmap.fmap) V.toList positions) :: [[[(Float,Float,Float)]]]
    cs = (fmap.fmap.fmap) fromVec4' colors :: [[[(Float,Float,Float,Float)]]]
    ts = (fmap.fmap.fmap) fromVec2' uvs
    d = (,,) <$$$.> ps <***.> cs <***.> ts
    verts = (fmap.fmap.concatMap) (\((x,y,z),(cr,cg,cb,ca),(u,v)) -> [x,y,z,cr,cg,cb,u,v]) d
  return $ (zipWith zip idx verts, concat mats)

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = fmap

(<$$$.>) :: (a -> b) -> [[[a]]] -> [[[b]]]
(<$$$.>) = fmap . fmap . fmap

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

(<***.>) :: [[[a -> b]]] -> [[[a]]] -> [[[b]]]
(<***.>) =  (zipWith . zipWith . zipWith) ($)

fromVec2' :: Vec2 -> (Float, Float)
fromVec2' xy = withVec2 (coerce xy) (,)
  
fromVec3' :: Vec3 -> (Float, Float, Float)
fromVec3' xyz = withVec3 (coerce xyz) (,,)

fromVec4' :: Vec4 -> (Float, Float, Float, Float)
fromVec4' xyzw = withVec4 (coerce xyzw) (,,,)

getVertices :: Gltf -> V.Vector (V3 Float)
getVertices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitivePositions) (mesh ^. _meshPrimitives)

getIndices :: Gltf -> V.Vector Word16
getIndices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitiveIndices) (mesh ^. _meshPrimitives)
