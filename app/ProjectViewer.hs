{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CPP #-}
module Main where

import SDL hiding (Texture, normalize)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.MSF as TMSF
import Control.Concurrent (MVar, newMVar, swapMVar)
import Data.MonadicStreamFunction  
import Data.UUID (nil)
import Foreign.C.Types  
import Unsafe.Coerce
import Graphics.Rendering.OpenGL as GL hiding (normalize)
import Lens.Micro
import Lens.Micro.Extras
import GHC.Float
import Linear.Metric (normalize)
import Data.Set as DS ( fromList, toList )
import Data.Maybe (listToMaybe)

import Graphics.RedViz.Backend
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Game
import Graphics.RedViz.Material as R
import Graphics.RedViz.Project
import Graphics.RedViz.Rendering 
import Graphics.RedViz.Component as C
import Graphics.RedViz.Texture as T
import Graphics.RedViz.Widget
import Graphics.RedViz.Uniforms hiding (debug)

import Graphics.RedViz.Entity as E

import GameLoop
import Debug.Trace as DT

debug :: Bool
#ifdef DEBUGSHADERS
debug = True
#else
debug = False
#endif


initProject :: Int -> Int -> Project
initProject resx' resy' =
  Project
  {  
    projname = "Test Project"
  , resx    = resx'
  , resy    = resy'
  , camMode = "AbsoluteLocation"
  , models  =
    [ "models/pighead.gltf"         -- 0 
    , "models/grid.gltf"            -- 1
    , "models/adder_mk1_orig.gltf"  -- 2
    , "models/planet_orig.gltf"     -- 3
    , "models/stars_orig.gltf"      -- 4
    , "models/star_orig.gltf"       -- 5
    , "models/splines_square.gltf"  -- 6
    , "models/splines_circle.gltf"  -- 7
    ]
  , fontModels = sharedFonts
  , iconModels =
    [
      "models/fnt_crosshair.gltf"
    , "models/brackets.gltf"
    ]
  , pobjects = 
    [ Schema -- stars
      { slable = "stars" 
      , suuid  = nil
      , scmps  =
        [ Renderable
          { modelIDXs = [4]
          , drws      = []
          , active    = False
          , backend   = pointsOpts
          }
        ]
      , schildren = []        
      , sparent   = nil
      }

      , Schema -- spaceship
      { slable = "spaceship"
      , suuid  = nil
      , scmps  = 
        [ Renderable
          { modelIDXs = [2]
          , drws      = []
          , active    = False
          , backend   = defaultOptions
          } 
        , Selectable { selected = False }
        , Transformable
          { xform =  
            (V4
             (V4 1 0 0 3.14)   -- <- . . . x ...
             (V4 0 1 0 0)      -- <- . . . y ...
             (V4 0 0 1 200)    -- <- . . . z-component of transform
             (V4 0 0 0 1))
          , tslvrs =
            [ Identity
            , Parentable
              { parent   = nil }
            , PreTransformable
              { txyz = V3 0 0 0
              , rord = XYZ
              , rxyz = V3 0 (pi/2) 0
              }
            , Movable
              { space    = WorldSpace   :: CoordSys
              , tvel     = V3 0 0.014 0 :: V3 Double -- velocity
              , kslvrs   = [
                  Attractable
                  { mass = 1000.0
                  , acc  = V3 0 0 0
                  , fr   = 100
                  , ft   = 0 }
                ] :: [Component]
              } 
            ]
          }
        ]
      , schildren = []
      , sparent   = nil
      }
    , Schema -- star
      {
        slable = "star"
      , suuid  = nil
      , scmps  =
        [ Renderable
          { modelIDXs = [5]
          , drws      = []
          , active    = False
          , backend   = defaultOptions
          }
        , Selectable { selected = False }
        , Transformable
          { xform =  
            (V4
             (V4 1 0 0 0)   -- <- . . . x ...
             (V4 0 1 0 0)   -- <- . . . y ...
             (V4 0 0 1 0)   -- <- . . . z-component of transform
             (V4 0 0 0 1))
          , tslvrs =
            [ Identity
            , Movable
              { space    = WorldSpace   :: CoordSys
              , tvel     = V3 0 0 0 :: V3 Double -- velocity
              , kslvrs   = [
                  Attractable
                  { mass = 100000000000.0
                  , acc  = V3 0 0 0 }
                ] :: [Component]
              } 
            ]
          }
        ]
      , schildren = []
      , sparent   = nil
      }
    , Schema -- planet
      {
        slable = "planet"
      , suuid  = nil
      , scmps  =
        [ Renderable
          { modelIDXs = [3]
          , drws      = []
          , active    = False
          , backend   = defaultOptions
          }
        , Selectable { selected = False }
        , Transformable
          { xform =  
            (V4
             (V4 1 0 0 0)     -- <- . . . x ...
             (V4 0 1 0 0)     -- <- . . . y ...
             (V4 0 0 1 200)   -- <- . . . z-component of transform
             (V4 0 0 0 1))
          , tslvrs =
            [ Identity
            , Movable
              { space    = WorldSpace   :: CoordSys
              , tvel     = V3 0 0 0 :: V3 Double -- velocity
              , kslvrs   = [
                  Attractable
                  { mass = 1000000000.0
                  , acc  = V3 0 0 0 }
                ] :: [Component]
              } 
            ]
          }
        ]
      , schildren = []
      , sparent   = nil
      }
    , Schema -- orbit
      { slable = "orbit"
      , suuid  = nil
      , scmps  = 
        [ Renderable
          { modelIDXs = [7]
          , drws      = []
          , active    = False
          , backend   = defaultOptions
          }
        , Transformable
          { xform =  
            (V4
             (V4 1 0 0 0)   -- <- . . . x ...
             (V4 0 1 0 0)   -- <- . . . y ...
             (V4 0 0 1 0)   -- <- . . . z-component of transform
             (V4 0 0 0 1))
          , tslvrs =
            [ Identity
            , PreTransformable
              { txyz = V3 0 0 0
              , rord = XYZ
              --, rxyz = V3 (-pi/2) 0 0
              , rxyz = V3 (0) 0 0
              }
            ]
          }
        , Identity
        ]
      , schildren = []
      , sparent   = nil
      }      
    , Schema -- splines square
      { slable = "splines_square"
      , suuid  = nil
      , scmps  = 
        [ Renderable
          { modelIDXs = [6]
          , drws      = []
          , active    = False
          , backend   = 
            Options
            { primitiveMode = Triangles
            , bgrColor      = Color4 0.5 0.0 0.0 1.0
            , ptSize        = 1.0
            , depthMsk      = Enabled
            --, blendFunc     = (SrcColor, OneMinusSrcAlpha)
            , blendFunc     = (SrcAlpha, OneMinusSrcAlpha)
            }

          }
        , Transformable
          { xform =  
            (V4
             (V4 1 0 0 0)   -- <- . . . x ...
             (V4 0 1 0 0)   -- <- . . . y ...
             (V4 0 0 1 200) -- <- . . . z-component of transform
             (V4 0 0 0 1))
          , tslvrs =
            [ Identity
            , PreTransformable
              { txyz = V3 0 0 0
              , rord = XYZ
              --, rxyz = V3 (-pi/2) 0 0
              , rxyz = V3 (0) 0 0
              }
            ]
          }
        , Identity
        ]
      , schildren = []
      , sparent   = nil
      }
    ]
  , pfonts =
    [ Schema
      { slable = "fonts"
      , suuid  = nil
      , scmps =
        [ Renderable
          { modelIDXs = [0..75]
          , drws     = []
          , active   = False
          , backend  = defaultOptions
          }
        ]
      , schildren = []
      , sparent   = nil
      }
    ]
  , picons =
    [ Schema
      { slable = "crosshair"
      , suuid  = nil
      , scmps =
        [ Renderable
          { modelIDXs = [0]
          , drws     = []
          , active   = False
          , backend  = defaultOptions
          }
        ]
      , schildren = []
      , sparent   = nil
      }
    , Schema
      { slable = "brackets"
      , suuid  = nil
      , scmps =
        [ Renderable
          { modelIDXs = [1]
          , drws     = []
          , active   = False
          , backend  = linesOpts
          }
        ]
      , schildren = []
      , sparent   = nil
      }
    ]
  , pcameras    = [ playCam ]
  }

playCam :: Schema
playCam = 
  Schema
  { slable = "PlayerCamera"
  , suuid  = nil
  , scmps  =
    [ Camerable
      { foc        = 50.0
      , apt        = 100.0 }
    , Measurable
      { mass = 1.0 }
    , Transformable
      { xform =  
        (V4
         (V4 1 0 0 0)    -- <- . . . x ...
         (V4 0 1 0 25) -- <- . . . y ...
         (V4 0 0 1 30)  -- <- . . . z-component of transform //230
         (V4 0 0 0 1))
      , tslvrs =
        [ Identity
        , Controllable
          { cvel    = V3 0 0 0     
          , cypr    = V3 0 0 0
          , cyprS   = V3 0 0 0
          , mouseS  = -0.0000025 -- mouse sensitivity
          , rotS    =  0.005    -- rotation sensitivity
          , movS    =  0.1       -- translation sensitivity
          , parent  = nil
          , phys    = Dynamic}
        
        ]
      }
    ]
  , schildren = []
  , sparent   = nil
  }

type DTime = Double

animate :: Window
--        -> DTime
        -> GameSettings
        -> Game
        -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
        -> IO ()
animate window gs g sf = do
  lastInteraction <- newMVar =<< SDL.time
  currentTime     <- SDL.time
  dt              <- (currentTime -) <$> swapMVar lastInteraction currentTime --dtime
  
  reactimateB $ input (dt*1000) >>> sfIO >>> output window
    
  SDL.quit
  where
    input dt = arr (const (dt, (gs, ())))                            :: MSF IO b (DTime, (GameSettings, ()))
    sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) g :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output w = arrM (renderOutput w gs)                              :: MSF IO   (Game, Maybe Bool) Bool

main :: IO ()
main = do
  let
    opts = initSettings
    (resX', resY') =
      ( unsafeCoerce $ resX opts
      , unsafeCoerce $ resY opts ) :: (CInt, CInt)

  scene <- setProjectUUID $ initProject (resX opts) (resY opts)

  let
    models'     = models     scene :: [FilePath]
    fonts'      = fontModels scene :: [FilePath]
    icons'      = iconModels scene :: [FilePath]    

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
    txord = DS.toList . DS.fromList $ zip uuids [0..]   -- this guarantees unique texture (uuid) bindings

    ftxs   = concatMap (\(_,m) -> R.textures m) $ concat fdms
    fuuids = fmap T.uuid ftxs
    ftxord = DS.toList . DS.fromList $ zip fuuids [0..] 

    itxs   = concatMap (\(_,m) -> R.textures m) $ concat idms
    iuuids = fmap T.uuid itxs
    itxord = DS.toList . DS.fromList $ zip iuuids [0..] 
    prj    = scene
        
  putStrLn "Binding Textures..."
  txTuples  <- mapM (bindTexture  txord) txs  :: IO [(Texture, TextureObject)]
  ftxTuples <- mapM (bindTexture ftxord) ftxs :: IO [(Texture, TextureObject)]
  itxTuples <- mapM (bindTexture itxord) itxs :: IO [(Texture, TextureObject)]

  objs'  <- mapM (fromSchema txTuples  dms ) (concatMap flatten $ pobjects prj)
  fobjs' <- mapM (fromSchema ftxTuples fdms) (pfonts   prj)
  iobjs' <- mapM (fromSchema itxTuples idms) (picons   prj)
  cams'  <- mapM (fromSchema [] []         ) (pcameras prj)

  let
    initGame' =
      initGame
      { 
        objs = objs'
      , cams = cams'
      , unis =
          defaultUniforms
          { u_res   = (resX', resY')
          , u_cam_a = case listToMaybe cams' of Nothing -> 50; Just cams' -> apt . head .camerables $ cams'
          , u_cam_f = case listToMaybe cams' of Nothing -> 50; Just cams' -> foc . head .camerables $ cams'
          }
      , wgts =
        [
          Cursor
          { active = True
          , icons  = iobjs'
          , cpos   = P (V2 0 0)
          , optionsW = defaultOptions
          , format = Format
            {
              alignment = CC
            , xres      = resX opts
            , yres      = resY opts
            , xoffset   = 0.0
            , yoffset   = 0.0
            , zoffset   = 0.0
            , soffset   = 0.0
            , ssize     = 1.0
            }
          }
        , InfoField
          { active = True
          , text   = []
          , fonts  = fobjs'
          , format = Format
            {
              alignment = CC
            , xres      = resX opts
            , yres      = resY opts
            , xoffset   = 0.0
            , yoffset   = 0.0
            , zoffset   = 0.0
            , soffset   = 0.9
            , ssize     = 0.8
            }
          , optionsW = defaultOptions
          }
        , Selector
          { active  = True
          , icons   = iobjs'
          , objects = []
          , format = Format
            {
              alignment = CC
            , xres      = resX opts
            , yres      = resY opts
            , xoffset   = 0.0
            , yoffset   = 0.0
            , zoffset   = 0.0
            , soffset   = 0.0
            , ssize     = 1.0
            }
          }
        ]
      }

  animate
    window
    --(1.0/60.0)-- 1.0/60.0 ~= 60 fps?
    initSettings
    initGame'
    gameLoop
  
  putStrLn "Exiting Game"
