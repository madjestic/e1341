{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import SDL hiding (Texture, normalize)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.MSF as TMSF
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
import Graphics.RedViz.Uniforms

import Graphics.RedViz.Entity as E

import GameLoop
import Debug.Trace as DT

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
  , fontModels = sharedFonts
  , iconModels =
    [
      "models/fnt_crosshair.gltf"
    , "models/brackets.gltf"
    ]
  , pobjects = 
    [ Schema
      { slable      = "test_object"
      , suuid       = nil
      , scmps       =
        [ Renderable
          { modelIDXs = [0,1]
          , drws      = []
          , active    = False
          , backend   = defaultBackendOptions
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
              { space   = WorldSpace
              , tvel    = V3 0.0 0 0
              , kinslv  = [] }
            , Turnable
              { space   = ObjectSpace
              , rord    = XYZ
              , cxyz    = V3 0 0 0
              , rxyz    = V3 0 0 0
              , avel    = V3 0 0 (0.05)
              , kinslv =
                [ Fadable
                  { life = 1.0
                  , age  = 0.0
                  , inc  = 0.01
                  , amp  = 1.0
                  , func = id }]
              }
            ]
          }
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
          , backend  = defaultBackendOptions
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
          , backend  = defaultBackendOptions
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
      { apt        = 50.0
      , foc        = 100.0 }
    , Movable
      { space  = WorldSpace
      --, txyz   = V3 0 0 20
      , tvel   = V3 0 0 0
      , kinslv = [] }
    , defaultCamTransformable
      { tslvrs =
        [ Identity
        , defaultControllable
          { cvel   = (V3 0 0 0) -- velocity
          , cypr   = (V3 0 0 0) -- rotation
          , cyprS  = (V3 0 0 0) -- sum of rotations
          }
        , Parentable
          { parent   = nil }
        ]
      }
    ]
  , schildren = []
  , sparent   = nil
  }

type DTime = Double

animate :: Window
        -> DTime
        -> GameSettings
        -> Game
        -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
        -> IO ()
animate window dt gs g sf = do
  reactimateB $ input >>> sfIO >>> output window
  SDL.quit
  where
    input    = arr (const (dt, (gs, ())))                            :: MSF IO b (DTime, (GameSettings, ()))
    sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) g :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output w = arrM (renderOutput w gs)                              :: MSF IO   (Game, Maybe Bool) Bool

main :: IO ()
main = do
  let
    opts = initSettings
    (resX', resY') =
      ( unsafeCoerce $ resX opts
      , unsafeCoerce $ resY opts ) :: (CInt, CInt)

  initProject' <- setProjectUUID $ initProject (resX opts) (resY opts)

  let
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
    txord = DS.toList . DS.fromList $ zip uuids [0..]   -- this guarantees unique texture (uuid) bindings

    ftxs   = concatMap (\(_,m) -> R.textures m) $ concat fdms
    fuuids = fmap T.uuid ftxs
    ftxord = DS.toList . DS.fromList $ zip fuuids [0..] 

    itxs   = concatMap (\(_,m) -> R.textures m) $ concat idms
    iuuids = fmap T.uuid itxs
    itxord = DS.toList . DS.fromList $ zip iuuids [0..] 
    prj    = initProject'
        
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
          { u_res = (resX', resY') }
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
            , xres      = resX opts
            , yres      = resY opts
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

  animate
    window
    (1.0/60.0 :: Double) -- 60 fps?
    initSettings
    initGame'
    gameLoop
  
  putStrLn "Exiting Game"
