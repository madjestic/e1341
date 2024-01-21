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
import Graphics.RedViz.Camera
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Drawable
import Graphics.RedViz.Game
import Graphics.RedViz.Material as R
import Graphics.RedViz.Object
import Graphics.RedViz.Project
import Graphics.RedViz.Rendering 
import Graphics.RedViz.Solvable
import Graphics.RedViz.Texture as T
import Graphics.RedViz.Transformable
import Graphics.RedViz.Widget
import Graphics.RedViz.Uniforms

import GameLoop
--import Debug.Trace as DT

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
    , "models/adder_mk1.gltf"
    , "models/planet.gltf"
    , "models/stars.gltf"
    ]
  , fontModels = sharedFonts
  , iconModels =
    [
      "models/fnt_crosshair.gltf"
    , "models/brackets.gltf"
    ]
  , preObjects = 
    [ PreObject
      {
        pname          = "spaceship"
      , ptype          = Default
      , puuid          = nil
      , modelIDXs      = [2]
      , tsolvers       =
        [ Identity
        , Movable
          { space  = WorldSpace
          , txyz   = V3 4 0 0
          , tvel   = V3 0 0 0
          , kinslv = [] }
        , Attractable
          { mass = 1000.0
          , acc  = V3 0 0 0 }
        ]
      , posolvers      =
        [ Identity
        , Selectable
        ]
        , options   = defaultBackendOptions
        , pparent   = nil
        , pchildren = []
        , pactive   = True
      }
    , PreObject
      {
        pname          = "planet"
      , ptype          = Default
      , puuid          = nil
      , modelIDXs      = [3]
      , tsolvers       =
        [ Identity
        , Movable
          { space  = WorldSpace
          , txyz   = V3 0 0 0
          , tvel   = V3 0 0 0
          , kinslv = [] }
        , Attractable
          { mass = 1000000.0
          , acc  = V3 0 0 0 }
        ]
      , posolvers      =
        [ Identity
        , Selectable
        ]
        , options   = defaultBackendOptions
        , pparent   = nil
        , pchildren = []
        , pactive   = False
      }
    , PreObject
      {
        pname          = "stars"
      , ptype          = Default
      , puuid          = nil
      , modelIDXs      = [4]
      , tsolvers       =
        [ Identity ]
      , posolvers      =
        [ Identity ]
        , options   = pointsOpts
        , pparent   = nil
        , pchildren = []
        , pactive   = False
      }
    ]
  , preFontObject =
    [ PreObject
      {
        pname      = "fonts"
      , ptype      = Font
      , puuid      = nil
      , modelIDXs  = [0..75]
      , tsolvers   = []
      , posolvers  = [ Identity ]
      , options    = defaultBackendOptions
      , pparent    = nil
      , pchildren  = []
      , pactive    = False
      }
    ]
  , preIconObject =
    [ PreObject
      {
        pname      = "crosshair"
      , ptype      = Icon
      , puuid      = nil
      , modelIDXs  = [0]
      , tsolvers   = []      
      , posolvers   = [ Identity ]
      , options    = defaultBackendOptions
      , pparent    = nil
      , pchildren  = []
      , pactive    = False
      }
    , PreObject
      {
        pname      = "brackets"
      , ptype      = Icon
      , puuid      = nil
      , modelIDXs  = [1]
      , tsolvers   = []      
      , posolvers   = [ Identity ]
      , options    = linesOpts
      , pparent    = nil
      , pchildren  = []
      , pactive    = False
      }
    ]
  --, pcameras    = [ defaultCam' ]
  , pcameras    = [ defaultCam' ]
  }

defaultCam' :: Camera  -- TODO: somehow a degault cam is read instead!
defaultCam' =
  Camera
  {
    name       = "PlayerCamera"
  , apt        = 50.0
  , foc        = 100.0
  , ctransform =
    defaultCamTransformable
    { tslvrs =
      [ Identity
      , Controllable
        { cvel   = (V3 0 0 0) -- velocity
        , cypr   = (V3 0 0 0) -- rotation
        , cyprS  = (V3 0 0 0) -- sum of rotations
        }
      , Parentable { parent = nil }
      ]
    }
  , mouseS     = -0.0025
  , keyboardRS = 0.05
  , keyboardTS = 0.05
  , cslvrs     = []
  , uuid       = nil
  , parent     = nil
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
  quit
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

  objs'  <- mapM (toObject txTuples  dms)  (concatMap flatten $ preObjects prj)
  fobjs' <- mapM (toObject ftxTuples fdms) (preFontObject prj)
  iobjs' <- mapM (toObject itxTuples idms) (preIconObject prj)

  let
    (_, initGame') = runState stepOnce $
      initGame
      { 
        objs      = objs'
      , cameras   = pcameras prj
      , uniforms =
          defaultUniforms
          { u_res   = (resX', resY') }
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
