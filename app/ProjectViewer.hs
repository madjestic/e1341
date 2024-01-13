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
        pname          = "test_object"
      , ptype          = Default
      , pidx           = 0
      , puuid          = nil
      , modelIDXs      = [2]
      , tsolvers       =
        [ Identity
        -- , Rotate
        --   { space = ObjectSpace
        --   , cxyz  = V3 0 0 0
        --   , rord  = XYZ
        --   , rxyz  = V3 0 0 (0.5)
        --   , avel  = V3 0 0 0.05 }
        -- , Translate
        --   { space   = WorldSpace
        --   , txyz    = V3 1.5 0 0
        --   , tvel    = V3 0.0 0 0
        --   , kinslv  = Identity }
        --   , Rotate
        --   { space   = ObjectSpace
        --   , cxyz    = V3 0 0 0
        --   , rord    = XYZ
        --   , rxyz    = V3 0 0 (0.5)
        --   , avel    = V3 0 0 (0.1)
        --   , kinslv =
        --     Speed
        --     { life = 1.0
        --     , age  = 0.0
        --     , inc  = 0.01
        --     , amp  = 1.0
        --     , func = id }
        --   }
        -- , Translate
        --  { space = WorldSpace
        --  , txyz  = V3 1.1 0 0
        --  , tvel  = V3 0.0 0 0 }
        ]
      , osolvers    =
        [ Identity
        , Selectable
        ]
        , options   = defaultBackendOptions
        , pparent   = nil
        , pchildren = []
      }
    ]
  , preFontObject =
    [ PreObject
      {
        pname      = "fonts"
      , ptype      = Font
      , pidx       = 0
      , puuid      = nil
      , modelIDXs  = [0..75]
      , tsolvers   = []
      , osolvers   = [ Identity ]
      , options    = defaultBackendOptions
      , pparent    = nil
      , pchildren  = []
      }
    ]
  , preIconObject =
    [ PreObject
      {
        pname      = "crosshair"
      , ptype      = Icon
      , pidx       = 0
      , puuid      = nil
      , modelIDXs  = [0]
      , tsolvers   = []      
      , osolvers   = [ Identity ]
      , options    = defaultBackendOptions
      , pparent    = nil
      , pchildren  = []
      }
    , PreObject
      {
        pname      = "brackets"
      , ptype      = Icon
      , pidx       = 1
      , puuid      = nil
      , modelIDXs  = [1]
      , tsolvers   = []      
      , osolvers   = [ Identity ]
      , options    = defaultBackendOptions'
      , pparent    = nil
      , pchildren  = []
      }
    ]
  , pcameras    = [ defaultCam ]
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
    gameLoop
  
  putStrLn "Exiting Game"
