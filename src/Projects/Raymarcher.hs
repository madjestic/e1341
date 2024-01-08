module Projects.Raymarcher where

import Data.UUID
import Linear.V3
import Linear.V4

import Graphics.RedViz.Solvable
import Graphics.RedViz.Backend
import Graphics.RedViz.Camera
import Graphics.RedViz.Project
import Graphics.RedViz.Object
import Graphics.RedViz.Transformable

raymarchProject :: Int -> Int -> Project
raymarchProject resx' resy' =
  Project
  {  
    projname = "Test Project"
  , resx    = resx'
  , resy    = resy'
  , camMode = "AbsoluteLocation"
  , models  =
    [ "models/pighead.gltf"
    , "models/grid.gltf"
    , "models/raymarch_square.gltf"
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
        pname          = "raymarch_square"
      , ptype          = Default
      , pidx           = 0
      , puuid          = nil
      , modelIDXs      = [2]
      , tsolvers       =
        [ Identity ]
      , osolvers    =
        [ Identity ]
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
  , pcameras    = [ projectCam ]
  }

projectCam :: Camera
projectCam =
  Camera
  {
    name       = "PlayerCamera"
  , apt        = 50.0
  , foc        = 100.0
  , ctransform = projectCamTransformable { tslvrs = [defaultCamSolver]}
  , mouseS     = -0.0025
  , keyboardRS = 0.05
  , keyboardTS = 0.05
  , cslvrs     = []
  , uuid       = nil
  , parent     = nil
  }

projectCamTransformable :: Transformable
projectCamTransformable =
  Transformable
  { xform =  
      (V4
        (V4 1 0 0 0)    -- <- . . . x ...
        (V4 0 1 0 0)    -- <- . . . y ...
        (V4 0 0 1 3)   -- <- . . . z-component of transform
        (V4 0 0 0 1))
  , tslvrs = [Identity]
  }
