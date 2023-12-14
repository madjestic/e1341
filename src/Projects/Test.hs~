module Projects.Test where

import Data.UUID

import Graphics.RedViz.Project
import Graphics.RedViz.Backend

defaultPreGUI :: PreGUI
defaultPreGUI =
  PreGUI
  {
    fonts = defaultFonts :: [Model]
  , icons = defaultIcons :: [Model]
  }

project :: Int -> Int -> Project
project resx' resy' =
  Project
  {  
    name    = "Test Project"
  , resx    = resx'
  , resy    = resy'
  , camMode = "AbsoluteLocation"
  , models  =
    [ (Model "src/pighead.gltf")
    , (Model "src/grid.gltf")
    ]
  , preobjects = 
    [ PreObject
      {
        pname          = "test"
      , ptype          = "default"
      , pidx           = 0
      , uuid           = nil
      , modelIDXs      = [0,1]
      , presolvers     = []
      , presolverAttrs = []
      , solvers        = ["rotate", "translate"]
      , solverAttrs    = [[0,0,0,0,0,0.01,0,0,0]
                         ,[0.01,0,0]]
      , options        = defaultBackendOptions
      }
    ]
  , background = []
  , gui        = defaultPreGUI
  , cameras    = [ defaultPCam ]
  }
