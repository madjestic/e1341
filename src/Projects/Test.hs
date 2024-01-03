module Projects.Test where

import Data.UUID
import Linear.V3

import Graphics.RedViz.Solvable
import Graphics.RedViz.Backend
import Graphics.RedViz.Camera
import Graphics.RedViz.Project
import Graphics.RedViz.Object

testPreObject :: PreObject
testPreObject = 
    PreObject
    {
      pname          = "pig_object"
    , ptype          = Default
    , pidx           = 0
    , puuid          = nil
    , modelIDXs      = [0]
    , tsolvers       =
      [ Identity
      -- , Rotate
      --   { space = ObjectSpace
      --   , cxyz  = V3 0 0 0
      --   , rord  = XYZ
      --   , rxyz  = V3 0 0 (0.5)
      --   , avel  = V3 0 0 0.05 }
      , Translate
        { space   = WorldSpace
        , txyz    = V3 1.5 0 0
        , tvel    = V3 0.0 0 0
        , kinslv = Identity }
        -- , Rotate
        -- { space   = ObjectSpace
        -- , cxyz    = V3 0 0 0
        -- , rord    = XYZ
        -- , rxyz    = V3 0 0 (0.5)
        -- , avel    = V3 0 0 (0.1)
        -- , kinslv  = Identity
        --   -- Speed
        --   -- { life = 1.0
        --   -- , age  = 0.0
        --   -- , inc  = 0.01
        --   -- , amp  = 1.0
        --   -- , func = id }
        -- }
      -- , Translate
      --  { space = WorldSpace
      --  , txyz  = V3 1.1 0 0
      --  , tvel  = V3 0.0 0 0 }
      ]
    , osolvers    =
      [ Identity
      , Select
      ]
      , options   = defaultBackendOptions
      , pparent   = nil
      , pchildren =
        [ PreObject
          {
            pname          = "grid_object"
          , ptype          = Default
          , pidx           = 0
          , puuid          = nil
          , modelIDXs      = [1]
          , tsolvers       =
            [ Identity
            -- , Rotate
            --   { space = ObjectSpace
            --   , cxyz  = V3 0 0 0
            --   , rord  = XYZ
            --   , rxyz  = V3 0 0 (0.5)
            --   , avel  = V3 0 0 0.05 }
            , Translate
              { space   = WorldSpace
              , txyz    = V3 1.5 0 0
              , tvel    = V3 0.0 0 0
              , kinslv = Identity }
              , Rotate
              { space   = ObjectSpace
              , cxyz    = V3 0 0 0
              , rord    = XYZ
              , rxyz    = V3 0 0 (0.5)
              , avel    = V3 0 0 (0.02)
              , kinslv  = Identity
                -- Speed
                -- { life = 1.0
                -- , age  = 0.0
                -- , inc  = 0.01
                -- , amp  = 1.0
                -- , func = id }
              }
            -- , Translate
            --  { space = WorldSpace
            --  , txyz  = V3 1.1 0 0
            --  , tvel  = V3 0.0 0 0 }
              , Parent
            ]
          , osolvers  =
            [ Identity
            , Select
            ]
            , options   = defaultBackendOptions
            , pparent   = nil
            , pchildren = []
          }            
        ]
    }      

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
  , preObjects = 
    [ PreObject
      {
        pname          = "test_object"
      , ptype          = Default
      , pidx           = 0
      , puuid          = nil
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
          { space   = WorldSpace
          , txyz    = V3 1.5 0 0
          , tvel    = V3 0.0 0 0
          , kinslv  = Identity }
          , Rotate
          { space   = ObjectSpace
          , cxyz    = V3 0 0 0
          , rord    = XYZ
          , rxyz    = V3 0 0 (0.5)
          , avel    = V3 0 0 (0.1)
          , kinslv =
            Speed
            { life = 1.0
            , age  = 0.0
            , inc  = 0.01
            , amp  = 1.0
            , func = id }
          }
        -- , Translate
        --  { space = WorldSpace
        --  , txyz  = V3 1.1 0 0
        --  , tvel  = V3 0.0 0 0 }
        ]
      , osolvers    =
        [ Identity
        , Select
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

parentTestProject :: Int -> Int -> Project
parentTestProject resx' resy' =
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
  , preObjects = 
    [ PreObject
      {
        pname          = "pig_object"
      , ptype          = Default
      , pidx           = 0
      , puuid          = nil
      , modelIDXs      = [0]
      , tsolvers       =
        [ Identity
        -- , Rotate
        --   { space = ObjectSpace
        --   , cxyz  = V3 0 0 0
        --   , rord  = XYZ
        --   , rxyz  = V3 0 0 (0.5)
        --   , avel  = V3 0 0 0.05 }
        , Translate
          { space   = WorldSpace
          , txyz    = V3 1.5 0 0
          , tvel    = V3 0.0 0 0
          , kinslv = Identity }
          -- , Rotate
          -- { space   = ObjectSpace
          -- , cxyz    = V3 0 0 0
          -- , rord    = XYZ
          -- , rxyz    = V3 0 0 (0.5)
          -- , avel    = V3 0 0 (0.1)
          -- , kinslv  = Identity
          --   -- Speed
          --   -- { life = 1.0
          --   -- , age  = 0.0
          --   -- , inc  = 0.01
          --   -- , amp  = 1.0
          --   -- , func = id }
          -- }
        -- , Translate
        --  { space = WorldSpace
        --  , txyz  = V3 1.1 0 0
        --  , tvel  = V3 0.0 0 0 }
        ]
      , osolvers    =
        [ Identity
        , Select
        ]
        , options   = defaultBackendOptions
        , pparent   = nil
        , pchildren =
          [ PreObject
            {
              pname          = "grid_object"
            , ptype          = Default
            , pidx           = 0
            , puuid          = nil
            , modelIDXs      = [1]
            , tsolvers       =
              [ Identity
              -- , Rotate
              --   { space = ObjectSpace
              --   , cxyz  = V3 0 0 0
              --   , rord  = XYZ
              --   , rxyz  = V3 0 0 (0.5)
              --   , avel  = V3 0 0 0.05 }
              , Translate
                { space   = WorldSpace
                , txyz    = V3 1.5 0 0
                , tvel    = V3 0.0 0 0
                , kinslv = Identity }
                , Rotate
                { space   = ObjectSpace
                , cxyz    = V3 0 0 0
                , rord    = XYZ
                , rxyz    = V3 0 0 (0.5)
                , avel    = V3 0 0 (0.02)
                , kinslv  = Identity
                  -- Speed
                  -- { life = 1.0
                  -- , age  = 0.0
                  -- , inc  = 0.01
                  -- , amp  = 1.0
                  -- , func = id }
                }
              -- , Translate
              --  { space = WorldSpace
              --  , txyz  = V3 1.1 0 0
              --  , tvel  = V3 0.0 0 0 }
                , Parent
              ]
            , osolvers  =
              [ Identity
              , Select
              ]
              , options   = defaultBackendOptions
              , pparent   = nil
              , pchildren = []
            }            
          ]
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
