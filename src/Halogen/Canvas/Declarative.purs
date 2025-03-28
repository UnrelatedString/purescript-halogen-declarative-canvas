module Halogen.Canvas.Declarative
 ( canvas
 , CanvasInput
 , Action
 ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HTML
import Halogen.HTML.Events as Event
import Graphics.Canvas
 ( CanvasElement
 , Context2D
 , getContext2D
 , setCanvasDimensions
 )

type CanvasInput =
  { width :: Int
  , height :: Int
  , draw :: Action
  }

type State = CanvasInput

type Action = Context2D -> Effect Unit

-- leaving queries and events as a TODO for now
canvas ::
  forall query output m.
  MonadEffect m =>
  H.Component query CanvasInput output m
canvas = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval evalSpec
  }

initialState :: CanvasInput -> State

render ::
  forall m.
  MonadEffect m =>
  State -> H.ComponentHTML Action () m
render rec = HTML.canvas
  [ Prop.width rec.width
  , Prop.height rec.height
  ]

evalSpec :: H.EvalSpec
evalSpec = H.defaultEval
 { receive = handleInput
 }

handleInput :: CanvasInput -> Maybe Action

-- handleAction ::
--   forall output m.
--   MonadEffect m =>
--   Action ->
--   H.HalogenM State Action () output m Unit
