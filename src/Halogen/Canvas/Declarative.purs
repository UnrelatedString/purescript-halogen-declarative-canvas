module Halogen.Canvas.Declarative
 ( declarativeCanvas
 , CanvasInput
 , Action
 ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.Component (EvalSpec)
import Halogen.HTML as HTML
import Halogen.HTML.Properties as Prop
import Halogen.HTML.Events as Event
import Graphics.Canvas
 ( CanvasElement
 , Context2D
 , Dimensions
 , getContext2D
 , getCanvasDimensions
 , clearRect
 )
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.HTMLElement (HTMLElement)

type CanvasInput =
  { width :: Int
  , height :: Int
  , draw :: Action
  }

type State =
  { width :: Int
  , height :: Int
  }

type Action = Context2D -> Effect Unit

refLabel :: H.RefLabel
refLabel = H.RefLabel "declCanvasRefLabel"

-- leaving queries and events as a TODO for now
declarativeCanvas ::
  forall query output m.
  MonadEffect m =>
  H.Component query CanvasInput output m
declarativeCanvas = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval evalSpec
  }

initialState :: CanvasInput -> State
initialState { width, height } = { width, height }

render ::
  forall m.
  MonadEffect m =>
  State -> H.ComponentHTML Action () m
render rec = HTML.canvas
  [ Prop.width rec.width
  , Prop.height rec.height
  , Prop.ref refLabel
  ]

evalSpec ::
  forall output query m.
  MonadEffect m =>
  EvalSpec State query Action () CanvasInput output m
evalSpec = H.defaultEval
  { receive = handleInput
  , handleAction = handleAction
  }

handleInput :: CanvasInput -> Maybe Action
handleInput rec = Just rec.draw

handleAction ::
  forall output m.
  MonadEffect m =>
  Action ->
  H.HalogenM State Action () output m Unit
handleAction action = do
  maybeElem <- H.getHTMLElementRef refLabel
  for_ maybeElem \elem -> liftEffect do
    let c = toCanvasElement elem
    ctx <- getContext2D c
    { width, height } <- getCanvasDimensions c
    clearRect ctx { x: 0.0, y: 0.0, width, height }
    action ctx

toCanvasElement :: HTMLElement -> CanvasElement
toCanvasElement = unsafeCoerce
