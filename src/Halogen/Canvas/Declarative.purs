module Halogen.Canvas.Declarative
 ( canvas
 , CanvasInput
 ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HTML
import Halogen.HTML.Events as Event

type CanvasInput

-- leaving queries and events as a TODO for now
canvas ::
    forall query output m.
    MonadEffect m =>
    H.Component query  output m
