module Snake.CanvasView exposing (view)

import Snake exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


view : Model -> Html Msg
view model = let
               unit = 28
               width = unit * Snake.columnCount
               height = unit * Snake.rowCount
               background = Collage.filled Color.grey (Collage.rect width height)
               snake = Collage.move (-4 * unit, 3 * unit) <| Collage.filled Color.blue (Collage.rect (10 * unit) unit)
               food = Collage.move (4 * unit, -3 * unit) <| Collage.filled Color.red (Collage.square unit)
               forms = [ background
                       , snake
                       , food
                       ]

             in
               Element.toHtml <| collage width height forms
