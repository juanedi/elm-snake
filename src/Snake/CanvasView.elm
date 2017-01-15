module Snake.CanvasView exposing (view)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (..)
import Snake exposing (..)


background : Form
background =
    let
        width =
            cellSize * Snake.columnCount

        height =
            cellSize * Snake.rowCount
    in
        Collage.filled Color.grey (Collage.rect width height)


cell : Color -> Int -> Int -> Form
cell color x y =
    let
        dx =
            -Snake.columnCount / 2 + 0.5 + (toFloat x)

        dy =
            Snake.rowCount / 2 - 0.5 - (toFloat y)

        square =
            Collage.square cellSize
    in
        Collage.move ( cellSize * dx, cellSize * dy ) <| Collage.filled color square


snake : Model -> Form
snake model =
    Collage.group (List.map (\( x, y ) -> cell Color.blue x y) model.snake)


food : Model -> Form
food model =
    let
        ( x, y ) =
            model.foodPosition
    in
        cell Color.red x y


view : Model -> Html Msg
view model =
    let
        width =
            cellSize * Snake.columnCount

        height =
            cellSize * Snake.rowCount

        background =
            Collage.filled Color.grey (Collage.rect width height)
    in
        Element.toHtml <|
            collage width
                height
                [ background
                , food model
                , snake model
                ]
