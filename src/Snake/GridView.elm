module Snake.GridView exposing (view)

import Snake exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (..)
import String exposing (concat)


-- STYLES

px : Int -> String
px n = (toString n) ++ "px"

gridStyle : List((String, String))
gridStyle = let gridWidth = Snake.columnCount * Snake.cellSize
            in [ ("width", px gridWidth)
               , ("padding", "0")
               ]  

rowStyle : List((String, String))
rowStyle = [ ("margin", "0")
           , ("padding", "0")
           ]

cellStyle : List((String, String)) 
cellStyle = [ ("float", "left")
            , ("position", "relative")
            , ("height", px Snake.cellSize)
            , ("width", px Snake.cellSize)
            ]

rgb : Color -> String
rgb color = let
              {red, green, blue} = Color.toRgb color
            in
              String.concat [ "rgb(" , toString red , "," , toString green , "," , toString blue, ")" ]

-- VIEW

cellColor : Model -> Int -> Int -> Color
cellColor model x y = if model.lost
                      then Color.grey
                      else
                        if List.member(x,y) model.snake
                          then Color.blue
                          else if (x,y) == model.foodPosition
                            then Color.red
                            else Color.grey

cell : Model -> Int -> Int -> Html a
cell model rowIndex colIndex = let
                                 cellRgb = rgb (cellColor model colIndex rowIndex)
                                 styles = ("background-color", cellRgb) :: cellStyle
                               in
                                 div [ style styles ] []

buildRow : Model -> Int -> Html a
buildRow model rowIndex = let
                            buildCell = (\colIndex -> cell model rowIndex colIndex)
                            cells = List.map buildCell [0..Snake.columnCount-1]
                          in
                            div [ class "row", style rowStyle ] cells


view : Model -> Html Msg
view model = div [ class "grid container", style gridStyle ]
                 (List.map (\rowIndex -> buildRow model rowIndex) [0..Snake.rowCount-1])
