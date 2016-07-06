module Snake.GridView exposing (view)

import Snake exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (..)

-- VIEW

cellWidth  = 28
cellHeight = 28

cellRgb : Model -> Int -> Int -> String
cellRgb model x y = let
                      rgb = Color.toRgb <| cellColor model x y
                    in
                      "rgb(" ++ toString rgb.red ++ "," ++ toString rgb.green ++ "," ++ toString rgb.blue ++ ")"

cellColor : Model -> Int -> Int -> Color
cellColor model x y = if model.lost
                      then Color.grey
                      else
                        if List.member(x,y) model.snake
                          then Color.blue
                          else if (x,y) == model.foodPosition
                            then Color.red
                            else Color.grey

px : Int -> String
px n = (toString n) ++ "px"

cell : Model -> Int -> Int -> Html a
cell model rowIndex colIndex = let c = 1
                               in
                                  div [ style [ ("float", "left")
                                              , ("position", "relative")
                                              , ("height", px cellHeight)
                                              , ("width", px cellWidth)
                                              , ("background-color", cellRgb model colIndex rowIndex)
                                              ]
                                      ]
                                      []

buildRow : Model -> Int -> Html a
buildRow model rowIndex = let
                            buildCell = (\colIndex -> cell model rowIndex colIndex)
                            cells = List.map buildCell [0..Snake.columnCount-1]
                          in
                            div [ class "row"
                                , style [ ("margin", "0")
                                        , ("padding", "0")
                                        ]
                                ] cells


view : Model -> Html Msg
view model = let gridWidth = Snake.columnCount * cellHeight
             in
               div [ class "grid container"
                   , style [ ("width", px gridWidth)
                           , ("padding", "0")
                           ]
                   ]
                   (List.map (\rowIndex -> buildRow model rowIndex) [0..Snake.rowCount-1])
