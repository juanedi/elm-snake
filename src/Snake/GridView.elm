module Snake.GridView exposing (view)

import Snake exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

-- VIEW

cellWidth  = 28
cellHeight = 28

cellClass : Model -> Int -> Int -> String
cellClass model x y = if model.lost
                      then "cell-background"
                      else
                        if List.member(x,y) model.snake
                          then "cell-snake"
                          else if (x,y) == model.foodPosition
                            then "cell-food"
                            else "cell-background"

px : Int -> String
px n = (toString n) ++ "px"

cell : Model -> Int -> Int -> Html a
cell model rowIndex colIndex = let c = cellClass model colIndex rowIndex
                               in
                                  div [ classList [ ("cell", True)
                                                  , (c, True)
                                                  ]
                                      , style [ ("height", px cellHeight)
                                              , ("width", px cellWidth)
                                              ]
                                      ]
                                      []

buildRow : Model -> Int -> Html a
buildRow model rowIndex = let
                            buildCell = (\colIndex -> cell model rowIndex colIndex)
                            cells = List.map buildCell [0..Snake.columnCount-1]
                          in
                            div [ class "row" ] cells


view : Model -> Html Msg
view model = let gridWidth = Snake.columnCount * cellHeight
             in
               div [ class "grid container"
                   , style [("width", px gridWidth)]
                   ]
                   (List.map (\rowIndex -> buildRow model rowIndex) [0..Snake.rowCount-1])
