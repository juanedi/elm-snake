module Snake exposing (Model, Msg(Tick), init, update, view, subscriptions, debugAttributes)

import Html exposing (..)
import Html.Attributes exposing(..)
import Keyboard
import Random
import Json.Encode exposing(..)

rowCount    = 20
columnCount = 20

-- MODEL


type alias Cell = (Int, Int)
type alias Snake = List(Cell)

type alias Model =
  { foodPosition : Cell
  , snake : Snake
  , direction : Direction
  }

init : (Model, Cmd Msg)
init =
  (Model (12,17) [(1,2),(1,3)] Right, Cmd.none)


head : Snake -> Cell
head snake = Maybe.withDefault (0,0) (List.head snake)

debugAttributes model = let (hX, hY) = head model.snake
                        in Json.Encode.object [ ("direction", string (toString model.direction))
                                              , ("head", Json.Encode.list [int hX, int hY])
                                              ]

-- UPDATE

type Direction
  = Up
  | Down
  | Left
  | Right

type Msg
  = Tick
  | KeyUp Keyboard.KeyCode
  | FoodAppeared Cell


changeDirection : Keyboard.KeyCode -> Direction -> Direction
changeDirection keyCode currentDirection = case keyCode of
                                             38 -> if currentDirection /= Down then Up else currentDirection
                                             40 -> if currentDirection /= Up then Down else currentDirection
                                             37 -> if currentDirection /= Right then Left else currentDirection
                                             39 -> if currentDirection /= Left then Right else currentDirection
                                             _  -> currentDirection

nextPosition : Snake -> Direction -> Cell
nextPosition snake direction = let (x,y) = Maybe.withDefault (0,0) (List.head snake)
                               in case direction of
                                    Up    -> (x, y-1)
                                    Down  -> (x, y+1)
                                    Left  -> (x-1, y)
                                    Right -> (x+1, y)

moveSnake : Cell -> Model -> Model
moveSnake next model = let sn1  = next :: model.snake
                           sn2  = if next /= model.foodPosition then List.take ((List.length sn1)-1) sn1 else sn1
                       in { model | snake = sn2 }

moveFood : Cmd Msg
moveFood = let generator = (Random.pair (Random.int 0 (columnCount-1)) (Random.int 0 (rowCount-1)))
           in Random.generate FoodAppeared generator

lost : Cell -> Model -> Bool
lost (x,y) model = List.member (x,y) model.snake
                     || x < 0
                     || columnCount <= x
                     || y < 0
                     || rowCount <= y

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      let next = nextPosition model.snake model.direction
      in if lost next model
            then init
            else let
                   model' = moveSnake next model
                   cmd    = if next /= model.foodPosition then Cmd.none else moveFood
                 in
                    (model', cmd)

    KeyUp keyCode ->
      ({model | direction = changeDirection keyCode model.direction}, Cmd.none)

    FoodAppeared (x,y) ->
      let (x', y') = model.foodPosition
      in if x == x' || y == y'
            then (model, moveFood)
            else ({model | foodPosition = (x,y)}, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.ups KeyUp


-- VIEW

cellWidth  = 28
cellHeight = 28

cellClass : Model -> Int -> Int -> String
cellClass model x y = if List.member(x,y) model.snake
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
                            cells = List.map buildCell [0..columnCount-1]
                          in
                            div [ classList [("row", True)] ] cells


view : Model -> Html Msg
view model = let gridWidth = columnCount * cellHeight
             in
               div [ classList [("grid", True), ("container", True)]
                   , style [("width", px gridWidth)]
                   ]
                   (List.map (\rowIndex -> buildRow model rowIndex) [0..rowCount-1])
