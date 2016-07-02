import Html exposing (..)
import Html.Attributes exposing(..)
import Html.App as Html
import Keyboard
import Time exposing (Time, second)
import Json.Encode exposing(..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

debug = True

rowCount    = 20
columnCount = 30

-- MODEL


type alias Cell = (Int, Int)
type alias Snake = List(Cell)

type alias Model =
  { time : Time
  , foodPosition : Cell
  , snake : Snake
  , direction : Direction
  }

init : (Model, Cmd Msg)
init =
  (Model 0 (12,17) [(10, 3), (10, 4), (10, 5), (11, 5), (12, 5), (12, 6), (12, 7), (12, 8), (12, 9)] Right, Cmd.none)



-- UPDATE

type Direction
  = Up
  | Down
  | Left
  | Right

type Msg
  = Tick Time
  | KeyUp Keyboard.KeyCode


changeDirection : Keyboard.KeyCode -> Direction -> Direction
changeDirection keyCode direction = case keyCode of
                                      38 -> Up
                                      40 -> Down
                                      37 -> Left
                                      39 -> Right
                                      _  -> direction

moveSnake : Model -> Model
moveSnake model = model

updateFoodPosition : Model -> Model
updateFoodPosition model = model

tick : Model -> Model
tick model = Model (model.time + 1) model.foodPosition model.snake model.direction


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ((moveSnake >> updateFoodPosition >> tick) model, Cmd.none)

    KeyUp keyCode ->
      (Model model.time model.foodPosition model.snake (changeDirection keyCode model.direction), Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Time.every second Tick,
    Keyboard.ups KeyUp
  ]



-- VIEW

cellWidth  = 28
cellHeight = 28

cellClass : Model -> Int -> Int -> String
cellClass model x y = if (x,y) == model.foodPosition
                        then "cell-food"
                        else if List.member (x,y) model.snake
                                then "cell-snake"
                                else "cell-background"

px : Int -> String
px n = (toString n) ++ "px"

cell : Model -> Int -> Int -> Html a
cell model rowIndex colIndex = let c = cellClass model rowIndex colIndex
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
                            cells = List.map buildCell [1..columnCount]
                          in
                            div [ classList [("row", True)] ] cells


gameGrid : Model -> Html Msg
gameGrid model = let gridWidth = columnCount * cellHeight
                 in
                   div [ classList [("grid", True), ("container", True)]
                       , style [("width", px gridWidth)]
                       ]
                       (List.map (\rowIndex -> buildRow model rowIndex) [1..rowCount])


modelInspector : Model -> Html Msg
modelInspector model = let attributes = [ ("direction", string (toString model.direction))
                                        , ("time", float model.time)
                                        ]
                       in div [ style [("margin-top", "40px")] ]
                              [ pre [] [text (Json.Encode.encode 2 (Json.Encode.object attributes))] ]

view : Model -> Html Msg
view model = if debug
                then div []
                         [ gameGrid model
                         , modelInspector model
                         ]
                else gameGrid model
