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

debug       = False
tickTime    = Time.inMilliseconds 150
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
  (Model 0 (12,17) [(1,2),(1,3)] Right, Cmd.none)



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
changeDirection keyCode currentDirection = case keyCode of
                                             38 -> if currentDirection /= Down then Up else currentDirection
                                             40 -> if currentDirection /= Up then Down else currentDirection
                                             37 -> if currentDirection /= Right then Left else currentDirection
                                             39 -> if currentDirection /= Left then Right else currentDirection
                                             _  -> currentDirection

nextPosition : Snake -> Direction -> Cell
nextPosition snake direction = let (x,y) = Maybe.withDefault (0,0) (List.head snake)
                               in case direction of
                                    Up    -> (x, (y-1) % rowCount)
                                    Down  -> (x, (y+1) % rowCount)
                                    Left  -> ((x-1) % columnCount, y)
                                    Right -> ((x+1) % columnCount, y)

moveSnake : Model -> Model
moveSnake model = let next = nextPosition model.snake model.direction
                      sn1  = next :: model.snake
                      sn2  = if next /= model.foodPosition then List.take ((List.length sn1)-1) sn1 else sn1
                  in { model | snake = sn2 }

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
    Time.every tickTime Tick,
    Keyboard.ups KeyUp
  ]



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


gameGrid : Model -> Html Msg
gameGrid model = let gridWidth = columnCount * cellHeight
                 in
                   div [ classList [("grid", True), ("container", True)]
                       , style [("width", px gridWidth)]
                       ]
                       (List.map (\rowIndex -> buildRow model rowIndex) [0..rowCount-1])


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
