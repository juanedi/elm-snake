module Snake exposing (Model
                      , Msg(Advance, Event)
                      , EventKind(..)
                      , init
                      , update
                      , subscriptions

                      -- for view modules
                      , columnCount
                      , rowCount
                      , cellSize
                      )

import Html exposing (..)
import Html.Attributes exposing(..)
import Keyboard
import Random
import Task

rowCount    = 22
columnCount = 22
cellSize    = 28

-- MODEL


type alias Cell = (Int, Int)
type alias Snake = List(Cell)

type alias Model =
  { foodPosition : Cell
  , snake : Snake
  , direction : Direction
  , directionChange : Maybe Direction
  , lost : Bool
  }

init : (Model, Cmd Msg)
init =
    ({ foodPosition = (12,17)
     , snake = [(1,2), (1,3), (1,4), (1,5)]
     , direction = Right
     , directionChange = Nothing
     , lost = False
     }
    , Cmd.none
    )


head : Snake -> Cell
head snake = Maybe.withDefault (0,0) (List.head snake)


-- UPDATE

type Direction
  = Up
  | Down
  | Left
  | Right

type Msg
  = Advance
  | KeyUp Keyboard.KeyCode
  | FoodAppeared Cell
  | Event EventKind

type EventKind
  = Died
  | Bite


keyToDirection : Keyboard.KeyCode -> Direction -> Direction
keyToDirection keyCode currentDirection = case keyCode of
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

updateDirection : Model -> Model
updateDirection model = case model.directionChange of
                          Just d ->
                            { model | direction = d, directionChange = Nothing }
                          Nothing ->
                            model

moveSnake : Cell -> Model -> Model
moveSnake next model = let sn1  = next :: model.snake
                           sn2  = if next /= model.foodPosition then List.take ((List.length sn1)-1) sn1 else sn1
                       in { model | snake = sn2 }

moveFood : Cmd Msg
moveFood = let generator = (Random.pair (Random.int 0 (columnCount-1)) (Random.int 0 (rowCount-1)))
           in Random.generate FoodAppeared generator

raiseEvent : EventKind -> Cmd Msg
raiseEvent kind = Task.perform Event Event (Task.succeed kind)

lost : Cell -> Model -> Bool
lost (x,y) model = List.member (x,y) model.snake
                     || x < 0
                     || columnCount <= x
                     || y < 0
                     || rowCount <= y

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if model.lost
  then
    (model, Cmd.none)
  else
    case msg of
      Advance ->
        let next = nextPosition model.snake model.direction
        in if lost next model
              then
                ({model | lost = True}, raiseEvent Died)
              else
                let
                  bite = next == model.foodPosition
                  cmd  = if bite then Cmd.batch [(raiseEvent Bite), moveFood] else Cmd.none
                  upd  = updateDirection >> moveSnake next
                in
                  (upd model, cmd)

      KeyUp keyCode ->
        case model.directionChange of
          Just _ ->
            (model, Cmd.none)
          Nothing ->
            let
              directionChange = keyToDirection keyCode model.direction
            in
              ({model | direction = directionChange, directionChange = Just directionChange}, Cmd.none)

      FoodAppeared (x,y) ->
        let (x', y') = model.foodPosition
        in if x == x' || y == y'
              then (model, moveFood)
              else ({model | foodPosition = (x,y)}, Cmd.none)

      Event _ ->
        -- This kind of messages are triggered by this module just to allow parent controls
        -- interested in events of this game to be notified without exposing our model.
        (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.ups KeyUp

