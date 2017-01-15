module Snake
    exposing
        ( Model
        , Msg(Tick, Event)
        , EventKind(..)
        , init
        , update
        , subscriptions
          -- for view modules
        , columnCount
        , rowCount
        , cellSize
        )

import Dict
import Keyboard
import Random
import Task


type alias Cell =
    ( Int, Int )


type alias Snake =
    List (Cell)


type alias Model =
    { foodPosition : Cell
    , snake : Snake
    , direction : Direction
    , directionChange : Maybe Direction
    , lost : Bool
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type Msg
    = Tick
    | KeyDown Keyboard.KeyCode
    | FoodAppeared Cell
      -- These "Event _" messages are used to notify the host module.
      -- This way we only deal here with the game rules and don't care about
      -- keeping score, restarting the game, etc.
    | Event EventKind


type EventKind
    = Died
    | Bite


init : ( Model, Cmd Msg )
init =
    ( { foodPosition = ( 12, 17 )
      , snake = [ ( 1, 2 ), ( 1, 3 ), ( 1, 4 ), ( 1, 5 ) ]
      , direction = Right
      , directionChange = Nothing
      , lost = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.lost then
        ( model, Cmd.none )
    else
        case msg of
            Tick ->
                let
                    next =
                        nextPosition model.snake model.direction
                in
                    if isLoosingPosition next model then
                        -- Currently, this module is only in charge of letting
                        -- the user play a single life. When the snake dies, we
                        -- notify our parent module which will be in charge of
                        -- counting the score and restarting the game.
                        ( { model | lost = True }, raiseEvent Died )
                    else
                        let
                            updatedModel =
                                model
                                    |> updateDirection
                                    |> moveSnake next

                            cmd =
                                if next == model.foodPosition then
                                    Cmd.batch [ (raiseEvent Bite), moveFood ]
                                else
                                    Cmd.none
                        in
                            ( updatedModel, cmd )

            -- When a key is pressed, the direction change is stored in the
            -- model and won't take effect until the next tick of the clock.
            KeyDown keyCode ->
                case model.directionChange of
                    Just _ ->
                        ( model, Cmd.none )

                    Nothing ->
                        let
                            directionChange =
                                keyToDirection keyCode model.direction
                        in
                            ( { model
                                | direction = directionChange
                                , directionChange = Just directionChange
                              }
                            , Cmd.none
                            )

            FoodAppeared ( x, y ) ->
                let
                    ( x2, y2 ) =
                        model.foodPosition
                in
                    if x == x2 || y == y2 then
                        ( model, moveFood )
                    else
                        ( { model | foodPosition = ( x, y ) }, Cmd.none )

            Event _ ->
                -- This kind of messages are triggered by this module just to
                -- allow parent controls interested in events of this game to be
                -- notified without exposing our model.
                ( model, Cmd.none )


rowCount =
    22


columnCount =
    22


cellSize =
    28


head : Snake -> Cell
head snake =
    -- The snake is never an empty list, so the default (0,0) is there just
    -- because the type system cannot know about that invariant.
    Maybe.withDefault ( 0, 0 ) (List.head snake)


keyToDirection : Keyboard.KeyCode -> Direction -> Direction
keyToDirection keyCode currentDirection =
    case Dict.get keyCode keyMappings of
        Just direction ->
            if direction == reverse currentDirection then
                currentDirection
            else
                direction

        Nothing ->
            currentDirection


keyMappings : Dict.Dict Keyboard.KeyCode Direction
keyMappings =
    Dict.fromList
        [ ( 38, Up )
        , ( 40, Down )
        , ( 37, Left )
        , ( 39, Right )
        ]


reverse : Direction -> Direction
reverse direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Right ->
            Left

        Left ->
            Right


nextPosition : Snake -> Direction -> Cell
nextPosition snake direction =
    let
        ( x, y ) =
            Maybe.withDefault ( 0, 0 ) (List.head snake)
    in
        case direction of
            Up ->
                ( x, y - 1 )

            Down ->
                ( x, y + 1 )

            Left ->
                ( x - 1, y )

            Right ->
                ( x + 1, y )


updateDirection : Model -> Model
updateDirection model =
    case model.directionChange of
        Just d ->
            { model | direction = d, directionChange = Nothing }

        Nothing ->
            model


moveSnake : Cell -> Model -> Model
moveSnake next model =
    let
        sn1 =
            next :: model.snake

        sn2 =
            if next /= model.foodPosition then
                List.take ((List.length sn1) - 1) sn1
            else
                sn1
    in
        { model | snake = sn2 }


moveFood : Cmd Msg
moveFood =
    let
        columnGenerator =
            Random.int 0 (columnCount - 1)

        rowGenerator =
            Random.int 0 (rowCount - 1)
    in
        Random.generate FoodAppeared <|
            Random.pair columnGenerator rowGenerator


raiseEvent : EventKind -> Cmd Msg
raiseEvent kind =
    Task.perform Event (Task.succeed kind)


isLoosingPosition : Cell -> Model -> Bool
isLoosingPosition ( x, y ) model =
    List.member ( x, y ) model.snake
        || (x < 0)
        || (columnCount <= x)
        || (y < 0)
        || (rowCount <= y)
