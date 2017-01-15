module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Snake exposing (..)
import Snake.CanvasView exposing (view)
import Snake.GridView exposing (view)
import Time exposing (Time, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


tickTime : Float
tickTime =
    -- TODO: Change the subscription based on the speed chosen by the user instead
    -- of having this small tick time and ignoring most of the events.
    Time.inMilliseconds 10



-- MODEL


type alias Model =
    { snakeModel : Snake.Model
    , viewMode : ViewMode
    , score : Int
    , record : Int
    , speed : Speed
    , debug : Bool
    , time : Int
    }


type Speed
    = Slow
    | Fast
    | Mindblowing


type ViewMode
    = Grid
    | Canvas


type Msg
    = Clock Time
    | SetSpeed Speed
    | SetViewMode ViewMode
    | SetDebug Bool
    | SnakeMsg Snake.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( sModel, cmd ) =
            Snake.init
    in
        ( { snakeModel = sModel
          , viewMode = Canvas
          , score = 0
          , record = 0
          , speed = Fast
          , debug = False
          , time = 0
          }
        , Cmd.map SnakeMsg cmd
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every tickTime Clock
        , Sub.map SnakeMsg (Snake.subscriptions model.snakeModel)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clock _ ->
            let
                updatedModel =
                    { model | time = model.time + 1 }
            in
                if shouldAdvance updatedModel then
                    let
                        ( sModel, cmd ) =
                            Snake.update Tick model.snakeModel
                    in
                        ( { model | snakeModel = sModel, time = model.time + 1 }, Cmd.map SnakeMsg cmd )
                else
                    ( updatedModel, Cmd.none )

        SetSpeed s ->
            ( { model | speed = s }, Cmd.none )

        SetViewMode m ->
            ( { model | viewMode = m }, Cmd.none )

        SetDebug v ->
            ( { model | debug = v }, Cmd.none )

        SnakeMsg (Event Bite) ->
            ( { model | score = model.score + 1 }, Cmd.none )

        SnakeMsg (Event Died) ->
            let
                record =
                    Basics.max model.score model.record

                ( sModel, cmd ) =
                    Snake.init
            in
                ( { model | snakeModel = sModel, score = 0, record = record }, Cmd.map SnakeMsg cmd )

        SnakeMsg m ->
            let
                ( sModel, cmd ) =
                    Snake.update m model.snakeModel
            in
                ( { model | snakeModel = sModel }, Cmd.map SnakeMsg cmd )


shouldAdvance : Model -> Bool
shouldAdvance model =
    let
        tickTime =
            case model.speed of
                Slow ->
                    20

                Fast ->
                    5

                Mindblowing ->
                    3
    in
        (model.time % tickTime) == 0



-- VIEW


view : Model -> Html Msg
view model =
    let
        inspector =
            modelInspector model

        baseView =
            [ gameView model
            , inspector
            ]
    in
        div [] [ rowLayout baseView ]


settings : Model -> Html Msg
settings model =
    Html.form [ class "settings" ]
        [ viewModeSelector model
        , speedSelector model
        , debugToggle model
        ]


viewModeOption : Model -> ViewMode -> Html Msg
viewModeOption model viewMode =
    input
        [ type_ "button"
        , onClick (SetViewMode viewMode)
        , value (toString viewMode)
        , classList
            [ ( "btn", True )
            , ( "btn-default", True )
            , ( "active", model.viewMode == viewMode )
            ]
        ]
        []


viewModeSelector : Model -> Html Msg
viewModeSelector model =
    div [ class "form-group" ]
        [ label [ for "view-mode" ] [ text "View mode" ]
        , div [ class "btn-group" ]
            [ viewModeOption model Canvas
            , viewModeOption model Grid
            ]
        ]


speedOption : Model -> Speed -> Html Msg
speedOption model speed =
    input
        [ type_ "button"
        , onClick (SetSpeed speed)
        , value (toString speed)
        , classList
            [ ( "btn", True )
            , ( "btn-default", True )
            , ( "active", model.speed == speed )
            ]
        ]
        []


speedSelector : Model -> Html Msg
speedSelector model =
    div [ class "form-group" ]
        [ label [ for "speed" ] [ text "Speed" ]
        , div [ class "btn-group" ]
            [ speedOption model Slow
            , speedOption model Fast
            , speedOption model Mindblowing
            ]
        ]


debugOption : Model -> Bool -> Html Msg
debugOption model val =
    input
        [ type_ "button"
        , onClick (SetDebug val)
        , value
            (if val then
                "On"
             else
                "Off"
            )
        , classList
            [ ( "btn", True )
            , ( "btn-default", True )
            , ( "active", model.debug == val )
            ]
        ]
        []


debugToggle : Model -> Html Msg
debugToggle model =
    div [ class "form-group" ]
        [ label [ for "debug" ] [ text "Debug" ]
        , div [ class "btn-group" ]
            [ debugOption model True
            , debugOption model False
            ]
        ]


modelInspector : Model -> Html Msg
modelInspector model =
    div [ classList [ ( "hidden", not model.debug ) ], style [ ( "margin-top", "40px" ) ] ]
        [ pre [] [ text (toString model) ] ]


points : Model -> Html Msg
points model =
    h1 [ class "status-part" ] [ text (toString model.score) ]


record : Model -> Html Msg
record model =
    h1 [ class "status-part" ] [ text (toString model.record) ]


statusPart : String -> Html Msg -> Html Msg
statusPart title content =
    div []
        [ h4 [] [ text title ]
        , content
        ]


gameView : Model -> Html Msg
gameView model =
    let
        display =
            case model.viewMode of
                Grid ->
                    Snake.GridView.view

                Canvas ->
                    Snake.CanvasView.view

        snakeView =
            Html.map SnakeMsg (display model.snakeModel)

        statusview =
            div []
                [ statusPart "Score" (points model)
                , statusPart "Record" (record model)
                , statusPart "Settings" (settings model)
                ]
    in
        div [ class "row" ]
            [ div [ class "col-md-8" ] [ snakeView ]
            , div [ class "col-md-4" ] [ statusview ]
            ]


row : Html Msg -> Html Msg
row component =
    div [ class "row" ]
        [ component ]


rowLayout : List (Html Msg) -> Html Msg
rowLayout components =
    div [ class "container" ]
        (List.map row components)
