import Snake exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Time exposing (Time, second)

tickTime = Time.inMilliseconds 50

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { snakeModel : Snake.Model
  , speed : Speed
  , debug : Bool
  , time : Int
  }

type Speed
  = Slow
  | Fast
  | Mindblowing

init : (Model, Cmd Msg)
init =
  let (sModel, cmd) = Snake.init
  in
     (Model sModel Fast False 0, Cmd.map SnakeMsg cmd)

-- UPDATE

type Msg
  = Clock Time
  | SetSpeed Speed
  | SetDebug Bool
  | SnakeMsg Snake.Msg

shouldTick: Model -> Bool
shouldTick model = let tickTime = case model.speed of
                                    Slow        -> 3
                                    Fast        -> 2
                                    Mindblowing -> 1
                   in
                      (model.time % tickTime) == 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
                     Clock _ ->
                       let model' = {model | time = model.time + 1}
                       in
                         if shouldTick model'
                           then
                             let (sModel, cmd) = Snake.update Snake.Tick model.snakeModel
                             in 
                                (Model sModel model.speed model.debug (model.time + 1), Cmd.map SnakeMsg cmd)
                           else
                             (model', Cmd.none)

                     SetSpeed s ->
                       ({model | speed = s}, Cmd.none)

                     SetDebug v ->
                       ({model | debug = v}, Cmd.none)

                     SnakeMsg m ->
                       let (sModel, cmd) = Snake.update m model.snakeModel
                       in
                          (Model sModel model.speed model.debug model.time, Cmd.map SnakeMsg cmd)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every tickTime Clock
            , Sub.map SnakeMsg (Snake.subscriptions model.snakeModel)
            ]

-- VIEW

settings : Model -> Html Msg
settings model = Html.form [ classList [("settings", True), ("form-inline", True)] ]
                           [ speedSelector model
                           , debugToggle model
                           ]

speedOption : Model -> Speed -> Html Msg
speedOption model speed = input [ type' "button"
                                 , onClick (SetSpeed speed)
                                 , value (toString speed)
                                 , classList [ ("btn", True)
                                             , ("btn-default", True)
                                             , ("active", model.speed == speed)]
                                 ]
                                 [ ]

speedSelector : Model -> Html Msg
speedSelector model = div [ classList [("form-group", True)] ]
                          [ label [ for "speed" ] [ text "Speed" ]
                          , div [ classList [("btn-group", True)] ]
                                [ speedOption model Slow
                                , speedOption model Fast
                                , speedOption model Mindblowing] 
                          ]


debugOption : Model -> Bool -> Html Msg
debugOption model val = input [ type' "button"
                                 , onClick (SetDebug val)
                                 , value (if val then "On" else "Off")
                                 , classList [ ("btn", True)
                                             , ("btn-default", True)
                                             , ("active", model.debug == val)
                                             ]
                                 ]
                                 [ ]

debugToggle : Model -> Html Msg
debugToggle model = div [ classList [("form-group", True)] ]
                          [ label [ for "debug" ] [ text "Debug" ]
                          , div [ classList [("btn-group", True)] ]
                                [ debugOption model True
                                , debugOption model False
                                ] 
                          ]

modelInspector : Model -> Html Msg
modelInspector model = div [ classList [("hidden", not model.debug)], style [("margin-top", "40px")] ]
                           [ pre [] [text (toString model)] ]

row : Html Msg -> Html Msg
row component = div [ classList [("row", True)] ]
                    [ component ]

rowLayout : List(Html Msg) -> Html Msg
rowLayout components = div [ classList [("container", True)] ]
                           (List.map row components)

view : Model -> Html Msg
view model = let
               snakeView = App.map SnakeMsg (Snake.view model.snakeModel)
               inspector = modelInspector model
               baseView  = [ snakeView
                           , settings model
                           , inspector
                           ]
             in
                div [] [rowLayout baseView]

