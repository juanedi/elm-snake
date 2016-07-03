import Snake exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import Time exposing (Time, second)

tickTime = Time.inMilliseconds 10

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
     ( { snakeModel = sModel
       , speed = Fast
       , debug = False
       , time = 0
       }
     , Cmd.map SnakeMsg cmd)

-- UPDATE

type Msg
  = Clock Time
  | SetSpeed Speed
  | SetDebug Bool
  | SnakeMsg Snake.Msg

shouldAdvance: Model -> Bool
shouldAdvance model = let tickTime = case model.speed of
                                       Slow        -> 10
                                       Fast        -> 5
                                       Mindblowing -> 3
                      in
                         (model.time % tickTime) == 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
                     Clock _ ->
                       let model' = {model | time = model.time + 1}
                       in
                         if shouldAdvance model'
                           then
                             let (sModel, cmd) = Snake.update Advance model.snakeModel
                             in 
                                ({model | snakeModel = sModel, time = model.time+1}, Cmd.map SnakeMsg cmd)
                           else
                             (model', Cmd.none)

                     SetSpeed s ->
                       ({model | speed = s}, Cmd.none)

                     SetDebug v ->
                       ({model | debug = v}, Cmd.none)

                     SnakeMsg m ->
                       let (sModel, cmd) = Snake.update m model.snakeModel
                       in
                          ({model | snakeModel = sModel}, Cmd.map SnakeMsg cmd)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every tickTime Clock
            , Sub.map SnakeMsg (Snake.subscriptions model.snakeModel)
            ]

-- VIEW

settings : Model -> Html Msg
settings model = Html.form [ class "settings" ]
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
speedSelector model = div [ class "form-group" ]
                          [ label [ for "speed" ] [ text "Speed" ]
                          , div [ class "btn-group" ]
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
debugToggle model = div [ class "form-group" ]
                          [ label [ for "debug" ] [ text "Debug" ]
                          , div [ class "btn-group" ]
                                [ debugOption model True
                                , debugOption model False
                                ] 
                          ]

modelInspector : Model -> Html Msg
modelInspector model = div [ classList [("hidden", not model.debug)], style [("margin-top", "40px")] ]
                           [ pre [] [text (toString model)] ]

points : Model -> Html Msg
points model = h1 [ class "status-part" ] [ text (toString model.snakeModel.bites) ]

statusPart : String -> Html Msg -> Html Msg
statusPart title content = div [] [ h4 [] [ text title ]
                                  , content
                                  ]

gameView : Model -> Html Msg
gameView model = let snakeView  = App.map SnakeMsg (Snake.view model.snakeModel)
                     statusview = div []
                                      [ statusPart "Score" (points model)
                                      , statusPart "Settings" (settings model)
                                      ]
                 in
                   div [ class "row" ]
                       [ div [ class "col-md-8" ] [ snakeView ]
                       , div [ class "col-md-4" ] [ statusview ]
                       ]

row : Html Msg -> Html Msg
row component = div [ class "row" ]
                    [ component ]

rowLayout : List(Html Msg) -> Html Msg
rowLayout components = div [ class "container" ]
                           (List.map row components)

view : Model -> Html Msg
view model = let
               inspector = modelInspector model
               baseView  = [ gameView model
                           , inspector
                           ]
             in
                div [] [rowLayout baseView]

