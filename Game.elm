import Snake exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing(..)
import Time exposing (Time, second)
import Json.Encode exposing(..)

debug    = True
tickTime = Time.inMilliseconds 150

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
  , time : Time
  }

init : (Model, Cmd Msg)
init =
  let (sModel, cmd) = Snake.init
  in
     (Model sModel 0, Cmd.map SnakeMsg cmd)

-- UPDATE

type Msg
  = Clock Time
  | SnakeMsg Snake.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
                     Clock _ ->
                       let (sModel, cmd) = Snake.update Snake.Tick model.snakeModel
                       in 
                          (Model sModel (model.time + 1), Cmd.map SnakeMsg cmd)

                     SnakeMsg m ->
                       let (sModel, cmd) = Snake.update m model.snakeModel
                       in
                          (Model sModel model.time, Cmd.map SnakeMsg cmd)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every tickTime Clock
            , Sub.map SnakeMsg (Snake.subscriptions model.snakeModel)
            ]

-- VIEW

modelInspector : Model -> Html Msg
modelInspector model = let attributes = Json.Encode.object [ ("time", float model.time)
                                                           , ("snake", Snake.debugAttributes model.snakeModel)
                                                           ]
                       in div [ style [("margin-top", "40px")] ]
                              [ pre [] [text (Json.Encode.encode 2 attributes)] ]

view : Model -> Html Msg
view model = let
               snakeView = App.map SnakeMsg (Snake.view model.snakeModel)
               baseView  = [snakeView]
               inspector = modelInspector model
             in
               if debug
                 then div [] (inspector::baseView)
                 else div [] baseView

