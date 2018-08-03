import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)
import Css exposing (..)
import Time exposing (Time, second)
import Mouse 
import Keyboard


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = 
  { time: Time
  , x: Int
  , y: Int
  }

init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , x = 0
    , y = 0
    }
    , Cmd.none
   )



-- UPDATE


type Msg
  = Tick Time
  | Position Int Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }, Cmd.none)
    Position x y ->
      ( { model | x = x, y = y }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every second Tick
    , Mouse.moves (\{x, y} -> Position x y)
    ]



-- VIEW


view : Model -> Html Msg
view model =
  let
    mstyle = 
      style
        [ ("position", "relative")
        , ("backgroundColor", "blue")
        , ("height", "100px")
        , ("width", "100px")
        , ("margin", "50px")
        ]
  in
    div []
      [ div [ mstyle ] []
      , p [] [ text <| "Model: " ++ toString model ]
      , p [] [ text <| "rel pos: " ++ relpos model ]
      , p [] [ text <| "inSeconds: " ++ (toString <| Basics.round <| Time.inSeconds model.time) ]
    ]



-- AUX


relpos : Model -> String
relpos model =
  let
    rp =
      { x = model.x - 100
      , y = model.y - 100
      }
  in
    toString rp