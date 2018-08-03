import Html exposing (Html, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Mouse 
import Keyboard



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)



-- UPDATE


type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    div []
      [ text (toString model)
        , div []
          [ text <| toString <| Basics.round <| Time.inSeconds model ]
        , div []
          [ text <| toString <| cos <| Time.inMinutes model ]
      ]
