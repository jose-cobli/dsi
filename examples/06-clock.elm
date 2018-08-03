import Html exposing (Html, div, p, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
  , onoff: Bool
  , driverSkill: Int
  }

init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , x = 0
    , y = 0
    , onoff = False
    , driverSkill = 50
    }
    , Cmd.none
   )



-- UPDATE


type Msg
  = Tick Time
  | Position Int Int
  | OnOff
  | IncrementSkill
  | DecrementSkill


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }, Cmd.none)
    Position x y ->
      ( { model | x = x, y = y }, Cmd.none)
    OnOff ->
      ( { model | onoff = not model.onoff }, Cmd.none)
    DecrementSkill ->
      ( { model | driverSkill = updateSkill model -1 }, Cmd.none )
    IncrementSkill ->
      ( { model | driverSkill = updateSkill model 1 }, Cmd.none )

updateSkill : Model -> Int -> Int
updateSkill model i =
  let
    updatedDriverSkill = model.driverSkill + i
  in
    if updatedDriverSkill < 0 then
      0
    else if updatedDriverSkill > 100 then
      100
    else
      updatedDriverSkill

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
      , button [ onClick OnOff ] [ text ("Turn me " ++ btnstate model) ]
      , div []
        [ text <| "Driver Skill " ++ (toString model.driverSkill)
        , button [ onClick IncrementSkill ] [ text "+"]
        , button [ onClick DecrementSkill ] [ text "-"]
      ]
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


btnstate : Model -> String
btnstate model =
  if model.onoff then
    "off"
  else
    "on"