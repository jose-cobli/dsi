import Html exposing (Html, div, p, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
-- import Svg exposing (..)
-- import Svg.Attributes exposing (..)
-- import Css exposing (..)
import Time exposing (Time, second)
import Mouse
-- import Keyboard
import Random
import Random.Float exposing (normal)


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
  , driverSkill: Float
  , randValue: Float
  , riskEvent: String
  , randGauss: Float
  }

init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , x = 0
    , y = 0
    , onoff = False
    , driverSkill = 10.0
    , randValue = 0.0
    , riskEvent = "Nothing"
    , randGauss = -1.0
    }
  , Cmd.none
  )

riskEvents =
  [ (0.1040, "hardBreak35")
  , (0.1250, "fastAcc35")
  , (0.2023, "hardBreak45")
  , (0.2665, "speedyTurn")
  , (0.4050, "hardBreak55")
  , (0.4157, "fastAcc45")
  , (1.0000, "fastAcc55")
  ]

gaussStdDev = 0.1
-- UPDATE


type Msg
  = Tick Time
  | Position Int Int
  | OnOff
  | IncrementSkill
  | DecrementSkill
  | Rand Time
  | NewValue Float
  | RandRE
  | NewRE Float


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
    Rand newTime ->
      ( model, Random.generate NewValue (Random.float 0.0 1.0) )
    NewValue newFloat ->
      ( { model | randValue = newFloat }, Cmd.none )
    RandRE ->
      ( model, Random.generate NewRE (normal (model.driverSkill / 100.0) gaussStdDev) )
    NewRE newFloat ->
      ( { model | riskEvent = getRE newFloat, randGauss = newFloat }, Cmd.none )

updateSkill : Model -> Float -> Float
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

getRE : Float -> String
getRE newFloat =
  let
    re = List.foldr (closest newFloat) (0.1040, "hardBreak35") riskEvents
  in
    Tuple.second re

closest newFloat sofar next =
  let
      minDistSofar = abs <| newFloat - (Tuple.first sofar)
      distNext = abs <| newFloat - (Tuple.first next)
  in
      if minDistSofar < distNext then
        sofar
      else
        next




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every second Tick
    , Time.every second Rand
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
      , div []
        [ p [] [ text <| "Random: " ++ (toString model.randValue) ]
        , p [] [ text <| "Random Gauss: " ++ (toString model.randGauss) ]
        ]
      , div []
        [ text <| "Risk Event: " ++ (toString model.riskEvent)
        , button [ onClick RandRE ] [ text "change"]
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