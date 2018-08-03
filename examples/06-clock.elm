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
import Debug exposing (log)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
    
    
-- Types

type alias RiskEvent =
  {
    event_type: String,
    event_weight: Float
  }
  
-- Funcs

tripScore: List RiskEvent -> Int -> Float -> Float
tripScore risk_events tripInSecs distAdjustCoeff =
  let
    weights = List.map (\e -> e.event_weight) risk_events
  in
    let sum = List.foldr (+) 0.0 weights
    in (e ^ ( -sum / (toFloat tripInSecs * distAdjustCoeff) )) * 100
    
    
updateDriverScore: Float -> Float -> Float -> Float -> Float -> Float
updateDriverScore oldScore newScore tripDistInKm movingAvgCoeff maxAlpha =
  let
    alpha = Basics.min (movingAvgCoeff * (logBase e (tripDistInKm + e))) maxAlpha
  in
    log (String.concat ["alpha ",  (toString alpha)])
    log (String.concat ["dist log ",  (toString (logBase e (tripDistInKm + e)))])
    log (String.concat ["new ",  (toString ((1 - alpha) * oldScore + alpha * newScore))])
    (1 - alpha) * oldScore + alpha * newScore


-- DriverScore

distanceAdjustmentCoefficient = 0.0023
movingAverageCoefficient = 0.015
maximumAverageCoefficient = 0.2

baseDuration = 1800 -- 30min
baseKm = 50 -- 50km

driverScore: Float -> List RiskEvent -> Float
driverScore oldScore riskEvents =
  let
    tripS = tripScore riskEvents baseDuration distanceAdjustmentCoefficient
  in
    log (String.concat ["ts ", (toString tripS)])
    updateDriverScore oldScore tripS baseKm movingAverageCoefficient maximumAverageCoefficient


highAcc = RiskEvent "HardAcc" 1.0
midAcc = RiskEvent "MidAcc" 0.415
lowAcc = RiskEvent "LowAcc" 0.125

-- MODEL


type alias Model =
  { time: Time
  , driverSkill: Float
  , randValue: Float
  , riskEvent: String
  , score: Float
  }

init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , driverSkill = 50.0
    , randValue = 0.0
    , riskEvent = "Nothing"
    , score = 50.0
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
-- UPDATE


type Msg
  = Tick Time
  | IncrementSkill
  | DecrementSkill
  | Rand Time
  | NewValue Float
  | RandRE
  | NewRE Float
  | IncrementScore
  | DecrementScore


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }, Cmd.none)
    DecrementSkill ->
      ( { model | driverSkill = updateSkill model -1 }, Cmd.none )
    IncrementSkill ->
      ( { model | driverSkill = updateSkill model 1 }, Cmd.none )
    Rand newTime ->
      ( model, Random.generate NewValue (Random.float 0.0 1.0) )
    NewValue newFloat ->
      ( { model | randValue = newFloat }, Cmd.none )
    RandRE ->
      ( model, Random.generate NewRE (normal (model.driverSkill / 100.0) 0.2) )
    NewRE newFloat ->
      ( { model | riskEvent = getRE newFloat }, Cmd.none )
    IncrementScore ->
      ( { model | score = driverScore model.score [lowAcc] }, Cmd.none )
    DecrementScore ->
      ( { model | score = driverScore model.score [lowAcc, highAcc, highAcc, highAcc] }, Cmd.none )

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
    re = List.head riskEvents
  in
    case re of
      Just some ->
        Tuple.second some
      Nothing ->
        "Nothing"

        
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every second Tick
    , Time.every second Rand
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
      [ p [] [ text <| "Model: " ++ toString model ]
      , p [] [ text <| "inSeconds: " ++ (toString <| Basics.round <| Time.inSeconds model.time) ]
      , div []
        [ text <| "Driver Skill " ++ (toString model.driverSkill)
        , button [ onClick IncrementSkill ] [ text "+"]
        , button [ onClick DecrementSkill ] [ text "-"]
      ]
      , div []
        [ text <| "Driver Score " ++ (toString model.score)
        , button [ onClick IncrementScore ] [ text "better"]
        , button [ onClick DecrementScore ] [ text "worse"]
      ]
      , div []
        [ p [] [ text <| "Random " ++ (toString model.randValue) ]
      ]
      , div []
        [ text <| "Risk Event: " ++ (toString model.riskEvent)
        , button [ onClick RandRE ] [ text "change"]
      ]
    ]
