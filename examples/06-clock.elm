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
    -- log (String.concat ["alpha ",  (toString alpha)])
    -- log (String.concat ["dist log ",  (toString (logBase e (tripDistInKm + e)))])
    -- log (String.concat ["new ",  (toString ((1 - alpha) * oldScore + alpha * newScore))])
    (1 - alpha) * oldScore + alpha * newScore


-- DriverScore

distanceAdjustmentCoefficient = 0.0023
movingAverageCoefficient = 0.015
maximumAverageCoefficient = 0.2
maxNumberOfEvents = 30

baseDuration = 1800 -- 30min
baseKm = 50 -- 50km

driverScore: Float -> List RiskEvent -> Float
driverScore oldScore riskEvents =
  let
    tripS = tripScore riskEvents baseDuration distanceAdjustmentCoefficient
  in
    -- log (String.concat ["ts ", (toString tripS)])
    updateDriverScore oldScore tripS baseKm movingAverageCoefficient maximumAverageCoefficient


-- MODEL


type alias Model =
  { driverSkill: Float
  , riskEvents: List RiskEvent
  , score: Float
  , tripHistory: ListFloat
  }

init : (Model, Cmd Msg)
init =
  (
    { driverSkill = 50.0
    , riskEvents = []
    , score = 50.0
    , tripHistory = []
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

type alias ListFloat = List Float
type alias ListRE = List RiskEvent
type Msg
  = IncrementSkill
  | DecrementSkill
  | RandRE Time
  | NewRiskEventList ListFloat

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DecrementSkill ->
      ( { model | driverSkill = updateSkill model -1 }, Cmd.none )
    IncrementSkill ->
      ( { model | driverSkill = updateSkill model 1 }, Cmd.none )
    RandRE newTime ->
      ( model, Random.generate NewRiskEventList (Random.list maxNumberOfEvents (normal (model.driverSkill / 100.0) gaussStdDev)) )
    NewRiskEventList newFloatList ->
      let
        re = riskEventsFromFloatList newFloatList model.driverSkill
        tripS = tripScore re baseDuration distanceAdjustmentCoefficient
      in
        ( { model | score = driverScore model.score re,  riskEvents = re, tripHistory = List.take 10 (tripS :: model.tripHistory)}, Cmd.none )
      

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

getRE : Float -> RiskEvent
getRE newFloat =
  let
    re = List.foldr (closest newFloat) (0.1040, "hardBreak35") riskEvents
  in
    RiskEvent (Tuple.second re) (Tuple.first re)

closest newFloat sofar next =
  let
      minDistSofar = abs <| newFloat - (Tuple.first sofar)
      distNext = abs <| newFloat - (Tuple.first next)
  in
      if minDistSofar < distNext then
        sofar
      else
        next
        
riskEventsFromFloatList: List Float -> Float -> List RiskEvent
riskEventsFromFloatList randomFloats skill =
  let
    numberOfEvents = round ( (skill / 100) * maxNumberOfEvents)
  in
    List.take numberOfEvents (List.map (\r -> getRE r) randomFloats)
    

tripToHtml trip =
  (
    div []
    [ text <| "Trip Score: " ++ (toString trip) ]
  )
formatTrips listOfTrips = List.map tripToHtml listOfTrips

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every second RandRE ]

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
    (List.append
      [ p [] [ text <| "Model: " ++ toString model ]
      , div []
        [ text <| "Driver Skill " ++ (toString model.driverSkill)
        , button [ onClick IncrementSkill ] [ text "+"]
        , button [ onClick DecrementSkill ] [ text "-"]
        ]
      , div []
        [ text <| "Driver Score " ++ (toString model.score) ]
      , div []
        [ text <| "Number of Risk Event: " ++ (toString (List.length model.riskEvents)) ]
      ]
      (formatTrips model.tripHistory))
