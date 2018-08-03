import Html exposing (Html, div, p, text, button, h1, h2, ul, li)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown)
import Svg exposing (svg, circle, line, rect, defs, linearGradient, stop)
import Svg.Attributes exposing (viewBox, height, width, cx, cy, r, fill, id, x, y, x1, x2, y1, y2, stroke, offset)
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
  , tripHistory: List Float
  , x: Int
  , y: Int
  }

init : (Model, Cmd Msg)
init =
  (
    { driverSkill = 50.0
    , riskEvents = []
    , score = 50.0
    , tripHistory = []
    , x = 0
    , y = 0
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

slider =
  { x0 = 0
  , y0 = 500
  , x1 = 800
  , y1 = 600
  }



-- UPDATE

type alias ListFloat = List Float
type alias ListRE = List RiskEvent
type Msg
  = IncrementSkill
  | DecrementSkill
  | RandRE Time
  | NewRiskEventList ListFloat
  | Position Int Int

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
    Position x y ->
      ( updateSkillFromSlider model x y, Cmd.none )
      

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

updateSkillFromSlider model x y =
  if x >= slider.x0 && x <= slider.x1
  && y >= slider.y0 && y <= slider.y1 then
    { model | x = x, y = y, driverSkill = 100.0 - (x / (toFloat slider.x1) * 100) }
  else
    model

getRE : Float -> RiskEvent
getRE newFloat =
  let
    re = List.foldr (closest newFloat) (0.1040, "hardBreak35") riskEvents
  in
    RiskEvent (Tuple.second re) (Tuple.first re)


closest newFloat sofar next =
  let
      minDistSofar =
        newFloat - (Tuple.first sofar)
        |> abs

      distNext =
        newFloat - (Tuple.first next)
        |> abs
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
    [ Time.every second RandRE
    , Mouse.clicks (\{x, y} -> Position x y)
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
    (List.append (List.append
      [ h1 [] [ text <| "Driver Score " ++ (toString <| round model.score) ]
      , div
        [ style
            [ ("position", "fixed")
            , ("top", toString slider.y0 ++ "px")
            , ("width", toString slider.x1 ++ "px")
            , ("height", toString (slider.y1 - slider.y0) ++ "px")
            ]
        ]
        [ svg
          [ viewBox "0 0 100% 30%"
          , width "100%"
          , height "100%"
          ]
          [ defs []
            [ linearGradient [ id "grad1", x1 "0%", y1 "0%", x2 "100%", y2 "0%" ]
              [ stop [ offset "0%", Svg.Attributes.style "stop-color:rgb(255,0,0,0.3);stop-opacity:1" ] []
              , stop [ offset "48%", Svg.Attributes.style "stop-color:rgb(255,255,255,0.3);stop-opacity:1" ] []
              , stop [ offset "52%", Svg.Attributes.style "stop-color:rgb(255,255,255,0.3);stop-opacity:1" ] []
              , stop [ offset "100%", Svg.Attributes.style "stop-color:rgb(0,255,0,0.3);stop-opacity:1" ] []
              ]
            ]
          , rect [ x "0", y "0", width "100%", height "100%", fill "url(#grad1)" ] []
          , line [ x1 "50%", y1 "2%", x2 "50%", y2 "98%", stroke "rgba(0,0,0,0.3)" ] []
          , maybeLine model
          ]
        ]
      ] (formatTrips model.tripHistory))
      [ h2 [] [ text <| "Driver Skill " ++ (toString (100.0 - model.driverSkill)) ]
      , div [ style [ ("position", "fixed"), ("top", toString slider.y1 ++ "px") ] ]
        [ h2 [] [ text "Próximos passos" ]
        , ul []
          [ li [] [ text "Mostrar quantidades de eventos por trip (e quais ocorreram)"]
          , li [] [ text "Quanto tempo eu demoro até chegar em uma nota boa?"]
          ]
        ]
      ])


maybeLine : Model -> Svg.Svg Msg
maybeLine model =
  line [ x1 <| toString model.x, y1 "2%", x2 <| toString model.x, y2 "98%", stroke "rgba(0,0,0,0.7)" ] []
