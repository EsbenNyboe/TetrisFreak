-- elm reactor
-- TODO: install optional Elm stuff

module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Browser.Events

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
    
-- MODEL
type alias Model = { time : Float, allCubes : AllCubes}

type alias AllCubes = List Cube

type alias Cube = 
  { x : Int
  , y : Int
  }

type Msg
  = Move Direction
  | SpawnCube
  | OnAnimationFrameDelta Float

type Direction
  = Left | Right | Up | Down

init: () -> (Model, Cmd Msg)
init _ = ({time = 0, allCubes = [{x=100, y=200}, {x=200, y=200}]}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Browser.Events.onAnimationFrameDelta OnAnimationFrameDelta

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  (case msg of
    SpawnCube -> { model | allCubes = (addCube {x=50, y=50} model.allCubes) }
    Move direction -> { model | allCubes = (List.map (updateSingleCube direction) model.allCubes) }
    OnAnimationFrameDelta delta -> { model | time = model.time + delta }
    , Cmd.none)

updateSingleCube : Direction -> Cube -> Cube
updateSingleCube direction cube = 
  case direction of
    Left ->
      { cube | x = cube.x - 10 }
    Right ->
      { cube | x = cube.x + 10 }
    Up ->
      { cube | y = cube.y - 10 }
    Down ->
      { cube | y = cube.y + 10 }

addCube : Cube -> AllCubes -> AllCubes
addCube = (::)
-- addCube cube model = 
--   cube :: model

-- VIEW
view: Model -> Html Msg
view model = 
  div []
    [ div [] [ text (String.fromFloat model.time) ]
    , button [ onClick (Move Left) ] [ text "Left" ]
    , button [ onClick (Move Right) ] [ text "Right" ]
    , div [] [ text (getFirstCubeX model.allCubes) ]
    , button [ onClick (Move Up) ] [ text "Up" ]
    , button [ onClick (Move Down) ] [ text "Down" ]
    , div [] [ text (getFirstCubeY model.allCubes) ]
    , button [ onClick SpawnCube ] [ text "Spawn" ]
    , drawBoxes model.allCubes
    ]

getFirstCubeX: AllCubes -> String
getFirstCubeX model = 
  String.fromInt 
    (case List.head model of 
      Just cube -> cube.x
      Nothing -> 0)

getFirstCubeY: AllCubes -> String
getFirstCubeY model = 
  String.fromInt 
    (case List.head model of 
      Just cube -> cube.y
      Nothing -> 0)

drawBoxes: AllCubes -> Html Msg
drawBoxes model = svg
    [ viewBox "0 0 400 400"
    , width "400"
    , height "400"
    ]
    (List.map drawBox model)

drawBox: Cube -> Svg Msg
drawBox cube = rect
        [ x (String.fromInt cube.x)
        , y (String.fromInt cube.y)
        , width "40"
        , height "40"
        , fill "red"
        , stroke "black"
        , strokeWidth "2"
        ]
        []
