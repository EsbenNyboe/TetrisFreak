module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (Svg)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
    
-- MODEL
type alias Model = List Cube

type alias Cube = 
  { x : Int
  , y : Int
  }

init: () -> (Model, Cmd Msg)
init _ = ([{x=100, y=200}, {x=200, y=200}], Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

type Msg
  = Move Direction
  | SpawnCube

type Direction
  = Left | Right | Up | Down

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
      SpawnCube -> (addCube {x=50, y=50} model, Cmd.none)
      Move direction -> (List.map (updateSingleCube direction) model, Cmd.none)

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

addCube : Cube -> Model -> Model
addCube = (::)
-- addCube cube model = 
--   cube :: model

-- VIEW
view: Model -> Html Msg
view model = 
  div []
    [ button [ onClick (Move Left) ] [ text "Left" ]
    , button [ onClick (Move Right) ] [ text "Right" ]
    , div [] [ text (getFirstCubeX model) ]
    , button [ onClick (Move Up) ] [ text "Up" ]
    , button [ onClick (Move Down) ] [ text "Down" ]
    , div [] [ text (getFirstCubeY model) ]
    , button [ onClick SpawnCube ] [ text "Spawn" ]
    , drawBoxes model
    ]

getFirstCubeX: Model -> String
getFirstCubeX model = 
  String.fromInt 
    (case List.head model of 
      Just cube -> cube.x
      Nothing -> 0)

getFirstCubeY: Model -> String
getFirstCubeY model = 
  String.fromInt 
    (case List.head model of 
      Just cube -> cube.y
      Nothing -> 0)

drawBoxes: Model -> Html Msg
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
