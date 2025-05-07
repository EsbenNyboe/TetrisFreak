-- elm reactor
-- F8
-- Set direction, from middle of screen to point that was clicked. Example: https://ellie-app.com/vp6SMVTgYVPa1


module Main exposing (..)

import Browser
import Browser.Events exposing (onClick)
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { time : Float, allCubes : AllCubes, lastClickedPoint : { x : Int, y : Int } }


type alias AllCubes =
    List Cube


type alias Cube =
    { x : Float
    , y : Float
    , velX : Float
    , velY : Float
    }


type Msg
    = SpawnCube
    | OnAnimationFrameDelta Float
    | Click { x : Int, y : Int }


type Direction
    = Left
    | Right
    | Up
    | Down


mapSize : Float
mapSize =
    400


cubeSize : Float
cubeSize =
    40


startDirection : { x : Float, y : Float }
startDirection =
    { x = 1, y = 0.5 }


startSpeed : Float
startSpeed =
    2


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0, allCubes = [], lastClickedPoint = { x = 0, y = 0 } }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta OnAnimationFrameDelta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        SpawnCube ->
            { model | allCubes = addCube { x = mapSize / 2, y = mapSize / 2, velX = startDirection.x * startSpeed, velY = startDirection.y * startSpeed } model.allCubes }

        OnAnimationFrameDelta delta ->
            { time = model.time + delta, allCubes = List.map updateSingleCube model.allCubes, lastClickedPoint = model.lastClickedPoint }

        Click point ->
            { model | lastClickedPoint = point }
    , Cmd.none
    )


updateSingleCube : Cube -> Cube
updateSingleCube { x, y, velX, velY } =
    { x = x + velX
    , y = y + velY
    , velX =
        if x + velX <= mapSize - cubeSize && x + velX >= cubeSize then
            velX

        else
            velX * -1
    , velY =
        if y + velY <= mapSize - cubeSize && y + velY >= cubeSize then
            velY

        else
            velY * -1
    }


addCube : Cube -> AllCubes -> AllCubes
addCube =
    (::)



-- addCube cube model =
--   cube :: model
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (String.fromFloat model.time) ]
        , button [ Html.Events.onClick SpawnCube ] [ text "Spawn" ]
        , drawBoxes model.allCubes
        ]


drawBoxes : AllCubes -> Html Msg
drawBoxes model =
    svg
        [ viewBox (String.concat [ "0 0 ", String.fromFloat mapSize, " ", String.fromFloat mapSize ])
        , width (String.fromFloat mapSize)
        , height (String.fromFloat mapSize)
        , Html.Attributes.style "background-color" "black"
        ]
        (List.map drawBox model)


drawBox : Cube -> Svg Msg
drawBox cube =
    rect
        [ x (String.fromFloat cube.x)
        , y (String.fromFloat cube.y)
        , width (String.fromFloat cubeSize)
        , height (String.fromFloat cubeSize)
        , fill "red"
        , stroke "black"
        , strokeWidth "2"
        ]
        []
