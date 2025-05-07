-- elm reactor
-- F8


module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
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
    { time : Float, allCubes : AllCubes }


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0, allCubes = [] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta OnAnimationFrameDelta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        SpawnCube ->
            { model | allCubes = addCube { x = 50, y = 50, velX = 1, velY = 0.5 } model.allCubes }

        OnAnimationFrameDelta delta ->
            { time = model.time + delta, allCubes = List.map updateSingleCube model.allCubes }
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
        , button [ onClick SpawnCube ] [ text "Spawn" ]
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
