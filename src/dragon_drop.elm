module Main exposing (..)

import Browser
import Array as A
import Html exposing (Html)
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events


-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }


-- MODEL

type alias Model =
  { cursor : Maybe Coordinate
  , circles : A.Array Circle
  }

type Msg
  = MouseDown Int Coordinate
  | MouseMove Coordinate
  | MouseUp

type alias Circle =
  { center : Coordinate
  , selected : Bool
  , id : Int
  }

type alias Coordinate =
  { x: Int
  , y: Int
  }

sub : Coordinate -> Coordinate -> Coordinate
sub a b =
  Coordinate (a.x - b.x) (a.y - b.y)

add : Coordinate -> Coordinate -> Coordinate
add a b =
  Coordinate (a.x + b.x) (a.y + b.y)


-- INIT

init : () -> ( Model, Cmd Msg )
init () =
  let
    model =
      { cursor = Nothing
      , circles = A.fromList
        [ Circle ( Coordinate 50 500 ) False 0
        , Circle ( Coordinate 100 500 ) False 1
        , Circle ( Coordinate 150 500 ) False 2
        , Circle ( Coordinate 200 500 ) False 3
        , Circle ( Coordinate 250 500 ) False 4
        , Circle ( Coordinate 300 500 ) False 5
        , Circle ( Coordinate 350 500 ) False 6
        , Circle ( Coordinate 400 500 ) False 7
        , Circle ( Coordinate 450 500 ) False 8
        , Circle ( Coordinate 500 500 ) False 9
        , Circle ( Coordinate 550 500 ) False 10
        , Circle ( Coordinate 600 500 ) False 11
        , Circle ( Coordinate 650 500 ) False 12
        , Circle ( Coordinate 700 500 ) False 13
        , Circle ( Coordinate 750 500 ) False 14
        , Circle ( Coordinate 800 500 ) False 15
        , Circle ( Coordinate 850 500 ) False 16
        ]
      }
  in
  ( model, Cmd.none )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    selectCircle : Int -> A.Array Circle
    selectCircle id =
      case A.get id model.circles of
        Nothing ->
          model.circles
        Just circle ->
          A.set id { circle | selected = True } model.circles

    turnOffSelectedStatus : Circle -> Circle
    turnOffSelectedStatus circle =
      { circle | selected = False }

    moveCircleTo : Coordinate -> Circle -> Circle
    moveCircleTo position circle =
      if circle.selected then
        { circle | center = add circle.center position }
      else
        circle
  in
  case msg of
    MouseDown id coordinate ->
      ( { model
          | cursor = Just coordinate
          , circles = selectCircle id
        }
      , Cmd.none
      )

    MouseUp ->
      ( { model
          | cursor = Nothing
          , circles = A.map turnOffSelectedStatus model.circles
        }
      , Cmd.none
      )

    MouseMove newPos ->
      case model.cursor of
        Nothing ->
          ( model, Cmd.none )

        Just oldPos ->
          ( { model
              | cursor = Just newPos
              , circles = A.map (moveCircleTo <| sub newPos oldPos) model.circles
            }
          , Cmd.none
          )



-- VIEW

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.h1 [] [ Html.text ( "Test! " ) ]
    , Svg.svg
      ( svgAttributes model )
      ( List.map circleSvg (A.toList model.circles) )
    ]

svgAttributes model =
  let
    attributes =
      [ Svg.Attributes.width "1000"
      , Svg.Attributes.height "1000"
      ]
    shouldTrackMouseMovement =
      model.cursor /=  Nothing
  in
  if shouldTrackMouseMovement then
    onMouseMove :: onMouseUp :: attributes
  else
    attributes

circleSvg : Circle -> Svg Msg
circleSvg circle =
  Svg.circle
    [ Svg.Attributes.fill "yellow"
    , Svg.Attributes.stroke "green"
    , Svg.Attributes.strokeWidth "4"
    , Svg.Attributes.r (if circle.selected then "27" else "25")
    , onMouseDown circle.id
    , translate circle.center
    ]
    []

onMouseUp : Svg.Attribute Msg
onMouseUp =
  Svg.Events.onMouseUp MouseUp

onMouseDown : Int -> Svg.Attribute Msg
onMouseDown id =
  Svg.Events.on "mousedown"
    <| Json.Decode.map (MouseDown id) coordinateDecoder


onMouseMove : Svg.Attribute Msg
onMouseMove =
  Svg.Events.on "mousemove"
    <| Json.Decode.map MouseMove coordinateDecoder

coordinateDecoder : Json.Decode.Decoder Coordinate
coordinateDecoder =
  Json.Decode.map2 Coordinate
    (Json.Decode.field "clientX" Json.Decode.int)
    (Json.Decode.field "clientY" Json.Decode.int)

translate : Coordinate -> Svg.Attribute Msg
translate position =
  Svg.Attributes.transform
    <| "translate(" ++ String.fromInt position.x ++ "," ++ String.fromInt position.y ++ ")"
