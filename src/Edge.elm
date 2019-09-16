module Edge exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes
import Svg
import Svg.Attributes

import Point exposing (Point)

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }


type Msg
  = Foo

type alias Model =
  { curves : List String
  }

init : () -> (Model, Cmd Msg)
init () =
  ( {curves = []}, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

type alias Edge =
  { b1 : Bezier
  , b2 : Bezier
  , b3 : Bezier
  , b4 : Bezier
  }

type Bezier
  = C Point Point Point
  | S Point Point


defaultEdge =
  { b1 = C (Point 50 20) (Point 100 25) (Point 80 0)
  , b2 = S (Point 70 -40) (Point 100 -40)
  , b3 = S (Point 140 -25) (Point 120 0)
  , b4 = S (Point 150 20) (Point 200 0)
  }

defaultPoints =
  [ Point 50 20
  , Point 100 25
  , Point 80 0
  , Point 70 -40
  , Point 100 -40
  , Point 140 -25
  , Point 120 0
  , Point 150 20
  , Point 200 0
  ]


makeEdge : Bool -> String -> List Point -> Edge
makeEdge isInverted orientation points =
  let
    rotate : Point -> Point
    rotate p =
      Point p.y p.x

    translate : Point -> Point
    translate p =
      Point.add p (Point 0 200)

    flip p q =
      Point (2*q.x - p.x) (2*q.y - p.y)

    reverse : List Point -> List Point
    reverse ps =
      case ps of
        [p1, p2, p3, p4, p5, p6, p7, p8, _] ->
          [p8, flip p6 p7, p7, flip p4 p5, p5, flip p2 p3, p3, p1, Point 0 0]
        _ ->
          ps

    fixOrientation ps =
      case orientation of
        "W" -> List.map rotate <| reverse ps
        "S" -> List.map translate <| reverse ps
        "E" -> List.map (rotate << translate) ps
        _ -> ps

    invert : Point -> Point
    invert p =
      Point p.x -p.y

    fixInversion ps =
      if isInverted then
        List.map invert ps
      else
        ps
  in
  case (fixOrientation << fixInversion) points of
    [p1, p2, p3, p4, p5, p6, p7, p8, p9] ->
      { b1 = C p1 p2 p3
      , b2 = S p4 p5
      , b3 = S p6 p7
      , b4 = S p8 p9
      }
    _ -> defaultEdge

bezierToString : Bezier -> String
bezierToString b =
  let
    combine pts =
      String.concat
        <| List.intersperse ", "
        <| List.map Point.toString pts
  in
  case b of
    C p1 p2 p3 ->
      "C " ++ (combine [p1, p2, p3])
    S p1 p2 ->
      "S " ++ (combine [p1, p2])

edgeToString : Edge -> String
edgeToString e =
  List.map bezierToString [e.b1, e.b2, e.b3, e.b4]
    |> List.intersperse " "
    |> String.concat


view : Model -> Html Msg
view model =
  let
    edge =
      [ Point 50 20
      , Point 100 25
      , Point 80 0
      , Point 70 -40
      , Point 100 -40
      , Point 140 -25
      , Point 120 0
      , Point 150 20
      , Point 200 0
      ]
  in
  Html.div
    [ Html.Attributes.style "width" <| "1000px"
    , Html.Attributes.style "height" <| "800px"
    ]
    [ Html.h1 [] [ Html.text "hello" ]
    , Svg.svg
      [ Svg.Attributes.width "100%"
      , Svg.Attributes.height "100%"
      ]
      [ Svg.g
        [ Svg.Attributes.transform "translate(100,100)" ]
        [ Svg.path
          [ Svg.Attributes.d
            <| "M 0 0 "
            ++ edgeToString (makeEdge True "N" defaultPoints)
            ++ edgeToString (makeEdge True "E" defaultPoints)
            ++ edgeToString (makeEdge False "S" defaultPoints)
            ++ edgeToString (makeEdge False "W" defaultPoints)
          , Svg.Attributes.stroke "red"
          , Svg.Attributes.strokeWidth "1px"
          , Svg.Attributes.fillOpacity "1.0"
          ]
          []
        ]

      ]
    ]