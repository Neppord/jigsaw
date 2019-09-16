module Edge exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events
import Svg
import Svg.Attributes
import Random
import Platform.Cmd exposing (Cmd)

import Point exposing (Point)

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

type Msg
  = Button

type alias Model =
  { curves : String
  , seed : Random.Seed
  , nx : Int
  , ny : Int
  }

init : () -> (Model, Cmd Msg)
init () =
  ( { curves = curveFromSingleEdge defaultPoints
    , seed = Random.initialSeed 0
    , nx = 5
    , ny = 5
    }
  , Cmd.none
  )

curveFromSingleEdge edge =
  "M 0 0 "
  ++ edgeToString (makeEdge True "N" edge)
  ++ edgeToString (makeEdge True "E" edge)
  ++ edgeToString (makeEdge False "S" edge)
  ++ edgeToString (makeEdge False "W" edge)


makeEdgePoints : Int -> Random.Seed -> (List EdgePoints, Random.Seed)
makeEdgePoints n seed =
  let
    (offsets, seed1) =
      Random.step (Random.list n <| Point.randomPoints 8 -5 5 -5 5) seed

    (chiralities, seed2) =
      Random.step (Random.list n <| Random.int 0 1) seed1

    -- Chirality 0 means the 'ear' is pointing up, 1 means it points down
    setChirality : EdgePoints -> Int -> List Point
    setChirality ep ch =
      if ch == 0 then
        List.map2 Point.add defaultPoints (ep ++ [Point 0 0])
      else
        List.map (\p -> Point p.x -p.y) (ep ++ [Point 0 0])
          |> List.map2 Point.add defaultPoints

    edgePoints =
      List.map2 setChirality offsets chiralities
  in
    (edgePoints, seed2)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    randomEdgePoints =
      Random.map2 Tuple.pair
        (Random.int 0 1)
        (Point.randomPoints 8 -5 5 -5 5)

    (edgePoints, seed) =
      Random.step (Random.list 4 randomEdgePoints) model.seed

    fixOffset ps =
      List.map2 Point.add defaultPoints (ps ++ [Point 0 0])

    newCurves =
      case edgePoints of
        [(ni, n), (ei, e), (si, s), (wi, w)] ->
          "M 0 0 "
          ++ (edgeToString <| makeEdge (ni == 1) "N" (fixOffset n))
          ++ (edgeToString <| makeEdge (ei == 1) "E" (fixOffset e))
          ++ (edgeToString <| makeEdge (si == 1) "S" (fixOffset s))
          ++ (edgeToString <| makeEdge (wi == 1) "W" (fixOffset w))
        _ ->
          curveFromSingleEdge defaultPoints

  in
    ( {model | seed = seed, curves = newCurves}, Cmd.none )

type alias Edge =
  { b1 : Bezier
  , b2 : Bezier
  , b3 : Bezier
  , b4 : Bezier
  }

type alias EdgePoints = List Point

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

pieceCurveFromPieceId : Int -> Int -> Int -> String
pieceCurveFromPieceId nx ny id =
  let
    nv = (nx - 1) * ny
    nh = nx * (ny - 1)

    north = getHorizontalEdge (id - nx)
    west = getVerticalEdge (id - (id // nx))
    south = getHorizontalEdge id
    east = getVerticalEdge (id - (id // nx))
  in
    "foo"

getHorizontalEdge id =
  []

getVerticalEdge id =
  []



view : Model -> Html Msg
view model =
  Html.div
  [ Html.Attributes.style "width" <| "1000px"
  , Html.Attributes.style "height" <| "800px"
  ]
  [ Html.button [ Html.Events.onClick Button ] [ Html.text "scramble" ]
  , Svg.svg
    [ Svg.Attributes.width "100%"
    , Svg.Attributes.height "100%"
    ]
    [ Svg.g
      [ Svg.Attributes.transform "translate(100,100)" ]
      [ Svg.path
        [ Svg.Attributes.d model.curves
        , Svg.Attributes.stroke "red"
        , Svg.Attributes.strokeWidth "1px"
        , Svg.Attributes.fillOpacity "1.0"
--        , Svg.Attributes.transform "rotate(45)"
        ]
        []
      ]

    ]
  ]