module Edge exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes
import Html.Events
import Svg
import Svg.Attributes
import Random
import Array

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
  ++ edgeToString (makeEdge "N" edge)
  ++ edgeToString (makeEdge "E" edge)
  ++ edgeToString (makeEdge "S" edge)
  ++ edgeToString (makeEdge "W" edge)


makeEdgePoints : Int -> Random.Seed -> (Array.Array EdgePoints, Random.Seed)
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
        ep
      else
        List.map (\p -> Point p.x -p.y) ep

    translatePoints ep =
      List.map2 Point.add defaultPoints (ep ++ [Point 0 0])

    edgePoints =
      List.map2 (setChirality << translatePoints) offsets chiralities
        |> Array.fromList
  in
    (edgePoints, seed2)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    (edgePoints, newSeed) =
      makeEdgePoints 4 model.seed

    newCurves =
      "M 0 0 "
      ++ (edgeToString <| makeEdge "N" <| getHorizontalEdge 0 edgePoints)
      ++ (edgeToString <| makeEdge "E" <| getVerticalEdge 1 edgePoints)
      ++ (edgeToString <| makeEdge "S" <| getHorizontalEdge 2 edgePoints)
      ++ (edgeToString <| makeEdge "W" <| getVerticalEdge 5 edgePoints)

  in
    ( {model | seed = newSeed, curves = newCurves}, Cmd.none )

type Edge
  = Curved { b1 : Bezier, b2 : Bezier, b3 : Bezier, b4 : Bezier}
  | Flat { a : Point, b : Point }

type alias EdgePoints = List Point

type Bezier
  = C Point Point Point
  | S Point Point


defaultCurvedEdge =
  Curved
    { b1 = C (Point 50 20) (Point 100 25) (Point 80 0)
    , b2 = S (Point 70 -40) (Point 100 -40)
    , b3 = S (Point 140 -25) (Point 120 0)
    , b4 = S (Point 150 20) (Point 200 0)
    }

defaultFlatEdge =
  Flat { a = Point 0 0, b = Point 200 0 }


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


makeEdge : String -> List Point -> Edge
makeEdge orientation points =
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
          List.reverse ps

    fixOrientation ps =
      case orientation of
        "W" -> List.map rotate <| reverse ps
        "S" -> List.map translate <| reverse ps
        "E" -> List.map (rotate << translate) ps
        _ -> ps

  in
  case fixOrientation points of
    [p1, p2, p3, p4, p5, p6, p7, p8, p9] ->
      Curved
        { b1 = C p1 p2 p3
        , b2 = S p4 p5
        , b3 = S p6 p7
        , b4 = S p8 p9
        }
    [p1, p2] ->
      Flat { a = p1, b = p2 }
    _ ->
      defaultCurvedEdge


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
  case e of
    Curved {b1, b2, b3, b4} ->
      List.map bezierToString [b1, b2, b3, b4]
        |> List.intersperse " "
        |> String.concat
    Flat {a, b} ->
      "L " ++ Point.toString a ++ ", " ++ Point.toString b

pieceCurveFromPieceId : Int -> Int -> Int -> Array.Array EdgePoints -> String
pieceCurveFromPieceId nx ny id edgePoints =
  let
    edge : String -> Edge
    edge orientation = getEdge orientation nx ny id edgePoints

    curveString =
      List.map (edge >> edgeToString) ["N", "E", "S", "W"]
        |> String.concat
  in
    "M 0 0 " ++ curveString

getEdge : String -> Int -> Int -> Int -> Array.Array EdgePoints-> Edge
getEdge orientation nx ny id edgePoints =
  let
    nv = (nx - 1) * ny
    n = nx * ny
    index =
      case orientation of
        "N" ->
          if id < nx then -1 else id - nx + nv
        "W" ->
          if (modBy nx id) == 0 then -1 else id - (id // nx) - 1
        "S" ->
          if id >= n - nx then -1 else id + nv
        _ ->
          if (modBy nx id) == (nx - 1) then -1 else id - (id // nx)
    points =
      Array.get index edgePoints
        |> Maybe.withDefault [Point 0 0, Point 200 0]
  in
    makeEdge orientation points

getHorizontalEdge id edgePoints =
  Array.get id edgePoints |> Maybe.withDefault [Point 0 0, Point 200 0]

getVerticalEdge id edgePoints =
  Array.get id edgePoints |> Maybe.withDefault [Point 0 0, Point 200 0]



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