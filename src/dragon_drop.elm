module Main exposing (..)

import Browser
import Array as A
import Html exposing (Html)
import Html.Events
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Random

import Point exposing (Point)

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
  { cursor : Maybe Point
  , pieces : A.Array Piece
  , maxZLevel : Int
  }

type Msg
  = MouseDown Int Point
  | MouseMove Point
  | MouseUp
  | Scramble
  | ScrambledPositions (List Point)

type alias Piece =
  { position : Point
  , offset : Point
  , selected : Bool
  , zlevel : Int
  , id : Int
--  , neighbors : List Piece
  }



-- Until I figure out how to handle index out of bounds
-- exceptions more elegantly
defaultPiece =
  Piece (Point 0 0) (Point 0 0) False -1 -1


-- INIT

init : () -> ( Model, Cmd Msg )
init () =
  let
    model =
      { cursor = Nothing
      , pieces = createPieces 5 5
      , maxZLevel = 24
      }
  in
  ( model, Cmd.none )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    selectPiece : Int -> A.Array Piece
    selectPiece id =
      case A.get id model.pieces of
        Nothing ->
          model.pieces
        Just piece ->
          A.set
            id
            { piece
              | selected = True
              , zlevel = model.maxZLevel
            }
            model.pieces

    turnOffSelectedStatus : Piece -> Piece
    turnOffSelectedStatus piece =
      { piece | selected = False }

    movePieceTo : Point -> Piece -> Piece
    movePieceTo newPosition piece =
      if piece.selected then
        { piece | position = Point.add piece.position newPosition }
      else
        piece
  in
  case msg of
    Scramble ->
      ( model
      , Random.generate ScrambledPositions
        <| Point.randomPoints (A.length model.pieces) 0 500
      )

    ScrambledPositions newPositions ->
      let
        changePiecePosition ind point =
          case A.get ind model.pieces of
            Just piece ->
              { piece | position = point }
            Nothing ->
              defaultPiece

        changeAllPiecePositions =
          List.indexedMap changePiecePosition newPositions

      in
        ( { model | pieces = A.fromList changeAllPiecePositions }
        , Cmd.none
        )

    MouseDown id coordinate ->
      ( { model
          | cursor = Just coordinate
          , pieces = selectPiece id
          , maxZLevel = model.maxZLevel + 1
        }
      , Cmd.none
      )

    MouseUp ->
      ( { model
          | cursor = Nothing
          , pieces = A.map turnOffSelectedStatus model.pieces
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
              , pieces = A.map (movePieceTo <| Point.sub newPos oldPos) model.pieces
            }
          , Cmd.none
          )



-- VIEW

view : Model -> Html Msg
view model =
  let
    definitions =
      Svg.defs [] ( definePuzzleImage :: definePieceClipPaths model )

    pieces =
      List.map svgPiece
        <| List.sortBy .zlevel
        <| A.toList model.pieces


    svgPiece piece =
      Svg.g
        [ onMouseDown piece.id
        , translate piece.position
        ]
        [ Svg.use
          [ Svg.Attributes.xlinkHref <| "#puzzle-image"
          , Svg.Attributes.clipPath <| clipPathRef piece
          ]
          []
        ]

  in
  Html.div []
    [ Html.h1 [] [ Html.text ( "Kitten jigsaw! " ) ]
    , Html.button [ Html.Events.onClick Scramble ] [ Html.text "scramble" ]
    , Svg.svg
      ( svgAttributes model )
      ( definitions :: pieces)
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

coordinateDecoder : Json.Decode.Decoder Point
coordinateDecoder =
  Json.Decode.map2 Point
    (Json.Decode.field "clientX" Json.Decode.int)
    (Json.Decode.field "clientY" Json.Decode.int)

translate : Point -> Svg.Attribute Msg
translate position =
  Svg.Attributes.transform
    <| "translate(" ++ String.fromInt position.x ++ "," ++ String.fromInt position.y ++ ")"


-- =================


createPieces : Int -> Int -> A.Array Piece
createPieces nx ny =
  let
    n = nx*ny
    width = 100
    height = 100
    range =
      A.fromList <| List.range 0 (n - 1)
    onePiece i =
      { position = Point 0 0
      , offset = Point (width*(modBy nx i)) (height*(i//nx))
      , selected = False
      , id = i
      , zlevel = i
      }

  in
    A.map onePiece range


definePuzzleImage : Svg Msg
definePuzzleImage =
  Svg.image
    [ Svg.Attributes.id "puzzle-image"
    , Svg.Attributes.xlinkHref "../resources/kitten.png"
    ]
    []


definePieceClipPaths : Model -> List (Svg Msg)
definePieceClipPaths model =
    List.map pieceClipPath (A.toList model.pieces)

pieceClipPath : Piece -> Svg Msg
pieceClipPath piece =
    let
      px num =
        String.fromInt num ++ "px"
    in
      Svg.clipPath [ Svg.Attributes.id <| pieceClipId piece ]
        [ Svg.rect
          [ Svg.Attributes.id <| pieceOutlineId piece
          , Svg.Attributes.width <| px 100
          , Svg.Attributes.height <| px 100
          , Svg.Attributes.x <| px <| piece.offset.x
          , Svg.Attributes.y <| px <| piece.offset.y
          ]
          []
      ]

pieceOutlineId : Piece -> String
pieceOutlineId piece =
    "piece-" ++ String.fromInt piece.id ++ "-outline"

pieceClipId : Piece -> String
pieceClipId piece =
    "piece-" ++ String.fromInt piece.id ++ "-clip"

clipPathRef : Piece -> String
clipPathRef piece =
    "url(#" ++ pieceClipId piece ++ ")"

