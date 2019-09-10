module Main exposing (..)

import Browser
import Array as A
import Dict as D
import Html exposing (Html)
import Html.Attributes
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
  , pieceGroups : D.Dict Int PieceGroup
  , maxZLevel : Int
  , image : JigsawImage
  , width : Int
  , height : Int
  }

type alias JigsawImage =
    { path : String
    , width : Int
    , height : Int
    , xpieces : Int
    , ypieces : Int
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
  , neighbours : List Int
  }

type alias PieceGroup =
    { id : Int
    , members : List Int
    , neighbours : List Int
    , position : Point
    , selected : Bool
    , zlevel : Int
    }


-- Until I figure out how to handle index out of bounds
-- exceptions more elegantly
defaultPiece =
  { position = (Point 0 0)
  , offset = (Point 0 0)
  , selected = False
  , zlevel = -1
  , id = -1
  , neighbours = []}


-- INIT

init : () -> ( Model, Cmd Msg )
init () =
  let
    image =
      { path = "../resources/kitten.png"
      , width = 533
      , height = 538
      , xpieces = 6
      , ypieces = 6
      }
    model =
      { cursor = Nothing
      , pieces = createPieces image
      , pieceGroups = D.empty
      , maxZLevel = image.xpieces * image.ypieces - 1
      , image = image
      , width = 1200
      , height = 850
      }
  in
  ( model, Cmd.none )


createPieces : JigsawImage -> A.Array Piece
createPieces image =
  let
    nx = image.xpieces
    ny = image.ypieces
    n = nx*ny
    pieceWidth = image.width // nx
    pieceHeight = image.height // ny
    range =
      A.fromList <| List.range 0 (n - 1)
    toPoint id =
      Point (modBy nx id) (id // nx)
    offset id =
      Point.dot
        ( toPoint id )
        ( Point pieceWidth pieceHeight )
    neighbourOffsets =
      [ -nx, -1, 1, nx ]
    possibleNeighbours i =
      List.map ((+) i) neighbourOffsets
    isRealNeighbour i x =
      Point.taxiDist (toPoint i) (toPoint x) == 1
    onePiece i =
      { position = Point 0 0
      , offset = offset i
      , selected = False
      , id = i
      , zlevel = i
      , neighbours = List.filter (isRealNeighbour i) <| possibleNeighbours i
      }

  in
    A.map onePiece range



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
        <| Point.randomPoints (A.length model.pieces) 50 (model.image.width - 50) 50 (model.image.height - 50)
      )

    ScrambledPositions newPositions ->
      let
        changePiecePosition ind point =
          case A.get ind model.pieces of
            Just piece ->
              { piece | position = Point.sub point piece.offset }
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
      let
        selectedPiece =
          case A.get 0 (A.filter .selected model.pieces) of
            Nothing -> defaultPiece
            Just piece -> { piece | selected = False }

        neighbourDistance neighbour =
          ( Point.dist selectedPiece.position neighbour.position
          , neighbour)

        neighbourFromId id =
          case A.get id model.pieces of
            Nothing -> defaultPiece
            Just piece -> piece

        distances =
          List.map (neighbourDistance << neighbourFromId) selectedPiece.neighbours

        takeFirst condition list =
          case list of
            [] ->
              Nothing
            x :: xs ->
              if condition x then
                Just x
              else
                takeFirst condition xs

        smallEnough (distance, _) =
          distance < 15.0

        maybeMoveSelectedPiece =
          case takeFirst smallEnough distances of
            Nothing ->
              selectedPiece
            Just (_, neighbour) ->
              { selectedPiece | position = neighbour.position, selected = False }

        pieces =
          A.map turnOffSelectedStatus model.pieces

        movedPieces =
          A.set selectedPiece.id maybeMoveSelectedPiece pieces
      in
        ( { model
            | cursor = Nothing
            , pieces = movedPieces
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
      Svg.defs [] ( definePuzzleImage model.image :: definePieceClipPaths model )

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
          , Svg.Attributes.clipPath <| clipPathRef piece.id
          ]
          []
        ]

    selectedPiece =
      case A.get 0 (A.filter .selected model.pieces) of
        Nothing -> defaultPiece
        Just piece -> piece

  in
  Html.div [ ]
    [ Html.h1 [] [ Html.text ( "Kitten jigsaw! " ) ]
    , Html.button [ Html.Events.onClick Scramble ] [ Html.text "scramble" ]
    , Html.h1 []
      [ Html.text
        <| String.fromInt selectedPiece.id
        ++ ", x: "
        ++ String.fromInt selectedPiece.position.x
        ++ ", y: "
        ++ String.fromInt selectedPiece.position.y
      ]
    , Html.div
        [ Html.Attributes.style "background-color" "#222222"
        , Html.Attributes.style "width" <| String.fromInt model.width ++ "px"
        , Html.Attributes.style "height" <| String.fromInt model.height ++ "px"
        ]
        [ Svg.svg
          ( svgAttributes model )
          ( definitions :: pieces )
        ]
    ]


svgAttributes model =
  let
    attributes =
      [ Svg.Attributes.width "100%"
      , Svg.Attributes.height "100%"
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


definePuzzleImage : JigsawImage -> Svg Msg
definePuzzleImage image =
  Svg.image
    [ Svg.Attributes.id "puzzle-image"
    , Svg.Attributes.xlinkHref image.path
    ]
    []


definePieceClipPaths : Model -> List (Svg Msg)
definePieceClipPaths model =
    List.map (pieceClipPath model.image) (A.toList model.pieces)

pieceClipPath : JigsawImage -> Piece -> Svg Msg
pieceClipPath image piece =
    let
      w = image.width // image.xpieces
      h = image.height // image.ypieces
      px num =
        String.fromInt num ++ "px"
    in
      Svg.clipPath [ Svg.Attributes.id <| pieceClipId piece.id ]
        [ Svg.rect
          [ Svg.Attributes.id <| pieceOutlineId piece.id
          , Svg.Attributes.width <| px w
          , Svg.Attributes.height <| px h
          , Svg.Attributes.x <| px piece.offset.x
          , Svg.Attributes.y <| px piece.offset.y
          ]
          []
      ]

pieceOutlineId : Int -> String
pieceOutlineId id =
    "piece-" ++ String.fromInt id ++ "-outline"

pieceClipId : Int -> String
pieceClipId id =
    "piece-" ++ String.fromInt id ++ "-clip"

clipPathRef : Int -> String
clipPathRef id =
    "url(#" ++ pieceClipId id ++ ")"

