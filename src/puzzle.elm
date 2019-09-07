module Main exposing (main)

import Browser
import Array as A
import Html exposing (Html, div, h1, text, button, text)
import Html.Events exposing (onClick)
import Svg.Attributes exposing (id)
import TypedSvg exposing (image, defs, svg, g, use, clipPath, rect)
import TypedSvg.Attributes exposing (width, height, xlinkHref)
import TypedSvg.Types exposing (num, px, ClipPath(..))
import TypedSvg.Core exposing (Svg)

-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Point =
    { x : Int
    , y : Int
    }

type alias Piece =
    { id : Int
    , offset : Point
    , position : Point
    , selected : Bool
    }

type alias Model =
    { pieces : A.Array Piece }


init : () -> (Model, Cmd Msg)
init () =
    ( Model <| A.fromList
        [ Piece 0 (Point 0 0) (Point 0 0) False
        , Piece 1 (Point 0 100) (Point 0 0) False
        , Piece 2 (Point 100 0) (Point 0 0) False
        , Piece 3 (Point 100 100) (Point 0 0) False
        ]
    , Cmd.none )


-- UPDATE

type Msg
  = MouseDown Int
  | MouseUp Int
  | MouseMove Int Int
  | Scramble Int

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    MouseDown id ->
      ( model, Cmd.none )
    MouseUp id ->
      ( model, Cmd.none )
    MouseMove x y ->
      ( model, Cmd.none )
    Scramble id ->
      ( { model | pieces = updatePiece id model.pieces}
      , Cmd.none )


updatePiece : Int -> A.Array Piece -> A.Array Piece
updatePiece id pieces =
    case A.get id pieces of
        Just p ->
            A.set id ({p | position = Point 200 0}) pieces
        Nothing ->
            pieces


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick (Scramble 1) ] [ text "foo" ]
    , h1 [] [ text ( "Test!" ) ]

    , svg
          [ width <| num 1000
          , height <| num 1000
          ]
          ( constructJigsawSvg model )

    ]

constructJigsawSvg : Model -> List (Svg msg)
constructJigsawSvg model =
    defs [] ( defineJigsawSvg model ) :: constructSvgPieces model.pieces

constructSvgPieces : A.Array Piece -> List (Svg msg)
constructSvgPieces pieces =
    List.map pieceToSvg (A.toList pieces)

pieceToSvg : Piece -> Svg msg
pieceToSvg piece =
    g []
        [ use
            [ TypedSvg.Attributes.x <| px <| toFloat piece.position.x
            , TypedSvg.Attributes.y <| px <| toFloat piece.position.y
            , xlinkHref <| "#puzzle-image"
            , TypedSvg.Attributes.clipPath <| clipPathRef piece
            ]
            []
        ]


defineJigsawSvg : Model -> List (Svg msg)
defineJigsawSvg model =
    definePuzzleImage :: definePieceClipPaths model

definePuzzleImage : Svg msg
definePuzzleImage =
  image
    [ id "puzzle-image"
    , xlinkHref "../resources/kitten.png"
    ]
    []

definePieceClipPaths : Model -> List (Svg msg)
definePieceClipPaths model =
    List.map pieceClipPath (A.toList model.pieces)

pieceClipPath : Piece -> Svg msg
pieceClipPath piece =
    clipPath [ id <| pieceClipId piece ]
        [ rect
            [ id <| pieceOutlineId piece
            , width <| px 100
            , height <| px 100
            , TypedSvg.Attributes.x <| px <| toFloat <| piece.offset.x
            , TypedSvg.Attributes.y <| px <| toFloat <| piece.offset.y
            ]
            []
        ]

pieceOutlineId : Piece -> String
pieceOutlineId piece =
    "piece-" ++ String.fromInt piece.id ++ "-outline"

pieceClipId : Piece -> String
pieceClipId piece =
    "piece-" ++ String.fromInt piece.id ++ "-clip"

clipPathRef : Piece -> ClipPath
clipPathRef piece =
    ClipPathFunc <| "url(#" ++ pieceClipId piece ++ ")"
