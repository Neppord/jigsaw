module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text)
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
    { pieces : List Piece }



init : () -> (Model, Cmd Msg)
init () =
    ( Model
        [ Piece 0 (Point 0 0) (Point 0 0) False
        , Piece 1 (Point 0 1) (Point 0 1) False
        , Piece 2 (Point 1 0) (Point 1 0) False
        , Piece 3 (Point 1 1) (Point 1 1) False
        ]
    , Cmd.none )


-- UPDATE

type Msg
  = MouseDown Int
  | MouseUp Int
  | MouseMove Int Int

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    MouseDown id ->
      ( model, Cmd.none )
    MouseUp id ->
      ( model, Cmd.none )
    MouseMove x y ->
      ( model, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ( "Test!" ) ]
    , svg
          [ width <| num 1000
          , height <| num 1000
          ]
          ( constructJigsawSvg model )
    ]

constructJigsawSvg : Model -> List (Svg msg)
constructJigsawSvg model =
    defs [] ( defineJigsawSvg model ) :: constructSvgPieces model.pieces

constructSvgPieces : List Piece -> List (Svg msg)
constructSvgPieces pieces =
    List.map pieceToSvg pieces

pieceToSvg : Piece -> Svg msg
pieceToSvg piece =
--  piece p.position.x p.position.y
--
--piece : Int -> Int -> Svg msg
--piece x y =
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
    List.map pieceClipPath model.pieces

pieceClipPath : Piece -> Svg msg
pieceClipPath piece =
    clipPath [ id <| pieceClipId piece ]
        [ rect
            [ id <| pieceOutlineId piece
            , width <| px 100
            , height <| px 100
            , TypedSvg.Attributes.x <| px <| toFloat <| piece.position.x * 100
            , TypedSvg.Attributes.y <| px <| toFloat <| piece.position.y * 100
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
