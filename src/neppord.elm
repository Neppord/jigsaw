-- Code borrowed from https://github.com/Neppord/puzzle

module Main exposing (main)

import Html exposing (Html, div, h1, text)
import Browser exposing (element)
import Color exposing (black, red)
import Platform.Sub exposing (Sub)
import Svg.Attributes exposing (id)
import TypedSvg exposing (circle, clipPath, defs, g, rect, svg, use, image)
import TypedSvg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, width, x, xlinkHref, y)
import TypedSvg.Types exposing (ClipPath(..), Fill(..), num, px)
import TypedSvg.Events exposing (onMouseDown)

import Json.Decode as D

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { xpos : Int
  , ypos : Int
  , mouseDown : Bool
  }

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

init : () -> (Model, Cmd Msg)
init () =
    ( ( Model 0 0 False), Cmd.none )


type Msg
  = MousePosition Int Int
  | MouseDown Bool

view : Model -> Html Msg
view model =
    div []
    [
      svg
          [ width <| num 800
          , height <| num 600
          ]
          [ defs
              []
              [ puzzleImage
              , pieceClipPath 0 0
              , pieceClipPath 1 0
              , pieceClipPath 0 1
              , pieceClipPath 1 1
              ]
          , piece 0 0
          , piece 1 0
          , piece 0 1
          , piece 1 1
          ]
    , h1 [] [ text ( "(" ++ String.fromInt model.xpos ++ ", " ++ String.fromInt model.ypos ++ ")")]
    ]

piece x y =
    g [ TypedSvg.Events.onMouseDown <| MousePosition x y ]
        [ use
            [ TypedSvg.Attributes.x <| px <| toFloat x
            , TypedSvg.Attributes.y <| px <| toFloat y
            , xlinkHref <| "#puzzle-image"
            , TypedSvg.Attributes.clipPath <| clipPathRef x y
            ]
            []
        , use
            [ TypedSvg.Attributes.x <| px <| toFloat x
            , TypedSvg.Attributes.y <| px <| toFloat y
            , xlinkHref <| "#" ++ pieceOutlineId x y
            , fill FillNone
            , stroke <| black
            , strokeWidth <| px 3
            ]
            []
        ]


clipPathRef x y =
    ClipPathFunc <| "url(#" ++ pieceClipId x y ++ ")"


pieceClipPath x y =
    clipPath [ id <| pieceClipId x y ]
        [ rect
            [ id <| pieceOutlineId x y
            , width <| px 100
            , height <| px 100
            , TypedSvg.Attributes.x <| px <| toFloat <| x * 100
            , TypedSvg.Attributes.y <| px <| toFloat <| y * 100
            ]
            []
        ]


pieceClipId x y =
    "piece-" ++ String.fromInt x ++ "-" ++ String.fromInt y ++ "-clip"


pieceOutlineId x y =
    "piece-" ++ String.fromInt x ++ "-" ++ String.fromInt y ++ "-outline"


puzzleImage =
  image
    [ id "puzzle-image"
    , xlinkHref "../resources/kitten.png"
    ]
    []
--    circle
--        [ id "puzzle-image"
--        , cx <| px 100
--        , cy <| px 100
--        , r <| px 100
--        , fill <| Fill red
--        ]
--        []


--update _ model =
--    ( model, Cmd.none )

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    MousePosition x y ->
      ( { model | xpos = x, ypos = y }
       , Cmd.none )
    MouseDown _ ->
      ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
--   onMouseDown (D.succeed <| MouseDown True)
  Sub.none