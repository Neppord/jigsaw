module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text)
import Svg.Attributes exposing (id)
import TypedSvg exposing (image, defs, svg, g, use)
import TypedSvg.Attributes exposing (width, height, xlinkHref)
import TypedSvg.Types exposing (num, px)
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
        , Piece 1 (Point 0 0) (Point 0 0) False
        , Piece 2 (Point 0 0) (Point 0 0) False
        , Piece 3 (Point 0 0) (Point 0 0) False
        ]
    , Cmd.none )


puzzleImage =
  image
    [ id "puzzle-image"
    , xlinkHref "../resources/kitten.png"
    ]
    []

piece : Svg msg
piece =
    g []
        [ use
            [ TypedSvg.Attributes.x <| px <| toFloat 100
            , TypedSvg.Attributes.y <| px <| toFloat 0
            , xlinkHref <| "#puzzle-image"
            --, TypedSvg.Attributes.clipPath <| clipPathRef x y
            ]
            []
        ]


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
          [ defs
              []
              [ puzzleImage ]
            , piece
          ]
    ]
