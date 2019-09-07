import Browser
import Html exposing (Html, Attribute, div, input, text, img)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


import Svg.Attributes exposing (id)
import TypedSvg exposing (circle, clipPath, defs, g, rect, svg, use)
import TypedSvg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, width, x, xlinkHref, y)
import TypedSvg.Types exposing (ClipPath(..), Fill(..), num, px)


import Html exposing (Html)
import Color
import TypedSvg exposing (svg, circle, image)
import TypedSvg.Attributes exposing (viewBox, cx, cy, r, fill, strokeWidth, stroke)
import TypedSvg.Types exposing (Fill(..), px)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
    , div [] [ text (String.reverse model.content) ]
    --, img [ src "../resources/kitten.png" ] []
    , svg
        [ viewBox -200 -200 1024 800
        , width <| num 1024
        , height <| num 800
        ]
        [ image [ xlinkHref "../resources/kitten.png" ] []
        ]
--        [ circle
--            [ cx (px 150)
--            , cy (px 150)
--            , r (px 30)
--            , fill <| Fill Color.black
--            , strokeWidth (px 2)
--            , stroke <| Color.rgba 90 60 60 0.5
--            ]
--            []
--        ]

    ]

