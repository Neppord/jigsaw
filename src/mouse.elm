import Browser
import Browser.Events as E
import Json.Decode as D
import Html exposing (..)
import Task
import Time



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { xpos : Int
  , ypos : Int
  , mouseDown : Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0 0 False
  , Cmd.none
  )



-- UPDATE


type Msg
  = UpdateMouse Int Int
  | MouseDown Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateMouse x y ->
      ( { model | xpos = x, ypos = y }
      , Cmd.none
      )

    MouseDown isDown ->
      ( { model | mouseDown = isDown }
      , Cmd.none
      )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if model.mouseDown then mousePositionSubscription else Sub.none
    , E.onMouseDown (D.succeed <| MouseDown True)
    , E.onMouseUp (D.succeed <| MouseDown False)
    ]

mousePositionSubscription : Sub Msg
mousePositionSubscription =
  E.onMouseMove (D.map2 UpdateMouse (D.field "pageX" D.int) (D.field "pageY" D.int))





-- VIEW


view : Model -> Html Msg
view model =
  let
    x = String.fromInt model.xpos
    y = String.fromInt model.ypos
    mouseDown = if model.mouseDown then "True" else "False"
  in
  div []
    [ h1 [] [ text ( "Mouse x: " ++ x ) ]
    , h1 [] [ text ( "Mouse y: " ++ y ) ]
    , h1 [] [ text ( "Mouse down: " ++ mouseDown ) ]
    ]