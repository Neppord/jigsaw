import Html exposing (Html, button, div, text)
import Html.Events
import Json.Decode
import Svg exposing (svg, rect)
import Svg.Attributes exposing (..)
import Mouse

main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- MODEL

type alias Model = 
  { drag : Maybe Drag
  , pos : Position
  }

init : (Model, Cmd Msg)
init = ( Model Nothing (Position 0 0), Cmd.none )

type alias Position =
  { x : Int
  , y : Int
  }

type alias Drag = 
  { startPos : Position
  , currentPos : Position
  }

-- UPDATE

type Msg = 
  Press Position |
  Release Position |
  Move Position

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Press pos ->
      ({model | drag = Just (Drag pos pos)}, Cmd.none)

    Release pos ->
      let
        newPos = getPosition model
      in
          
      ( { model | pos = newPos, drag = Nothing }, Cmd.none )

    Move pos ->
      let
        newDrag = case model.drag of
          Nothing ->    -- Should not happen
            model.drag
          Just drag ->
            Just { drag | currentPos = pos }
          
      in
        ( { model | drag = newDrag}, Cmd.none )



-- VIEW

view : Model -> Html Msg
view model =

  let 
    pos = getPosition model

  in
    div []
      [ svg
          [ width "400"
          , height "100"
          , viewBox "0 0 400 100"
          ]
          [ rect
              [ customOnMouseDown 
              , id "C"
              , width "100"
              , height "050"
              , x ( toString pos.x )
              , y ( toString pos.y )
              , fill "#000"
              , cursor "pointer"
              ]
              []
          ]
      ]

customOnMouseDown : Html.Attribute Msg
customOnMouseDown =
  let
    
    decoder = 
      Json.Decode.oneOf
      [ Json.Decode.map 
        Press 
        ( Json.Decode.map2
            Position 
              ( Json.Decode.field "pageX" Json.Decode.int) 
              ( Json.Decode.field "pageY"   Json.Decode.int) 
        )
      , Json.Decode.succeed (Press ( Position 500 500 ))
      ]  
      
  in
      
    Html.Events.on "mousedown" decoder


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves Move, Mouse.ups Release ]


 -- HELPERS

getPosition : Model -> Position
getPosition {drag, pos} =
  case drag of 
    Nothing ->
      pos

    Just { startPos, currentPos } ->
      Position
      ( pos.x + currentPos.x - startPos.x )
      ( pos.y + currentPos.y - startPos.y )