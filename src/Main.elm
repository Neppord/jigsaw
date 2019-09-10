module Main exposing (..)

import Browser
import Set as S
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
--  , pieces : A.Array Piece
  , pieceGroups : D.Dict Int PieceGroup
  , selectedId : Int
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

--type alias Piece =
--  { position : Point
--  , selected : Bool
--  , zlevel : Int
--  , id : Int
--  , neighbours : List Int
--  }

type alias PieceGroup =
  { id : Int
  , members : List Int
  , neighbours : S.Set Int
  , position : Point
  , selected : Bool
  , zlevel : Int
  }


-- Until I figure out how to handle index out of bounds
-- exceptions more elegantly
defaultPieceGroup : PieceGroup
defaultPieceGroup =
  { position = Point 0 0
  , selected = False
  , zlevel = -1
  , id = -1
  , neighbours = S.empty
  , members = []
  }


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
--      , pieces = createPieces image
      , pieceGroups = createPieceGroups image
      , selectedId = -1
      , maxZLevel = image.xpieces * image.ypieces - 1
      , image = image
      , width = 1200
      , height = 850
      }
  in
  ( model, Cmd.none )


createPieceGroups : JigsawImage -> D.Dict Int PieceGroup
createPieceGroups image =
  let
    nx = image.xpieces
    ny = image.ypieces
    n = nx*ny
    range =
      List.range 0 (n - 1)
    neighbourOffsets =
      [ -nx, -1, 1, nx ]
    possibleNeighbours i =
      List.map ((+) i) neighbourOffsets
    isRealNeighbour i x =
      Point.taxiDist
        ( pieceIdToPoint i image.xpieces )
        ( pieceIdToPoint x image.xpieces ) == 1
    onePieceGroup i =
      ( i
      , { position = Point 0 0
        , selected = False
        , id = i
        , zlevel = i
        , members = [ i ]
        , neighbours = S.filter (isRealNeighbour i) <| S.fromList (possibleNeighbours i)
        }
      )

  in
    D.fromList <| List.map onePieceGroup range


pieceIdToPoint : Int -> Int -> Point
pieceIdToPoint id xpieces =
  Point (modBy xpieces id) (id // xpieces)

pieceIdToOffset : Int -> JigsawImage -> Point
pieceIdToOffset id image =
  let
    pieceWidth = image.width // image.xpieces
    pieceHeight = image.height // image.ypieces
  in
    Point.dot
      ( pieceIdToPoint id image.xpieces )
      ( Point pieceWidth pieceHeight )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    turnOffSelectedStatus : PieceGroup -> PieceGroup
    turnOffSelectedStatus pg =
      { pg | selected = False }

  in
  case msg of
    Scramble ->
      let
        n = D.size model.pieceGroups
        xmin = 0
        xmax = model.width - 50
        ymin = 0
        ymax = model.height - 50
        scrambleCommand =
          Random.generate ScrambledPositions
            <| Point.randomPoints n xmin xmax ymin ymax
      in
        ( model, scrambleCommand )

    ScrambledPositions newPositions ->
      let
        updatePieceGroup : PieceGroup -> Point -> (Int, PieceGroup)
        updatePieceGroup pg pos =
          (pg.id, { pg | position = Point.sub pos (pieceIdToOffset pg.id model.image) } )

        updatePositions : List PieceGroup -> List Point -> D.Dict Int PieceGroup
        updatePositions pieceGroups positions =
          D.fromList <| List.map2 updatePieceGroup pieceGroups positions

        updatedPieceGroups =
          updatePositions (D.values model.pieceGroups) newPositions
      in
        ( { model | pieceGroups = updatedPieceGroups }, Cmd.none )

    MouseDown id coordinate ->
      let
        alter : Maybe PieceGroup -> Maybe PieceGroup
        alter pieceGroup =
          case pieceGroup of
            Nothing -> Nothing
            Just pg -> Just {pg | selected = True, zlevel = model.maxZLevel}

      in
        ( { model
            | cursor = Just coordinate
            , pieceGroups = D.update id alter model.pieceGroups
            , maxZLevel = model.maxZLevel + 1
            , selectedId = id
          }
        , Cmd.none
        )

    MouseUp ->
      let
        neighbourDistance : PieceGroup -> PieceGroup -> (Float, PieceGroup)
        neighbourDistance selectedPiece neighbour =
          ( Point.dist selectedPiece.position neighbour.position
          , neighbour)

        neighbourFromId : Int -> PieceGroup
        neighbourFromId id =
          Maybe.withDefault defaultPieceGroup
            <| D.get id model.pieceGroups

        distances : PieceGroup -> List (Float, PieceGroup)
        distances selectedPiece =
          List.map ((neighbourDistance selectedPiece) << neighbourFromId) (S.toList selectedPiece.neighbours)

        takeFirst : (a -> Bool) -> List a -> Maybe a
        takeFirst condition list =
          case list of
            [] ->
              Nothing
            x :: xs ->
              if condition x then
                Just x
              else
                takeFirst condition xs

        smallEnough : (Float, a) -> Bool
        smallEnough (distance, _) =
          distance < 15.0

        maybeMoveSelectedPiece : Maybe PieceGroup -> Maybe PieceGroup
        maybeMoveSelectedPiece pg =
          case pg of
            Nothing -> Nothing
            Just selectedPiece ->
              case takeFirst smallEnough (distances selectedPiece) of
                Nothing ->
                  Just { selectedPiece | selected = False }
                Just (_, neighbour) ->
                  Just { selectedPiece | position = neighbour.position, selected = False }

        updatedPieceGroups =
          D.update model.selectedId (maybeMoveSelectedPiece) model.pieceGroups

    ---------------------------

        closeNeighbour : PieceGroup -> Maybe PieceGroup
        closeNeighbour selected =
          case takeFirst smallEnough (distances selected) of
            Nothing -> Nothing
            Just (_, neighbour) -> Just neighbour

        merge : PieceGroup -> PieceGroup -> PieceGroup
        merge a b =
          let
            newMembers = b.members ++ a.members
            newNeighbours = S.diff (S.union b.neighbours a.neighbours) (S.fromList newMembers)
          in
            { b
              | selected = False
              , members = newMembers
              , neighbours = newNeighbours}

        updatedPieceGroups2 : PieceGroup -> D.Dict Int PieceGroup
        updatedPieceGroups2 selected =
          case closeNeighbour selected of
            Just neighbour ->
              D.remove selected.id <| D.insert neighbour.id (merge selected neighbour) model.pieceGroups
            Nothing ->
              D.insert selected.id { selected | selected = False } model.pieceGroups

      in
        ( { model
            | cursor = Nothing
            , pieceGroups = updatedPieceGroups2
                <| Maybe.withDefault defaultPieceGroup
                <| D.get model.selectedId model.pieceGroups
            , selectedId = -1
          }
        , Cmd.none
        )

    MouseMove newPos ->
      let
        movePieceGroup : Point -> Int -> PieceGroup -> PieceGroup
        movePieceGroup pos _ pg =
          if pg.selected then
            { pg | position = Point.add pg.position pos}
          else
            pg

      in
        case model.cursor of
          Nothing ->
            ( model, Cmd.none )

          Just oldPos ->
            ( { model
                | cursor = Just newPos
                , pieceGroups = D.map (movePieceGroup <| Point.sub newPos oldPos) model.pieceGroups
              }
            , Cmd.none
            )



-- VIEW

view : Model -> Html Msg
view model =
  let
    definitions =
      Svg.defs [] ( definePuzzleImage model.image :: definePieceClipPaths model.image )

    pieces =
      List.map svgPieceGroup
        <| List.sortBy .zlevel
        <| D.values model.pieceGroups

    svgPieceGroup pg =
      Svg.g
        [ onMouseDown pg.id
        , translate pg.position
        ]
        ( List.concat <| List.map (svgPiece pg.selected) pg.members )

    svgPiece selected id =
        [ Svg.use
          [ Svg.Attributes.xlinkHref <| "#puzzle-image"
          , Svg.Attributes.clipPath <| clipPathRef id
          ]
          []
        , Svg.use
          [ Svg.Attributes.xlinkHref <| "#" ++ pieceOutlineId id
          , Svg.Attributes.fill "white"
          , Svg.Attributes.fillOpacity "0.0"
          , Svg.Attributes.stroke <| if selected then "green" else "black"
          , Svg.Attributes.strokeWidth "2px"
          ]
          []
        ]

  in
  Html.div [ ]
    [ Html.h1 [] [ Html.text ( "Kitten jigsaw! " ) ]
    , Html.button [ Html.Events.onClick Scramble ] [ Html.text "scramble" ]
    , Html.div
        [ Html.Attributes.style "background-color" "#CCCCCC"
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
    , Svg.Attributes.pointerEvents "none"
    ]
    []


definePieceClipPaths : JigsawImage -> List (Svg Msg)
definePieceClipPaths image =
  List.map (pieceClipPath image) (List.range 0 (image.xpieces * image.ypieces - 1))

pieceClipPath : JigsawImage -> Int -> Svg Msg
pieceClipPath image id =
  let
    w = image.width // image.xpieces
    h = image.height // image.ypieces
    offset = pieceIdToOffset id image
    px num =
      String.fromInt num ++ "px"
  in
    Svg.clipPath [ Svg.Attributes.id <| pieceClipId id ]
      [ Svg.rect
        [ Svg.Attributes.id <| pieceOutlineId id
        , Svg.Attributes.width <| px w
        , Svg.Attributes.height <| px h
        , Svg.Attributes.x <| px offset.x
        , Svg.Attributes.y <| px offset.y
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

