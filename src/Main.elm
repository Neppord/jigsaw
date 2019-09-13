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
import Util exposing (takeFirst)

-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }


-- MODEL

type Msg
  = MouseDown Int Point
  | MouseMove Point
  | MouseUp
  | Scramble
  | ScrambledPositions (List Point, List Int)

type alias Model =
  { cursor : Maybe Point
  , pieceGroups : D.Dict Int PieceGroup
  , selected : Selected
  , maxZLevel : Int
  , image : JigsawImage
  , width : Int
  , height : Int
  , snapDistance : Float
  , selectionBox : SelectionBox
  , debug : String
  }

type alias JigsawImage =
    { path : String
    , width : Int
    , height : Int
    , xpieces : Int
    , ypieces : Int
    }

type alias PieceGroup =
  { id : Int
  , members : List Int
  , neighbours : S.Set Int
  , position : Point
  , isSelected : Bool
  , zlevel : Int
  }

type SelectionBox
  = Normal Box
  | Inverted Box
  | NullBox

type alias Box =
  { staticCorner : Point
  , movingCorner : Point
  }

type Selected
  = Multiple
  | Single Int
  | NullSelection


boxTopLeft : Box -> Point
boxTopLeft box =
  Point
    (min box.staticCorner.x box.movingCorner.x)
    (min box.staticCorner.y box.movingCorner.y)

boxBottomRight : Box -> Point
boxBottomRight box =
  Point
    (max box.staticCorner.x box.movingCorner.x)
    (max box.staticCorner.y box.movingCorner.y)

-- Until I figure out how to handle index out of bounds
-- exceptions more elegantly
defaultPieceGroup : PieceGroup
defaultPieceGroup =
  { position = Point 0 0
  , isSelected = False
  , zlevel = -1
  , id = -10
  , neighbours = S.empty
  , members = []
  }


-- INIT

init : () -> ( Model, Cmd Msg )
init () =
  let
    image =
      { path = "resources/kitten.png"
      , width = 533
      , height = 538
      , xpieces = 6
      , ypieces = 6
      }
    model =
      resetModel image [] []
  in
  ( model, Cmd.none )


resetModel : JigsawImage -> List Point -> List Int -> Model
resetModel image positions zlevels =
  { cursor = Nothing
  , pieceGroups = createPieceGroups image positions zlevels
  , selected = NullSelection
  , maxZLevel = image.xpieces * image.ypieces
  , image = image
  , width = 2000
  , height = 1000
  , snapDistance = 30.0
  , selectionBox = NullBox
  , debug = "Nothing to see here..."
  }

createPieceGroups : JigsawImage -> List Point -> List Int -> D.Dict Int PieceGroup
createPieceGroups image points levels =
  let
    nx = image.xpieces
    ny = image.ypieces
    n = nx*ny

    range =
      List.range 0 (n - 1)
    positions =
      if List.length points < n then
        List.map (pieceIdToOffset image) range
      else
        points
    zlevels =
      if List.length levels < n then
        range
      else
        levels
    neighbourOffsets =
      [ -nx, -1, 1, nx ]
    possibleNeighbours i =
      List.map ((+) i) neighbourOffsets
    isRealNeighbour i x =
       x >= 0 && x < n &&
      Point.taxiDist
        ( pieceIdToPoint i image.xpieces )
        ( pieceIdToPoint x image.xpieces ) == 1
    onePieceGroup i pos zlevel =
      ( i
      , { position = Point.sub pos (pieceIdToOffset image i)
        , isSelected = False
        , id = i
        , zlevel = zlevel
        , members = [ i ]
        , neighbours = S.filter (isRealNeighbour i) <| S.fromList (possibleNeighbours i)
        }
      )

  in
    D.fromList <| List.map3 onePieceGroup range positions zlevels


pieceIdToPoint : Int -> Int -> Point
pieceIdToPoint id xpieces =
  Point (modBy xpieces id) (id // xpieces)

pieceIdToOffset : JigsawImage -> Int -> Point
pieceIdToOffset image id =
  let
    pieceWidth = image.width // image.xpieces
    pieceHeight = image.height // image.ypieces
  in
    Point.dot
      ( pieceIdToPoint id image.xpieces )
      ( Point pieceWidth pieceHeight )


isPieceInsideBox : JigsawImage -> Point -> Point -> Point -> Int -> Bool
isPieceInsideBox image pos boxTL boxBR id =
  let
    pieceWidth = image.width // image.xpieces
    pieceHeight = image.height // image.ypieces
    pieceTL = Point.add pos <| pieceIdToOffset image id
    pieceBR = Point.add pieceTL <| Point pieceWidth pieceHeight
  in
    ( pieceTL.x < boxBR.x ) &&
    ( pieceTL.y < boxBR.y ) &&
    ( pieceBR.x > boxTL.x ) &&
    ( pieceBR.y > boxTL.y )

isPieceGroupInsideBox : JigsawImage -> Point -> Point -> PieceGroup -> Bool
isPieceGroupInsideBox image boxTL boxBR pieceGroup =
  List.any (isPieceInsideBox image pieceGroup.position boxTL boxBR) pieceGroup.members

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Scramble ->
      let
        n = model.image.xpieces * model.image.ypieces
        xmin = 0
        xmax = model.width - model.image.width // model.image.xpieces
        ymin = 0
        ymax = model.height - model.image.height // model.image.ypieces
        scrambleCommand =
          Random.generate ScrambledPositions
            <| Point.randomPointsAndZ n xmin xmax ymin ymax
      in
        ( model, scrambleCommand )

    ScrambledPositions (newPositions, zlevels) ->
      ( resetModel model.image newPositions zlevels, Cmd.none )

    MouseDown id coordinate ->
      let
        clickedOnBackground = id == -1
        newModel =
          if clickedOnBackground then
            startSelectionBox model coordinate
          else
            selectPieceGroup model id coordinate
      in
        ( newModel, Cmd.none )

    MouseUp ->
      let
        newModel =
          case model.selectionBox of
          Normal box ->
            { model
            | selectionBox = NullBox
            , cursor = Nothing
            , selected = currentSelection model.pieceGroups
            }
          Inverted box ->
            { model
            | selectionBox = NullBox
            , cursor = Nothing
            , selected = currentSelection model.pieceGroups
            }
          NullBox ->
            case model.selected of
              Multiple ->
                { model | cursor = Nothing }
              NullSelection ->
                { model | cursor = Nothing }
              Single id ->
                { model
                  | cursor = Nothing
                  , selected = NullSelection
                  , pieceGroups =
                      D.get id model.pieceGroups
                      |> Maybe.withDefault defaultPieceGroup
                      |> snapToNeighbour model
                }
      in
        ( newModel, Cmd.none )

    MouseMove newPos ->
      case model.cursor of
        Nothing ->
          ( model, Cmd.none )

        Just oldPos ->
          case model.selectionBox of
            NullBox ->
              let
                movePieceGroup : Int -> PieceGroup -> PieceGroup
                movePieceGroup _ pg =
                  if pg.isSelected then
                    { pg | position = Point.add pg.position <| Point.sub newPos oldPos}
                  else
                    pg
                updatedModel =
                  { model
                  | cursor = Just newPos
                  , pieceGroups = D.map movePieceGroup model.pieceGroups
                  }
              in
                ( updatedModel, Cmd.none )

            Normal box ->
              let
                tl = boxTopLeft box
                br = boxBottomRight box
                selectPiece _ pg =
                  if isPieceGroupInsideBox model.image tl br pg then
                    {pg | isSelected = True}
                  else
                    {pg | isSelected = False}
                updatedPieceGroups =
                  D.map selectPiece model.pieceGroups
              in
              ( { model
                | selectionBox = Normal {box | movingCorner = newPos}
                , pieceGroups = updatedPieceGroups
                }
              , Cmd.none )
            Inverted box ->
              ( model, Cmd.none )



selectPieceGroup : Model -> Int -> Point -> Model
selectPieceGroup model id coordinate =
  let
    selectedBefore =
      D.get id model.pieceGroups
        |> Maybe.withDefault defaultPieceGroup
        |> .isSelected

    setZlevel pieceGroup =
      case pieceGroup of
        Nothing -> Nothing
        Just pg -> Just {pg | zlevel = model.maxZLevel}

    fixZlevels pieceGroups =
      D.update id setZlevel pieceGroups

    startDragging =
      selectedBefore && model.selected == Multiple

    deselectAll pieceGroups =
      D.map (\key pg -> {pg | isSelected = key == id}) pieceGroups
  in
    { model
    | maxZLevel = model.maxZLevel + 1
    , cursor = Just coordinate
    , selected = if not selectedBefore then Single id else model.selected
    , pieceGroups =
        if startDragging then
          fixZlevels model.pieceGroups
        else
          deselectAll << fixZlevels <| model.pieceGroups
    }

startSelectionBox : Model -> Point -> Model
startSelectionBox model coordinate =
  { model
    | cursor = Just coordinate
    , selected = NullSelection
    , pieceGroups = D.map (\_ pg -> {pg | isSelected = False}) model.pieceGroups
    , selectionBox = Normal
      { staticCorner = coordinate
      , movingCorner = coordinate
      }
  }


snapToNeighbour : Model -> PieceGroup -> D.Dict Int PieceGroup
snapToNeighbour model selected =
  let
    neighbourDistance : PieceGroup -> PieceGroup -> (Float, PieceGroup)
    neighbourDistance selectedPiece neighbour =
      ( Point.dist selectedPiece.position neighbour.position
      , neighbour)

    neighbourFromId : Int -> PieceGroup
    neighbourFromId id =
      Maybe.withDefault defaultPieceGroup
        <| D.get id model.pieceGroups

    distanceToSelected : List (Float, PieceGroup)
    distanceToSelected =
      List.map ((neighbourDistance selected) << neighbourFromId) (S.toList selected.neighbours)

    smallEnough : (Float, a) -> Bool
    smallEnough (distance, _) =
      distance < model.snapDistance

    closeNeighbour : Maybe PieceGroup
    closeNeighbour =
      case takeFirst smallEnough distanceToSelected of
        Nothing -> Nothing
        Just (_, neighbour) -> Just neighbour

    merge : PieceGroup -> PieceGroup -> PieceGroup
    merge a b =
      let
        newMembers = b.members ++ a.members
        newNeighbours = S.diff (S.union b.neighbours a.neighbours) (S.fromList newMembers)
      in
        { b
          | isSelected = False
          , members = newMembers
          , neighbours = newNeighbours
          , zlevel = a.zlevel}
  in
  case closeNeighbour of
    Just neighbour ->
      let
        fixNeighbours : S.Set Int -> Int -> Int -> S.Set Int
        fixNeighbours neighbours wrong right =
          if S.member wrong neighbours then
            S.insert right <| S.remove wrong neighbours
          else
            neighbours

        replaceSelectedIdWithNeighbourId _ pg =
            {pg | neighbours = fixNeighbours pg.neighbours selected.id neighbour.id}

      in
        merge selected neighbour
          |> Util.flip (D.insert neighbour.id) model.pieceGroups
          |> D.remove selected.id
          |> D.map replaceSelectedIdWithNeighbourId

    Nothing ->
      D.insert selected.id { selected | isSelected = False } model.pieceGroups





allSelectedPieceGroups pieceGroups =
  D.filter (\_ pg -> pg.isSelected) pieceGroups

currentSelection : D.Dict Int PieceGroup -> Selected
currentSelection pieceGroups =
  case D.keys <| allSelectedPieceGroups pieceGroups of
    [] -> NullSelection
    id :: [] -> Single id
    _ -> Multiple

-- VIEW

view : Model -> Html Msg
view model =
  let
    definitions =
      Svg.defs [] ( definePuzzleImage model.image :: definePieceClipPaths model.image )

    background =
      Svg.rect
        [ Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.fill "blue"
        , Svg.Attributes.opacity "0.0"
        , onMouseDown -1
        ]
        []

    svgSelectionBox box =
      let
        topLeft = boxTopLeft box
        bottomRight = boxBottomRight box
      in
        Svg.rect
          [ Svg.Attributes.width <| String.fromInt (bottomRight.x - topLeft.x)
          , Svg.Attributes.height <| String.fromInt (bottomRight.y - topLeft.y)
          , Svg.Attributes.fill "blue"
          , Svg.Attributes.fillOpacity "0.2"
          , Svg.Attributes.stroke "darkblue"
          , Svg.Attributes.strokeWidth "2px"
          , Svg.Attributes.strokeOpacity "0.9"
          , translate (topLeft)
          ]
          []

    normalSelection =
      case model.selectionBox of
        Normal box ->
          [ svgSelectionBox box ]
        Inverted box -> []
        NullBox -> []

    pieces =
      List.concat
        <| List.map svgPieceGroup
        <| List.sortBy .zlevel
        <| D.values model.pieceGroups

    svgPieceGroup pg =
      List.map (svgMember pg.id pg.position pg.isSelected) pg.members

    svgMember groupId pos selected id =
      Svg.g [ onMouseDown groupId, translate pos ]
        <| [svgClipPath id] ++ [svgOutlines selected id]

    svgClipPath id =
        Svg.use
        [ Svg.Attributes.xlinkHref <| "#puzzle-image"
        , Svg.Attributes.clipPath <| clipPathRef id
        ]
        []

    svgOutlines selected id =
        Svg.use
        [ Svg.Attributes.xlinkHref <| "#" ++ pieceOutlineId id
        , Svg.Attributes.fill "white"
        , Svg.Attributes.fillOpacity "0.0"
        , Svg.Attributes.stroke <| if selected then "red" else "black"
        , Svg.Attributes.strokeWidth "2px"
        ]
        []

  in
  Html.div [ ]
    [ Html.h1 [] [ Html.text ( "Kitten jigsaw! " ) ]
    , Html.button [ Html.Events.onClick Scramble ] [ Html.text "scramble" ]
--    , Html.h1 [] [ Html.text model.debug ]
    , Html.div
        [ Html.Attributes.style "background-color" "#CCCCCC"
        , Html.Attributes.style "width" <| String.fromInt model.width ++ "px"
        , Html.Attributes.style "height" <| String.fromInt model.height ++ "px"
        ]
        [ Svg.svg
          ( svgAttributes model )
          ( definitions :: background :: pieces ++ normalSelection)
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
    (Json.Decode.field "offsetX" Json.Decode.int)
    (Json.Decode.field "offsetY" Json.Decode.int)

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
    offset = pieceIdToOffset image id
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

