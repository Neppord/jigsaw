module Main exposing (..)

import Set as S
import Dict as D
import Array as A
import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Random
import Random.List

import Point exposing (Point)
import Util exposing (takeFirst)
import Edge exposing (EdgePoints, makeEdgePoints)

-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL

type Msg
  = MouseDown Int Point Keyboard
  | MouseMove Point
  | MouseUp
  | Scramble
  | KeyChanged Bool Key

type alias Keyboard =
  { shift : Bool
  , ctrl : Bool
  }

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
  , seed : Random.Seed
  , edgePoints : A.Array EdgePoints
  , visibleGroups : S.Set Int
  , keyboard : Keyboard
  }

type alias JigsawImage =
  { path : String
  , width : Int
  , height : Int
  , xpieces : Int
  , ypieces : Int
  , scale : Float
  }

type alias PieceGroup =
  { id : Int
  , members : List Int
  , neighbours : S.Set Int
  , position : Point
  , isSelected : Bool
  , zlevel : Int
  , visibilityGroup : Int
  }

type SelectionBox
  = Normal Box
  | Inverted Box
  | NullBox

type alias Box =
  { staticCorner : Point
  , movingCorner : Point
  , selectedIds : S.Set Int
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
  , visibilityGroup = -1
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
      , scale = 1.0
      }
    model =
      resetModel image (Random.initialSeed 0)
  in
  ( model, Cmd.none )


resetModel : JigsawImage -> Random.Seed -> Model
resetModel image seed =
  let
    (w, h) = (1800, 1100)
    (nx, ny) = (image.xpieces, image.ypieces)
    numberOfEdges = 2 * nx * ny - nx - ny

    (positions, seed1) = shufflePiecePositions w h image seed
    (zlevels, seed2) = shuffleZLevels (nx * ny) seed1
    (edgePoints, seed3) = makeEdgePoints numberOfEdges seed2
  in
    { cursor = Nothing
    , pieceGroups = createPieceGroups image positions zlevels
    , selected = NullSelection
    , maxZLevel = nx * ny
    , image = image
    , width = w
    , height = h
    , snapDistance = 30.0
    , selectionBox = NullBox
    , debug = "Nothing to see here..."
    , seed = seed3
    , edgePoints = edgePoints
    , visibleGroups = S.fromList [-1]
    , keyboard = { shift = False, ctrl = False }
    }


shufflePiecePositions : Int -> Int -> JigsawImage -> Random.Seed -> (List Point, Random.Seed)
shufflePiecePositions w h image seed =
  let
    n = image.xpieces * image.ypieces
    xmin = 0
    xmax = w - image.width // image.xpieces
    ymin = 0
    ymax = h - image.height // image.ypieces
  in
    Random.step (Point.randomPoints n xmin xmax ymin ymax) seed

shuffleZLevels : Int -> Random.Seed -> (List Int, Random.Seed)
shuffleZLevels n seed =
  Random.step (Random.List.shuffle <| List.range 0 (n - 1)) seed

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
        , visibilityGroup = -1
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
    pieceWidth =  floor <| image.scale * toFloat (image.width // image.xpieces)
    pieceHeight = floor <| image.scale * toFloat (image.height // image.ypieces)
  in
    Point.dot
      ( pieceIdToPoint id image.xpieces )
      ( Point pieceWidth pieceHeight )


isPieceInsideBox : JigsawImage -> Point -> Point -> Point -> Int -> Bool
isPieceInsideBox image pos boxTL boxBR id =
  let
    pieceWidth =  floor <| image.scale * toFloat (image.width // image.xpieces)
    pieceHeight = floor <| image.scale * toFloat (image.height // image.ypieces)
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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ E.onKeyDown (Json.Decode.map (keyDecoder True) (Json.Decode.field "key" Json.Decode.string))
    , E.onKeyUp (Json.Decode.map (keyDecoder False) (Json.Decode.field "key" Json.Decode.string))
    ]

keyDecoder isDown key =
  case key of
    "0" -> KeyChanged isDown (Number 0)
    "1" -> KeyChanged isDown (Number 1)
    "2" -> KeyChanged isDown (Number 2)
    "3" -> KeyChanged isDown (Number 3)
    "4" -> KeyChanged isDown (Number 4)
    "5" -> KeyChanged isDown (Number 5)
    "6" -> KeyChanged isDown (Number 6)
    "7" -> KeyChanged isDown (Number 7)
    "8" -> KeyChanged isDown (Number 8)
    "9" -> KeyChanged isDown (Number 9)
    "Control" -> KeyChanged isDown Control
    "Shift" -> KeyChanged isDown Shift
    _ -> KeyChanged isDown Other

-- UPDATE

type Key
  = Number Int
  | Control
  | Shift
  | Other

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyChanged isDown key ->
      let

        assignVisibilityGroup visibilityGroup _ pg  =
          if pg.isSelected && pg.visibilityGroup /= visibilityGroup then
            {pg | visibilityGroup = visibilityGroup, isSelected = False}
          else
            pg

        newPieceGroups visibilityGroup =
          D.map (assignVisibilityGroup visibilityGroup) model.pieceGroups

        toggleVisibilityOf group number =
          if S.member number group then
            S.remove number group
          else
            S.insert number group
      in
      case key of
        Number x ->
          case (model.keyboard.ctrl, isDown) of
            (True, True) ->
              ( {model | pieceGroups = newPieceGroups x}, Cmd.none)
            (False, True) ->
              ( {model | visibleGroups = toggleVisibilityOf model.visibleGroups x
                , debug =
                  S.toList (toggleVisibilityOf model.visibleGroups x)
                    |> List.map String.fromInt
                    |> List.intersperse ", "
                    |> String.concat
              }, Cmd.none )
            (_, False) ->
              ( model, Cmd.none )

        Control ->
          let
            newKeyboard keyboard = {keyboard | ctrl = isDown}
          in
            ( {model | keyboard = newKeyboard model.keyboard}, Cmd.none )
        Shift ->
          let
            newKeyboard keyboard = {keyboard | shift = isDown}
          in
            ( {model | keyboard = newKeyboard model.keyboard}, Cmd.none )
        Other -> ( model, Cmd.none )

    Scramble ->
      let
        newModel = resetModel model.image model.seed
      in
        ( newModel, Cmd.none )

    MouseDown id coordinate keyboard ->
      let
        clickedOnBackground = id == -1
        newModel =
          if clickedOnBackground then
            startSelectionBox model coordinate keyboard
          else
            selectPieceGroup model id coordinate keyboard
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
                  let
                    isVisible = S.member pg.visibilityGroup model.visibleGroups
                    originallySelected = S.member pg.id box.selectedIds
                    insideBoxNow = isPieceGroupInsideBox model.image tl br pg
                    newSelectionStatus =
                      if isVisible then
                        if originallySelected && insideBoxNow then
                          True
                        else if originallySelected && not insideBoxNow then
                          True
                        else if not originallySelected && insideBoxNow then
                          True
                        else
                          False
                      else
                        False
                  in
                    { pg | isSelected = newSelectionStatus }
                updatedPieceGroups =
                  D.map selectPiece model.pieceGroups
              in
              ( { model
                | selectionBox = Normal {box | movingCorner = newPos}
                , pieceGroups = updatedPieceGroups
                }
              , Cmd.none )
            Inverted box ->
              let
                tl = boxTopLeft box
                br = boxBottomRight box
                selectPiece _ pg =
                  let
                    isVisible = S.member pg.visibilityGroup model.visibleGroups
                    originallySelected = S.member pg.id box.selectedIds
                    insideBoxNow = isPieceGroupInsideBox model.image tl br pg
                    newSelectionStatus =
                      if isVisible then
                        if originallySelected && insideBoxNow then
                          False
                        else if originallySelected && not insideBoxNow then
                          True
                        else if not originallySelected && insideBoxNow then
                          True
                        else
                          False
                      else
                        False
                  in
                    { pg | isSelected = newSelectionStatus }

                updatedPieceGroups =
                  D.map selectPiece model.pieceGroups
              in
              ( { model
                | selectionBox = Inverted {box | movingCorner = newPos}
                , pieceGroups = updatedPieceGroups
                }
              , Cmd.none )


selectPieceGroup : Model -> Int -> Point -> Keyboard -> Model
selectPieceGroup model id coordinate keyboard =
  let
    clickedPieceGroup =
      D.get id model.pieceGroups
        |> Maybe.withDefault defaultPieceGroup

    wasSelectedBefore =
      clickedPieceGroup.isSelected

    shouldStartDragging =
      wasSelectedBefore && model.selected == Multiple

    fixZlevels =
      D.insert id { clickedPieceGroup | zlevel = model.maxZLevel}

    selectClickedPieceGroup =
      D.insert id { clickedPieceGroup | isSelected = True }

    invertClickedPieceGroup =
      D.insert id { clickedPieceGroup | isSelected = not clickedPieceGroup.isSelected }

    deselectAllOther =
      D.map (\key pg -> {pg | isSelected = key == id})

    newPieceGroups =
      if keyboard.ctrl then
        invertClickedPieceGroup
      else if keyboard.shift then
        selectClickedPieceGroup << fixZlevels
      else if shouldStartDragging then
        fixZlevels
      else
        deselectAllOther << fixZlevels
  in
    { model
    | maxZLevel = model.maxZLevel + 1
    , cursor = Just coordinate
    , selected = currentSelection <| newPieceGroups model.pieceGroups
    , pieceGroups = newPieceGroups model.pieceGroups
    }

startSelectionBox : Model -> Point -> Keyboard -> Model
startSelectionBox model coordinate keyboard =
  let
    ids = allSelectedPieceGroups model.pieceGroups
      |> D.keys
      |> S.fromList
  in
  if keyboard.ctrl then
    { model
      | cursor = Just coordinate
      , selectionBox = Inverted
        { staticCorner = coordinate
        , movingCorner = coordinate
        , selectedIds = ids
        }
      }
  else if keyboard.shift then
    { model
      | cursor = Just coordinate
      , selectionBox = Normal
        { staticCorner = coordinate
        , movingCorner = coordinate
        , selectedIds = ids
        }
    }
  else
    { model
      | cursor = Just coordinate
      , selected = NullSelection
      , pieceGroups = D.map (\_ pg -> {pg | isSelected = False}) model.pieceGroups
      , selectionBox = Normal
        { staticCorner = coordinate
        , movingCorner = coordinate
        , selectedIds = S.empty
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
        Just (_, neighbour) ->
          if S.member neighbour.visibilityGroup model.visibleGroups then
            Just neighbour
          else
            Nothing

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
      model.pieceGroups


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
      Svg.defs []
        ( definePuzzleImage model.image ::
          definePieceClipPaths model.image model.edgePoints (D.values model.pieceGroups) )


    background =
      Svg.rect
        [ Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.fill "blue"
        , Svg.Attributes.opacity "0.0"
        , onMouseDown -1
        ]
        []

    svgSelectionBox box color =
      let
        topLeft = boxTopLeft box
        bottomRight = boxBottomRight box
      in
        Svg.rect
          [ Svg.Attributes.width <| String.fromInt (bottomRight.x - topLeft.x)
          , Svg.Attributes.height <| String.fromInt (bottomRight.y - topLeft.y)
          , Svg.Attributes.fill color
          , Svg.Attributes.fillOpacity "0.2"
          , Svg.Attributes.stroke <| "dark"++color
          , Svg.Attributes.strokeWidth "2px"
          , Svg.Attributes.strokeOpacity "0.9"
          , translate (topLeft)
          ]
          []

    normalSelection =
      case model.selectionBox of
        Normal box ->
          [ svgSelectionBox box "blue" ]
        Inverted box ->
          [ svgSelectionBox box "green" ]
        NullBox -> []

    pieces =
      List.map svgPieceGroup
        <| List.sortBy .zlevel
        <| List.filter (\pg -> S.member pg.visibilityGroup model.visibleGroups)
        <| D.values model.pieceGroups

    svgPieceGroup pg =
      Svg.g
        [ onMouseDown pg.id, translate pg.position ]
        ([ Svg.use
            [ Svg.Attributes.xlinkHref <| "#puzzle-image"
            , Svg.Attributes.clipPath <| clipPathRef pg.id
            ]
            []
        ] ++ List.map (svgOutlines pg.isSelected) pg.members)

    svgOutlines selected id =
      Svg.use
        [ Svg.Attributes.xlinkHref <| "#" ++ pieceOutlineId id
        , Svg.Attributes.stroke <| if selected then "red" else "black"
        , Svg.Attributes.strokeWidth "5px"
        ]
        []

  in
  Html.div [ ]
    [ Html.button [ Html.Events.onClick Scramble ] [ Html.text "scramble" ]
    , Html.h1 [] [ Html.text model.debug ]
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
    <| Json.Decode.map4 (\x y shift ctrl -> MouseDown id (Point x y) {shift=shift, ctrl=ctrl})
      (Json.Decode.field "offsetX" Json.Decode.int)
      (Json.Decode.field "offsetY" Json.Decode.int)
      (Json.Decode.field "shiftKey" Json.Decode.bool)
      (Json.Decode.field "ctrlKey" Json.Decode.bool)

onMouseMove : Svg.Attribute Msg
onMouseMove =
  Svg.Events.on "mousemove"
    <| Json.Decode.map2 (\x y -> MouseMove (Point x y))
      (Json.Decode.field "offsetX" Json.Decode.int)
      (Json.Decode.field "offsetY" Json.Decode.int)

translate : Point -> Svg.Attribute Msg
translate position =
  Svg.Attributes.transform
    <| "translate(" ++ Point.toString position ++ ")"


definePuzzleImage : JigsawImage -> Svg Msg
definePuzzleImage image =
  Svg.image
    [ Svg.Attributes.id "puzzle-image"
    , Svg.Attributes.xlinkHref image.path
    , Svg.Attributes.pointerEvents "none"
    , Svg.Attributes.transform <| "scale(" ++ String.fromFloat image.scale ++ ")"
    ]
    []


definePieceClipPaths : JigsawImage -> A.Array EdgePoints -> List PieceGroup -> List (Svg Msg)
definePieceClipPaths image edgePoints pieceGroups =
  List.map (pieceGroupClipPath image edgePoints) pieceGroups

pieceGroupClipPath : JigsawImage -> A.Array EdgePoints -> PieceGroup -> Svg Msg
pieceGroupClipPath image edgePoints pieceGroup =
  Svg.clipPath [ Svg.Attributes.id <| pieceClipId pieceGroup.id ]
    <| List.map (piecePath image edgePoints) pieceGroup.members

piecePath : JigsawImage -> A.Array EdgePoints -> Int -> Svg Msg
piecePath image edgePoints id =
  let
    w = image.scale * toFloat (image.width // image.xpieces)
    h = image.scale * toFloat (image.height // image.ypieces)
    offset = pieceIdToOffset image id

    curve = Edge.pieceCurveFromPieceId image.xpieces image.ypieces id edgePoints
    move = "translate(" ++ Point.toString offset ++ ") "
    scale = "scale(" ++ String.fromFloat (w / 200.0) ++ " " ++ String.fromFloat (h / 200.0) ++ ")"
  in
    Svg.path
      [ Svg.Attributes.id <| pieceOutlineId id
      , Svg.Attributes.transform <| move ++ scale
      , Svg.Attributes.d curve
      , Svg.Attributes.fillOpacity "0.0"
      ]
      []

pieceOutlineId : Int -> String
pieceOutlineId id =
  "piece-" ++ String.fromInt id ++ "-outline"

pieceClipId : Int -> String
pieceClipId id =
  "piece-" ++ String.fromInt id ++ "-clip"

clipPathRef : Int -> String
clipPathRef id =
  "url(#" ++ pieceClipId id ++ ")"

