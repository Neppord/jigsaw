port module Main exposing (..)


import Set as S
import Dict as D
import Array as A
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes
import Random
import Random.List
import File exposing (File)
import File.Select
import Task
import Canvas
import Canvas.Settings
import Canvas.Settings.Advanced as Settings
import Canvas.Texture as Texture exposing (Texture)
import Color

import Point exposing (Point)
import Util exposing (takeFirst)
import Edge exposing (EdgePoints, makeEdgePoints, pieceCurveFromPieceId)

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
  = MouseDown Point Keyboard
  | MouseMove Point
  | MouseUp
  | Scramble
  | KeyChanged Bool Key
  | PickImage
  | GotImage File
  | EncodedImage String
  | UpdateDim (Int, Int)
  | UpdateSerialize (List String)
  | TextureLoaded (Maybe Texture)
  | FooBar

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
  , texture : Load Sprites
  , sprites : A.Array String
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

type Load a
  = Loading
  | Success a
  | Failure

type alias Sprites =
  A.Array Texture

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


type alias JSMessage =
  { contours : List String
  , nx : Int
  , ny : Int
  }

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
      { path = "resources/hongkong.jpg"
      , width = 6000
      , height = 4000
      , xpieces = 40
      , ypieces = 30
      , scale = 0.2
      , texture = Loading
      , sprites = A.empty
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
    xmax = toFloat w - (toFloat image.width) / (toFloat image.xpieces)
    ymin = 0
    ymax = toFloat h - (toFloat image.height) / (toFloat image.ypieces)
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
--        List.map (Point.mul image.scale) points
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
      , { position = Point.sub pos <| Point.mul image.scale (pieceIdToOffset image i)
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

--
pieceIdToPoint : Int -> Int -> Point
pieceIdToPoint id xpieces =
  Point (toFloat (modBy xpieces id)) (toFloat (id // xpieces))

pieceIdToOffset : JigsawImage -> Int -> Point
pieceIdToOffset image id =
  let
    pieceWidth =  image.scale * (toFloat image.width) / (toFloat image.xpieces)
    pieceHeight = image.scale * (toFloat image.height) / (toFloat image.ypieces)
  in
    Point
      (pieceWidth * (toFloat <| modBy image.xpieces id))
      (pieceHeight * (toFloat <| id // image.xpieces))



isPieceInsideBox : JigsawImage -> Point -> Point -> Point -> Int -> Bool
isPieceInsideBox image pos boxTL boxBR id =
  let
    pieceWidth = image.scale * (toFloat image.width) / (toFloat image.xpieces)
    pieceHeight = image.scale * (toFloat image.height) / (toFloat image.ypieces)
    pieceTL = Point.add pos <| pieceIdToOffset image id
    pieceBR = Point.add pieceTL <| Point pieceWidth pieceHeight
  in
    ( pieceTL.x <= boxBR.x ) &&
    ( pieceTL.y + 100 <= boxBR.y ) &&
    ( pieceBR.x >= boxTL.x ) &&
    ( pieceBR.y + 100 >= boxTL.y )

isPieceGroupInsideBox : JigsawImage -> Point -> Point -> PieceGroup -> Bool
isPieceGroupInsideBox image boxTL boxBR pieceGroup =
  List.any (isPieceInsideBox image pieceGroup.position boxTL boxBR) pieceGroup.members


-- SUBSCRIPTIONS

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

isPointInsidePiece : JigsawImage -> Point -> Point -> Int -> Bool
isPointInsidePiece image point pos id =
  let
    pieceWidth = image.scale * (toFloat image.width) / (toFloat image.xpieces)
    pieceHeight = image.scale * (toFloat image.height) / (toFloat image.ypieces)
    pieceTL = Point.add pos <| pieceIdToOffset image id
    pieceBR = Point.add pieceTL <| Point pieceWidth pieceHeight
  in
    ( pieceTL.x <= point.x ) &&
    ( pieceTL.y + 100 <= point.y ) &&
    ( pieceBR.x >= point.x ) &&
    ( pieceBR.y + 100 >= point.y )

isPointInsidePieceGroup visibleGroups image point pieceGroup =
  (S.member pieceGroup.visibilityGroup visibleGroups) &&
  (List.any (isPointInsidePiece image point pieceGroup.position) pieceGroup.members)



subscriptions : Model -> Sub Msg
subscriptions model =
  let
    trackMouseMovement =
      if model.cursor /= Nothing then
        Browser.Events.onMouseMove
          <| Json.Decode.map2 (\x y -> MouseMove (Point (toFloat x) (toFloat y)))
            (Json.Decode.field "pageX" Json.Decode.int)
            (Json.Decode.field "pageY" Json.Decode.int)
      else
        Sub.none

    trackMouseDown =
      Browser.Events.onMouseDown
        <| Json.Decode.map4 (\x y shift ctrl -> MouseDown (Point (toFloat x) (toFloat y)) {shift=shift, ctrl=ctrl})
          (Json.Decode.field "pageX" Json.Decode.int)
          (Json.Decode.field "pageY" Json.Decode.int)
          (Json.Decode.field "shiftKey" Json.Decode.bool)
          (Json.Decode.field "ctrlKey" Json.Decode.bool)

    trackMouseUp =
      Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
  in
  Sub.batch
    [ trackMouseMovement
    , trackMouseDown
    , trackMouseUp
    , Browser.Events.onKeyDown (Json.Decode.map (keyDecoder True) (Json.Decode.field "key" Json.Decode.string))
    , Browser.Events.onKeyUp (Json.Decode.map (keyDecoder False) (Json.Decode.field "key" Json.Decode.string))
    , newDim UpdateDim
    , newSerialize UpdateSerialize
    ]

-- UPDATE

type Key
  = Number Int
  | Control
  | Shift
  | Other



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FooBar ->
      let
        scale = Debug.log "scale = " <|
          Point
            ((toFloat model.image.width) / (200.0 * toFloat model.image.xpieces))
            ((toFloat model.image.height) / (200.0 * toFloat model.image.ypieces))
        contour pid =
          pieceCurveFromPieceId scale model.image.xpieces model.image.ypieces model.edgePoints pid

        contours =
          List.concatMap (\pg -> List.map contour pg.members) <| D.values model.pieceGroups

      in
      ( model
      , svgToDataUrl {contours = contours, nx = model.image.xpieces, ny = model.image.ypieces})
    TextureLoaded Nothing ->
      ( {model | image =
          let
            oldImage = model.image
          in
            {oldImage | texture = Failure}
        }
      , Cmd.none)
    TextureLoaded (Just texture) ->
      ( {model | image =
          let
            oldImage = model.image
            pieceWidth = (toFloat model.image.width) / (toFloat model.image.xpieces)
            pieceHeight = (toFloat model.image.height) / (toFloat model.image.ypieces)
            sprite t id =
              let
                offset = pieceIdToOffset model.image id

              in
              Texture.sprite
                { x = offset.x / model.image.scale
                , y = offset.y / model.image.scale
                , width = pieceWidth
                , height = pieceHeight
                }
                t
            sprites =
              A.fromList
                <| List.map (sprite texture)
                <| List.range 0 (model.image.xpieces * model.image.ypieces - 1)
          in
            {oldImage | texture = Success sprites}
        }
      , Cmd.none
      )
    UpdateSerialize sprites ->
      let
        oldImage = model.image
        newImage = {oldImage | sprites = A.fromList sprites}
      in
      ( {model | image = newImage}, Cmd.none )

    UpdateDim (x, y) ->
      let
        oldImage = model.image
        newImage = {oldImage | width = x, height = y}
        newModel = resetModel newImage model.seed
      in
      ( newModel
      , Cmd.none)
    PickImage ->
      ( model, File.Select.file ["image/*"] GotImage )
    GotImage file ->
      ( model, Task.perform EncodedImage (File.toUrl file))

    EncodedImage url ->
      let
        oldImage = model.image
        newImage = {oldImage | path = url}
      in
      ( {model | image = newImage}
      , getDim url )
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
              ( {model | visibleGroups = toggleVisibilityOf model.visibleGroups x}, Cmd.none )
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

    MouseDown coordinate keyboard ->
      let
        clickedPieceGroup =
          D.values model.pieceGroups
            |> List.filter (isPointInsidePieceGroup model.visibleGroups model.image coordinate)
            |> List.foldl (\a b -> if a.zlevel > b.zlevel then a else b) defaultPieceGroup

        clickedOnBackground =
          clickedPieceGroup.id == -10

        newModel =
          if clickedOnBackground then
            startSelectionBox model coordinate keyboard
          else
            selectPieceGroup model clickedPieceGroup.id coordinate keyboard
      in
        ( newModel
        , Cmd.none )

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
  Html.div
    ( turnOffTheBloodyImageDragging )
    [
      viewMenu model
    , Html.div
      [ Html.Attributes.style "width" <| String.fromInt model.image.width ++ "px"
      , Html.Attributes.style "height" <| String.fromInt model.image.height ++ "px"
      , Html.Attributes.style "position" "absolute"
      , Html.Attributes.style "top" "100px"
      , Html.Attributes.style "left" "0px"
      ]
      (
--        (viewCanvas model) ::
        (preloadImage model.image) ::
        (viewDiv model) ++
        (viewSelectionBox model)
      )
    ]

preloadImage image =
  Html.img
  [ Html.Events.on "load" (Json.Decode.succeed FooBar)
  , Html.Attributes.id "my_image"
  , Html.Attributes.style "display" "none"
  , Html.Attributes.src image.path
  ]
  []


turnOffTheBloodyImageDragging =
  [ Html.Attributes.style "-webkit-user-select" "none"
  , Html.Attributes.style "-khtml-user-select" "none"
  , Html.Attributes.style "-moz-user-select" "none"
  , Html.Attributes.style "-o-user-select" "none"
  , Html.Attributes.style "user-select" "none"
  , Html.Attributes.draggable "false"
  ]

viewSelectionBox model =
  let
    divSelectionBox box color =
      let
        topLeft = boxTopLeft box
        bottomRight = boxBottomRight box
        top = max 100 topLeft.y
        coord = Point.sub bottomRight topLeft
      in
        Html.div
          ([ Html.Attributes.style "width" <| Point.xToPixel coord
          , Html.Attributes.style "height" <| String.fromInt (floor (bottomRight.y - top)) ++ "px"
          , Html.Attributes.style "background-color" color
          , Html.Attributes.style "border-style" "dotted"
          , Html.Attributes.style "top" <| String.fromInt (floor top - 100) ++ "px"
          , Html.Attributes.style "left" <| Point.xToPixel topLeft
          , Html.Attributes.style "z-index" <| String.fromInt (model.maxZLevel + 1)
          , Html.Attributes.style "position" "absolute"
          ] ++ turnOffTheBloodyImageDragging)
          []
  in
    case model.selectionBox of
      Normal box ->
        [ divSelectionBox box "rgba(0,0,255,0.2)" ]
      Inverted box ->
        [ divSelectionBox box "rgba(0,255,0,0.2)" ]
      NullBox ->
        []

pieceGroupSize members nx =
  let
    xpos = modBy nx
    ypos id = id // nx

    left = List.map xpos members |> List.minimum |> Maybe.withDefault 0 |> toFloat
    right = List.map xpos members |> List.maximum |> Maybe.withDefault 0 |> toFloat
    top = List.map ypos members |> List.minimum |> Maybe.withDefault 0 |> toFloat
    bottom = List.map ypos members |> List.maximum |> Maybe.withDefault 0 |> toFloat
  in
    {t=top, l=left, r=right, b=bottom}


viewDiv model =
  let
    -- TODO: Fix default sprite
    imageWidth = model.image.scale * toFloat model.image.width
    imageHeight = model.image.scale * toFloat model.image.height
    pieceWidth =  imageWidth / toFloat model.image.xpieces
    pieceHeight = imageHeight / toFloat model.image.ypieces
    sprite pid =
      A.get pid model.image.sprites
        |> Maybe.withDefault model.image.path

    viewPieceGroup pg =
      let
        color = if pg.isSelected then "red" else "black"
        display = if S.member pg.visibilityGroup model.visibleGroups then "block" else "none"
        zlevel = String.fromInt pg.zlevel
      in
      List.map (pieceDiv pg.position color display zlevel) pg.members

    pieceDiv : Point -> String -> String -> String -> Int -> Html Msg
    pieceDiv pos color display zlevel pid =
      let
        offset = pieceIdToOffset model.image pid


        tl = Point.add offset (Point.sub pos <| Point (pieceWidth/2) (pieceHeight/2))
        wh = Point (2 * pieceWidth) (2 * pieceHeight)
        wh2 = Point.mul 0.5 wh
--        color = if pg.isSelected then "red" else "black"
--        display = if S.member pg.visibilityGroup model.visibleGroups then "block" else "none"

        -- TODO: Fix default sprite
--        sprite = A.get pg.id model.image.sprites |> Maybe.withDefault model.image.path
      in
      Html.div
      [ Html.Attributes.style "z-index" <| zlevel
--      , Html.Attributes.style "filter" <| "drop-shadow(0px 0px 2px " ++ color ++ ")"
      , Html.Attributes.style "position" "absolute"
      ]
      [
      Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "width" <| Point.xToPixel wh
        , Html.Attributes.style "height" <| Point.yToPixel wh
        , Html.Attributes.style "top" <| Point.yToPixel tl
        , Html.Attributes.style "left" <| Point.xToPixel tl
        , Html.Attributes.style "z-index" <| zlevel
        , Html.Attributes.style "display" display
        ]
        [
          Html.img
          ([
             Html.Attributes.src <| sprite pid
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0px"
            , Html.Attributes.style "left" "0px"
            , Html.Attributes.style "width" <| Point.xToPixel wh
            , Html.Attributes.style "height" <| Point.yToPixel wh
          ] ++ turnOffTheBloodyImageDragging)
          []
        ]
      ]


    viewPieces =
      List.concatMap viewPieceGroup <| D.values model.pieceGroups

--    clipPathDefs =
--      Svg.defs
--        []
--        ( definePieceGroupClipPaths model.image model.edgePoints (D.values model.pieceGroups))
  in
    [ Html.div
      ( turnOffTheBloodyImageDragging )
      ( viewPieces )
--    , Svg.svg
--      []
--      [ clipPathDefs ]
    ]


definePieceGroupClipPaths image edgePoints pieceGroups =
  List.map (pieceGroupPath image edgePoints) pieceGroups

pieceGroupPath : JigsawImage -> A.Array EdgePoints -> PieceGroup -> Svg Msg
pieceGroupPath image edgePoints pieceGroup =
  let
    {t, l, r, b} = pieceGroupSize pieceGroup.members image.xpieces
    imageWidth = image.scale * (toFloat image.width) / (toFloat image.xpieces)
    imageHeight = image.scale * (toFloat image.height) / (toFloat image.ypieces)

    offset = Point (imageWidth * (0.5 - l)) (imageHeight * (0.5 - t))

    curve = (Edge.pieceGroupCurve pieceGroup.members image.xpieces image.ypieces edgePoints)
    move = "translate(" ++ Point.toIntString offset ++ ") "
    scale = "scale("
      ++ String.fromFloat (imageWidth / 200.0) ++ " "
      ++ String.fromFloat (imageHeight / 200.0) ++ ")"
  in
    Svg.clipPath
    [ Svg.Attributes.id <| pieceClipId pieceGroup.id ]
    [ Svg.path
      [ Svg.Attributes.id <| pieceOutlineId pieceGroup.id
      , Svg.Attributes.transform <| move ++ scale
      , Svg.Attributes.d curve
      , Svg.Attributes.fillOpacity "0.0"
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

-- MENU

viewMenu : Model -> Html Msg
viewMenu model =
  Html.div
  [ Html.Attributes.style "width" "100%"
  , Html.Attributes.style "height" "50px"
  , Html.Attributes.style "position" "fixed"
  , Html.Attributes.style "top" "0"
  , Html.Attributes.style "left" "0px"
  , Html.Attributes.style "background-color" "#CCCCCC"
  , Html.Attributes.style "box-shadow" "-2px 5px 4px grey"
  , Html.Attributes.style "z-index" <| String.fromInt <| model.maxZLevel + 10
  , Html.Attributes.style "flex-direction" "column"
  ]
  [
    Html.div
    [ Html.Attributes.class "dropdown"
    ]
    [
      Html.button
      [ Html.Attributes.class "dropbtn"
      ]
      [ Html.text "Menu"
      ]
    , Html.div
      [ Html.Attributes.class "dropdown-content"
      ]
      [ Html.a
        [ Html.Events.onClick Scramble ]
        [ Html.text "Scramble" ]
      , Html.a
        [ Html.Events.onClick PickImage ]
        [ Html.text "Choose image" ]
      , Html.a
        []
        [ Html.text "[TODO] Options" ]
      ]
    ]
  ]

-- CANVAS

viewCanvas : Model -> Html Msg
viewCanvas model =
  Canvas.toHtmlWith
    { width = model.width
    , height = model.height
    , textures = [ Texture.loadFromImageUrl model.image.path TextureLoaded ]
    }
    []
    (
      [ clearCanvas (toFloat model.width) (toFloat model.height) ]
      ++ (drawPieces model.image model.pieceGroups)
      ++ [drawMasks model.image model.pieceGroups]
    )

clearCanvas width height =
  Canvas.shapes
    [ Canvas.Settings.fill Color.white ]
    [ Canvas.rect (0, 0) width height ]


drawPieces : JigsawImage -> D.Dict Int PieceGroup -> List Canvas.Renderable
drawPieces image pieceGroups =
  let
    scale =
      [ Settings.transform [ Settings.scale image.scale image.scale ] ]

    renderPiece sprites pos pid =
      let
        offset = pieceIdToOffset image pid
        position =
          ( (pos.x + offset.x) / image.scale
          , (pos.y + offset.y) / image.scale
          )
      in
      case A.get pid sprites of
        Just sprite -> Canvas.texture scale position sprite
        Nothing -> Canvas.text [] (100, 100) "Oops"

    renderPieceGroup : A.Array Texture -> PieceGroup -> List Canvas.Renderable
    renderPieceGroup sprites pg =
      List.map (renderPiece sprites pg.position) pg.members

  in
  case image.texture of
    Loading ->
      [ Canvas.text [] (100, 100) "Loading texture" ]
    Failure ->
      [ Canvas.text [] (100, 100) "Failed to load texture" ]
    Success sprites ->
      List.concatMap (renderPieceGroup sprites) (D.values pieceGroups)


drawMasks image pieceGroups =
  Canvas.shapes
    [ Canvas.Settings.fill (Color.rgba 1 0 0 1)
    , Settings.compositeOperationMode Settings.DestinationIn
    ]
    ( List.concatMap (drawPieceGroupMasks image) <| D.values pieceGroups)

drawPieceGroupMasks image pg =
  List.map (drawPieceMask image pg.position) pg.members

drawPieceMask : JigsawImage -> Point -> Int -> Canvas.Shape
drawPieceMask image pos pid =
  let
    imageWidth = image.scale * toFloat image.width
    imageHeight = image.scale * toFloat image.height
    pieceWidth =  imageWidth / toFloat image.xpieces
    pieceHeight =  imageHeight / toFloat image.ypieces
    offset = pieceIdToOffset image pid
  in
    Canvas.circle (pos.x + offset.x + pieceWidth / 2, pos.y + offset.y + pieceHeight / 2) (pieceWidth / 2)



port newDim : ((Int, Int) -> msg) -> Sub msg
port getDim : String -> Cmd msg
port newSerialize : (List String -> msg) -> Sub msg
port svgToDataUrl : JSMessage -> Cmd msg