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
import WebGL
import WebGL.Texture as Texture
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

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
  = MouseDown Point Keyboard
  | MouseMove Point
  | MouseUp
  | Scramble
  | KeyChanged Bool Key
  | PickImage
  | GotImage File
  | EncodedImage String
  | UpdateDim (Int, Int)
  | GotTexture (Result Texture.Error Texture.Texture)

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
  , texture : Maybe Texture.Texture
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
      , xpieces = 3
      , ypieces = 3
      , scale = 1
      }
    model =
      resetModel image (Random.initialSeed 0)
  in
  ( model, Task.attempt GotTexture (Texture.loadWith Texture.nonPowerOfTwoOptions image.path) )

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
    , texture = Nothing
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
    pieceWidth = floor <| image.scale * toFloat (image.width // image.xpieces)
    pieceHeight = floor <| image.scale * toFloat (image.height // image.ypieces)
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
          <| Json.Decode.map2 (\x y -> MouseMove (Point x y))
            (Json.Decode.field "pageX" Json.Decode.int)
            (Json.Decode.field "pageY" Json.Decode.int)
      else
        Sub.none

    trackMouseDown =
      Browser.Events.onMouseDown
        <| Json.Decode.map4 (\x y shift ctrl -> MouseDown (Point x y) {shift=shift, ctrl=ctrl})
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
    GotTexture result ->
      case result of
        Result.Ok texture ->
          ( { model | texture = Just texture, debug = "got that texture!" }
          , Cmd.none
          )
        Result.Err foo ->
          case foo of
            Texture.LoadError ->
              ( {model | texture = Nothing, debug = "Texture load error!"}, Cmd.none )
            Texture.SizeError x y ->
              ( {model | texture = Nothing, debug = "Texture size error! "++String.fromInt x ++ ", " ++ String.fromInt y}, Cmd.none )

    UpdateDim (x, y) ->
      let
        oldImage = model.image
        newImage = {oldImage | width = x, height = y}
        newModel = resetModel newImage model.seed
      in
      ( newModel
      , Task.attempt GotTexture (Texture.loadWith Texture.nonPowerOfTwoOptions model.image.path))
    PickImage ->
      ( model, File.Select.file ["image/*"] GotImage )
    GotImage file ->
      ( {model | debug = "got a file!"}, Task.perform EncodedImage (File.toUrl file))

    EncodedImage url ->
      let
        oldImage = model.image
        newImage = {oldImage | path = url}
      in
      ( {model | debug = "file url length: " ++ String.fromInt (String.length url), image = newImage}
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
        ( newModel, Task.attempt GotTexture (Texture.loadWith Texture.nonPowerOfTwoOptions newModel.image.path) )

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
      ((viewWebGL model) :: (viewSelectionBox model))
    ]


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
      in
        Html.div
          ([ Html.Attributes.style "width" <| String.fromInt (bottomRight.x - topLeft.x) ++ "px"
          , Html.Attributes.style "height" <| String.fromInt (bottomRight.y - top) ++ "px"
          , Html.Attributes.style "background-color" color
          , Html.Attributes.style "border-style" "dotted"
          , Html.Attributes.style "top" <| String.fromInt (top - 100) ++ "px"
          , Html.Attributes.style "left" <| String.fromInt topLeft.x ++ "px"
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

    left = List.map xpos members |> List.minimum |> Maybe.withDefault 0
    right = List.map xpos members |> List.maximum |> Maybe.withDefault 0
    top = List.map ypos members |> List.minimum |> Maybe.withDefault 0
    bottom = List.map ypos members |> List.maximum |> Maybe.withDefault 0
  in
    {t=top, l=left, r=right, b=bottom}


viewDiv model =
  let
    pieceGroupDiv : PieceGroup -> Html Msg
    pieceGroupDiv pg =
      let
        {t, l, r, b} = pieceGroupSize pg.members model.image.xpieces

        imageWidth = model.image.scale * toFloat model.image.width
        imageHeight = model.image.scale * toFloat model.image.height
        pieceWidth =  (floor imageWidth // model.image.xpieces)
        pieceHeight = (floor imageHeight // model.image.ypieces)

        top =  pg.position.y + t * pieceHeight - pieceHeight // 2
        left = pg.position.x + l * pieceWidth - pieceWidth // 2
        w =  (r - l + 2) * pieceWidth
        h =  (b - t + 2) * pieceHeight
        bgx = pieceWidth // 2 - l * pieceWidth
        bgy = pieceHeight // 2 - t * pieceHeight

        color = if pg.isSelected then "red" else "black"
        display = if S.member pg.visibilityGroup model.visibleGroups then "block" else "none"
      in
      Html.div
      [ Html.Attributes.style "z-index" <| String.fromInt pg.zlevel
      , Html.Attributes.style "filter" <| "drop-shadow(0px 0px 2px " ++ color ++ ")"
      , Html.Attributes.style "position" "absolute"

      ]
      [
      Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "width" <| String.fromInt w ++ "px"
        , Html.Attributes.style "height" <| String.fromInt h ++ "px"
        , Html.Attributes.style "top" <| String.fromInt top ++ "px"
        , Html.Attributes.style "left" <| String.fromInt left ++ "px"
        , Html.Attributes.style "z-index" <| String.fromInt pg.zlevel
        , Html.Attributes.style "clipPath" <| clipPathRef pg.id
        , Html.Attributes.style "display" display
        ]
        [
          Html.img
          ([ Html.Attributes.src model.image.path
          , Html.Attributes.width <| floor imageWidth
          , Html.Attributes.height <| floor imageHeight
          , Html.Attributes.style "position" "absolute"
          , Html.Attributes.style "top" <| String.fromInt bgy ++ "px"
          , Html.Attributes.style "left" <| String.fromInt bgx ++ "px"
          ] ++ turnOffTheBloodyImageDragging)
          []
        ]
      ]


    viewPieces =
      List.map pieceGroupDiv <| D.values model.pieceGroups

    clipPathDefs =
      Svg.defs
        []
        ( definePieceGroupClipPaths model.image model.edgePoints (D.values model.pieceGroups))
  in
    [ Html.div
      ( turnOffTheBloodyImageDragging )
      ( viewPieces )
    , Svg.svg
      []
      [ clipPathDefs ]
    ]


definePieceGroupClipPaths image edgePoints pieceGroups =
  List.map (pieceGroupPath image edgePoints) pieceGroups

pieceGroupPath : JigsawImage -> A.Array EdgePoints -> PieceGroup -> Svg Msg
pieceGroupPath image edgePoints pieceGroup =
  let
    {t, l, r, b} = pieceGroupSize pieceGroup.members image.xpieces
    imageWidth = floor <| image.scale * toFloat (image.width // image.xpieces)
    imageHeight = floor <| image.scale * toFloat (image.height // image.ypieces)

    offsetx = (imageWidth // 2 - l * imageWidth)
    offsety = (imageHeight // 2 - t * imageHeight)

    curve = Edge.pieceGroupCurve pieceGroup.members image.xpieces image.ypieces edgePoints
    move = "translate("
      ++ String.fromInt offsetx ++ " "
      ++ String.fromInt offsety ++ ") "
    scale = "scale("
      ++ String.fromFloat (toFloat imageWidth / 200.0) ++ " "
      ++ String.fromFloat (toFloat imageHeight / 200.0) ++ ")"
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
  , Html.Attributes.style "box-shadow" "-2px 5px 4px rgba(0,0,0,0.8)"
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

  , Html.p
    [ Html.Attributes.style "height" "1000px"
    , Html.Attributes.style "width" "1000px"]
    [ Html.text model.debug ]
  ]


port newDim : ((Int, Int) -> msg) -> Sub msg
port getDim : String -> Cmd msg


-- WEBGL
type alias Uniforms =
  { translation : Mat4
  , scale : Mat4
  , texture : Texture.Texture
--  , rotation : Mat4
  }

type alias Vertex =
  { position : Vec3
  , coord : Vec2
  }


viewWebGL : Model -> Html Msg
viewWebGL model =
  let
    pieceWidth = toFloat <| model.image.width // model.image.xpieces
    pieceHeight = toFloat <| model.image.height // model.image.ypieces
    scale =
      Mat4.makeScale
        <| vec3 (pieceWidth / toFloat model.width) (pieceHeight / toFloat model.height) 1

    makePieceEntity texture pos pid =
      let
        w = toFloat model.width
        h = toFloat model.height
        offset = pieceIdToOffset model.image pid
        xpos = toFloat <| pos.x + offset.x
        ypos = toFloat <| pos.y + offset.y
        x = 2 * (xpos - w / 2 + pieceWidth / 2) / w
        y = 2 * (h / 2 - ypos - pieceHeight / 2) / h
        translation = Mat4.makeTranslate (vec3 x y 0)
      in
        WebGL.entity
          vertexShader
          fragmentShader
          (pieceMesh model.image pid)
          { translation = translation
          , scale = scale
          , texture = texture }

    makePieceGroupEntity : Texture.Texture -> PieceGroup -> List WebGL.Entity
    makePieceGroupEntity texture pg =
      List.map (makePieceEntity texture pg.position) pg.members

  in
  case model.texture of
    Nothing ->
      Html.text "Loading texture..."
    Just texture ->
      WebGL.toHtml
      [ Html.Attributes.width model.width
      , Html.Attributes.height model.height
      , Html.Attributes.style "display" "block"
      ]
      ( List.concat
        <| List.map (makePieceGroupEntity texture)
        <| List.reverse
        <| List.sortBy .zlevel
        <| D.values model.pieceGroups )


texturePosition : JigsawImage -> Int -> Float -> Float -> Vec2
texturePosition image id x y =
  let
    pieceWidth = image.scale * toFloat (image.width // image.xpieces)
    pieceHeight = image.scale * toFloat (image.height // image.ypieces)
    imageWidth = toFloat image.width
    imageHeight = toFloat image.height
    offset = pieceIdToOffset image id

    newx = (x + 1) * pieceWidth / (2 * imageWidth) + (toFloat offset.x) / imageWidth
    newy = (y - 1) * pieceHeight / (2 * imageHeight) + 1 - (toFloat offset.y) / imageHeight
  in
    vec2 newx newy


pieceMesh : JigsawImage -> Int -> WebGL.Mesh Vertex
pieceMesh image id =
  let
    points = [ (-1.0, -1.0), (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0) ]
    pointToVertex (x, y) =
      Vertex (vec3 x y 0) (texturePosition image id x y)
  in
    WebGL.triangleFan
      <| List.map pointToVertex points

vertexShader : WebGL.Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 translation;
        uniform mat4 scale;

        varying vec2 vcoord;

        void main () {
            gl_Position =  translation * scale * vec4(position, 1.0);
            vcoord = coord;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vcoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec2 vcoord;
        uniform sampler2D texture;
        void main () {
            gl_FragColor = texture2D(texture, vcoord);
        }
    |]
