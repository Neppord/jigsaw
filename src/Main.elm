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
import Random
import Random.List
import Random.Set
import Random.Extra
import List.Extra
import File exposing (File)
import File.Select
import Task

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
  | FinishedLoading ThingThatCanLoad
  | Cheat Int
  | SetOptionsVisibility Bool

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
  , visibleGroups : S.Set Int
  , keyboard : Keyboard
  , loading : LoadStatus
  , edgePoints : A.Array EdgePoints
  , viewOptions : Bool
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

type ThingThatCanLoad
  = ImageUrl String
  | ImageDiv
  | CanvasDraw (Int, Int)

type LoadStatus
  = ParsingImageUrl
  | LoadingImage
  | DrawingCanvas
  | Finished

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

type alias MultipleContoursMessage =
  { contours : List ContourMessage
  , nx : Int
  , ny : Int
  }

type alias SingleContourMessage =
  { contour : ContourMessage
  , nx : Int
  , ny : Int
  }

type alias ContourMessage =
  { canvasId : String
  , contour: String
  , t: Float
  , l: Float
  , r: Float
  , b: Float
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
      , xpieces = 6
      , ypieces = 4
      , scale = 0.2
      }
  in
  ( createModel image, Cmd.none )

createModel : JigsawImage -> Model
createModel image =
  { cursor = Nothing
  , pieceGroups = D.empty
  , selected = NullSelection
  , maxZLevel = image.xpieces * image.ypieces
  , image = image
  , width = 1800
  , height = 1100
  , snapDistance = 30.0
  , selectionBox = NullBox
  , seed = Random.initialSeed 0
  , visibleGroups = S.fromList [-1]
  , keyboard = { shift = False, ctrl = False }
  , loading = LoadingImage
  , edgePoints = A.empty
  , viewOptions = False
  }

{-  This resets everything except piece positions. For that, use
    randomizePieceGroupPositions.
-}
resetModel : Model -> (Model, Cmd Msg)
resetModel model =
  let
    nx = model.image.xpieces
    ny = model.image.ypieces
    numberOfPieces = nx * ny
    numberOfEdges = Debug.log "ImageDiv" (2 * nx * ny - nx - ny)

    (zlevels, seed1) = generateRandomZLevels numberOfPieces model.seed
    (edgePoints, seed2) = makeEdgePoints numberOfEdges seed1

    newModel =
      { model
        | cursor = Nothing
        , pieceGroups = createPieceGroups model.image zlevels
        , selected = NullSelection
        , maxZLevel = model.image.xpieces * model.image.ypieces
        , selectionBox = NullBox
        , seed = seed2
        , visibleGroups = S.fromList [-1]
        , keyboard = { shift = False, ctrl = False}
        , loading = DrawingCanvas
        , edgePoints = edgePoints
        , viewOptions = False
      }

    command =
      drawPiecesInCanvas
        <| multipleContoursMessage
            newModel.image.xpieces
            newModel.image.ypieces
            newModel.edgePoints
            newModel.pieceGroups
  in
    ( newModel
    , command
    )


generateRandomPiecePositions : Int -> Int -> JigsawImage -> Random.Seed -> (List Point, Random.Seed)
generateRandomPiecePositions w h image seed =
  let
    n = image.xpieces * image.ypieces
    xmin = 0
    xmax = toFloat w - (toFloat image.width) / (toFloat image.xpieces)
    ymin = 0
    ymax = toFloat h - (toFloat image.height) / (toFloat image.ypieces)
  in
    Random.step (Point.randomPoints n xmin xmax ymin ymax) seed

generateRandomZLevels : Int -> Random.Seed -> (List Int, Random.Seed)
generateRandomZLevels n seed =
  Random.step (Random.List.shuffle <| List.range 0 (n - 1)) seed

createPieceGroups : JigsawImage -> List Int -> D.Dict Int PieceGroup
createPieceGroups image levels =
  let
    nx = image.xpieces
    ny = image.ypieces
    n = nx*ny

    range =
      List.range 0 (n - 1)
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
    onePieceGroup i zlevel =
      ( i
      , { position = Point 0 0
        , isSelected = False
        , id = i
        , zlevel = zlevel
        , members = [ i ]
        , neighbours = S.filter (isRealNeighbour i) <| S.fromList (possibleNeighbours i)
        , visibilityGroup = -1
        }
      )
  in
    D.fromList <| List.map2 onePieceGroup range zlevels

randomizePieceGroupPositions : Model -> Model
randomizePieceGroupPositions model =
  let
    (positions, newSeed) = generateRandomPiecePositions model.width model.height model.image model.seed

    setPosition pos (id, pg) =
      (id, {pg | position = Point.sub pos <| pieceIdToOffset model.image id})

    newPieceGroups =
      D.toList model.pieceGroups
        |> List.map2 setPosition positions
        |> D.fromList

  in
    { model | pieceGroups = newPieceGroups, seed = newSeed }


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


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    trackMouseMovement =
      if model.cursor /= Nothing && not model.viewOptions then
        Browser.Events.onMouseMove
          <| Json.Decode.map2 (\x y -> MouseMove (Point (toFloat x) (toFloat y)))
            (Json.Decode.field "pageX" Json.Decode.int)
            (Json.Decode.field "pageY" Json.Decode.int)
      else
        Sub.none

    trackMouseDown =
      if not model.viewOptions then
        Browser.Events.onMouseDown
          <| Json.Decode.map4 (\x y shift ctrl -> MouseDown (Point (toFloat x) (toFloat y)) {shift=shift, ctrl=ctrl})
            (Json.Decode.field "pageX" Json.Decode.int)
            (Json.Decode.field "pageY" Json.Decode.int)
            (Json.Decode.field "shiftKey" Json.Decode.bool)
            (Json.Decode.field "ctrlKey" Json.Decode.bool)
      else
        Sub.none

    trackMouseUp =
      Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
  in
  Sub.batch
    [ trackMouseMovement
    , trackMouseDown
    , trackMouseUp
    , Browser.Events.onKeyDown (Json.Decode.map (keyDecoder True) (Json.Decode.field "key" Json.Decode.string))
    , Browser.Events.onKeyUp (Json.Decode.map (keyDecoder False) (Json.Decode.field "key" Json.Decode.string))
    , canvasDrawn (FinishedLoading << CanvasDraw)
    ]

-- UPDATE

type Key
  = Number Int
  | Control
  | Shift
  | Other


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

multipleContoursMessage : Int -> Int -> A.Array EdgePoints -> D.Dict Int PieceGroup -> MultipleContoursMessage
multipleContoursMessage nx ny edgePoints pieceGroups =
  let
    contours =
      List.map (contourMessage nx ny edgePoints) <| D.values pieceGroups
  in
    {contours = contours, nx = nx, ny = ny}

singleContourMessage : Int -> Int -> A.Array EdgePoints -> PieceGroup -> SingleContourMessage
singleContourMessage nx ny edgePoints pg =
  {contour = contourMessage nx ny edgePoints pg, nx = nx, ny = ny}

contourMessage nx ny edgePoints pg =
  let
    {t, l, r, b} = pieceGroupSize pg.members nx
  in
    { canvasId = getCanvasId pg.id
    , contour = Edge.pieceGroupCurve pg.members nx ny edgePoints
    , t = t
    , l = l
    , r = r
    , b = b
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetOptionsVisibility state ->
      ( {model | viewOptions = state}, Cmd.none )
    Cheat number ->
      cheatManyTimes number model

    FinishedLoading thing ->
      let
        oldImage = model.image
      in
      case thing of
        ImageUrl url ->
          ( {model | image = {oldImage | path = url}, loading = LoadingImage}
          , Cmd.none
          )
        ImageDiv ->
          resetModel model
        CanvasDraw (x, y) ->
          let
            updatedModel = {model | image = {oldImage | width = x, height = y}, loading = Finished}
          in
          ( randomizePieceGroupPositions updatedModel
          , Cmd.none )

    PickImage ->
      ( model
      , File.Select.file ["image/*"] GotImage )
    GotImage file ->
      ( {model | loading = ParsingImageUrl}
      , Task.perform (FinishedLoading << ImageUrl) (File.toUrl file))

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
      resetModel model

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
      case model.selectionBox of
      Normal box ->
        ({ model
        | selectionBox = NullBox
        , cursor = Nothing
        , selected = currentSelection model.pieceGroups
        }, Cmd.none)
      Inverted box ->
        ({ model
        | selectionBox = NullBox
        , cursor = Nothing
        , selected = currentSelection model.pieceGroups
        }, Cmd.none)
      NullBox ->
        case model.selected of
          Multiple ->
            ({ model | cursor = Nothing }, Cmd.none)
          NullSelection ->
            ({ model | cursor = Nothing }, Cmd.none)
          Single id ->
            let
              (newPieceGroups, message) =
                D.get id model.pieceGroups
                  |> Maybe.withDefault defaultPieceGroup
                  |> snapToNeighbour model

              cmd =
                case message of
                  Just newContour -> drawPieceGroup newContour
                  Nothing -> Cmd.none
            in
            ({ model
              | cursor = Nothing
              , selected = NullSelection
              , pieceGroups = newPieceGroups
            }, cmd)

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


snapToNeighbour : Model -> PieceGroup -> (D.Dict Int PieceGroup, Maybe SingleContourMessage)
snapToNeighbour model selected =
  let
    neighbourDistance : PieceGroup -> (Float, PieceGroup)
    neighbourDistance neighbour =
      ( Point.dist selected.position neighbour.position
      , neighbour)

    neighbourFromId : Int -> PieceGroup
    neighbourFromId id =
      Maybe.withDefault defaultPieceGroup
        <| D.get id model.pieceGroups

    distanceToSelected : List (Float, PieceGroup)
    distanceToSelected =
      List.map (neighbourDistance << neighbourFromId) (S.toList selected.neighbours)

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

        newPieceGroups =
          merge selected neighbour
            |> Util.flip (D.insert neighbour.id) model.pieceGroups
            |> D.remove selected.id
            |> D.map replaceSelectedIdWithNeighbourId

        updatedNeighbour =
          D.get neighbour.id newPieceGroups |> Maybe.withDefault defaultPieceGroup

      in
        ( newPieceGroups
        , Just <|
          singleContourMessage
            model.image.xpieces
            model.image.ypieces
            model.edgePoints
            updatedNeighbour
        )

    Nothing ->
      (model.pieceGroups, Nothing)


allSelectedPieceGroups pieceGroups =
  D.filter (\_ pg -> pg.isSelected) pieceGroups

currentSelection : D.Dict Int PieceGroup -> Selected
currentSelection pieceGroups =
  case D.keys <| allSelectedPieceGroups pieceGroups of
    [] -> NullSelection
    id :: [] -> Single id
    _ -> Multiple

cheat : Model -> (Model, Maybe SingleContourMessage)
cheat model =
  let
    (randomPieceGroup, seed1) =
      case Random.step (Random.Extra.sample <| D.values model.pieceGroups) model.seed of
        (Nothing, seed) -> (defaultPieceGroup, seed)
        (Just pieceGroup, seed) -> (pieceGroup, seed)

    (randomNeighbourId, seed2) =
      case (Random.step (Random.Set.sample randomPieceGroup.neighbours) seed1) of
        (Nothing, seed) -> (0, seed)
        (Just id, seed) -> (id, seed)

    randomNeighbour =
      D.get randomNeighbourId model.pieceGroups
        |> Maybe.withDefault defaultPieceGroup

    (newPieceGroups, newContours) =
      snapToNeighbour model { randomPieceGroup | position = randomNeighbour.position}
  in
  if D.size model.pieceGroups > 1 then
    ( { model | seed = seed2, pieceGroups = newPieceGroups }
    , newContours)
  else
    (model, Nothing)

cheatManyTimes : Int -> Model -> (Model, Cmd Msg)
cheatManyTimes n model =
  let
    (newModel, msgs) = cheatManyTimesHelper n [] model
    cmd = drawPiecesInCanvas msg

    msg =
      { nx = model.image.xpieces
      , ny = model.image.ypieces
      , contours = List.foldl mergeMsg [] msgs
      }

    mergeMsg maybeMessage messages =
      case maybeMessage of
        Nothing -> messages
        Just message -> message.contour :: messages
  in
  (newModel, cmd)


cheatManyTimesHelper : Int -> List (Maybe SingleContourMessage) -> Model -> (Model, List (Maybe SingleContourMessage))
cheatManyTimesHelper n msgs model =
  if n > 0 then
    let
      (newModel, msg) = cheat model
    in
      cheatManyTimesHelper (n - 1) (msg :: msgs) newModel
  else
    (model, msgs)

-- VIEW

view : Model -> Html Msg
view model =
  Html.div
    ( turnOffTheBloodyImageDragging )
    [
      viewMenu model
    , viewOptionsModal model
    , Html.div
      [ Html.Attributes.style "width" <| String.fromInt model.image.width ++ "px"
      , Html.Attributes.style "height" <| String.fromInt model.image.height ++ "px"
      , Html.Attributes.style "position" "absolute"
      , Html.Attributes.style "top" "100px"
      , Html.Attributes.style "left" "0px"
      ]
      (
        (loadingScreen model.loading) ::
        (preloadImage model.image) ::
        (viewDiv model) ++
        (viewSelectionBox model)
      )
    ]

loadingScreen : LoadStatus -> Html Msg
loadingScreen loading =
  let
    display =
      case loading of
        Finished -> "none"
        _ -> "block"

    loadText =
      case loading of
        ParsingImageUrl -> "Parsing Image URL"
        LoadingImage -> "Loading Image"
        DrawingCanvas -> "Drawing canvas"
        Finished -> "Finished loading!"
  in
    Html.h1
    [ Html.Attributes.style "display" display ]
    [ Html.text <| loadText ]

preloadImage image =
  Html.img
  [ Html.Events.on "load" <| Json.Decode.succeed <| FinishedLoading ImageDiv
  , Html.Attributes.id "jigsawImage"
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

getCanvasId : Int -> String
getCanvasId pieceGroupId =
  "canvas-id-" ++ String.fromInt pieceGroupId

viewDiv : Model -> List (Html Msg)
viewDiv model =
  let
    visibility =
      case model.loading of
        Finished -> Html.Attributes.style "display" "block"
        _ -> Html.Attributes.style "display" "none"
    imageWidth = model.image.scale * toFloat model.image.width
    imageHeight = model.image.scale * toFloat model.image.height
    pieceWidth =  imageWidth / toFloat model.image.xpieces
    pieceHeight = imageHeight / toFloat model.image.ypieces

    {-  This is a (not so pretty) workaround to try to trick Elm into making a static collection of
        divs containing a static number of canvas elements. The canvas elements are referred to in
        a JS port, and if Elm remakes any of the canvas elements, even if the IDs are the same,
        it will result in the JS code to refer to the wrong canvas elements and all kinds of weird
        behavior results. Thus, if a piecegroup gets merged and is removed from the dict, it is
        essential that we still tell Elm to keep a similar looking canvas object. We will not display
        it, obviously, but it will still be there, to avoid the bugs. I don't know if there is a
        better way around this. Maybe if Elm had better support for Canvas, there would be something
        I could do...
    -}
    viewPieceGroups =
      let
        pidToArgs pid =
          case D.get pid model.pieceGroups of
            Nothing ->
              { wh = Point 0 0
              , tl = Point 0 0
              , zlevel = "0"
              , id = pid
              , display = "none"
              }
            Just pg ->
              let
                {t, l, r, b} = pieceGroupSize pg.members model.image.xpieces
              in
              { wh = Point ((r - l + 2) * pieceWidth) ((b - t + 2) * pieceHeight)
              , tl = Point (pg.position.x + (l - 0.5) * pieceWidth) (pg.position.y + (t - 0.5) * pieceHeight)
              , zlevel = String.fromInt pg.zlevel
              , id = pid
              , display = if S.member pg.visibilityGroup model.visibleGroups then "block" else "none"
              }

        allPids =
          List.range 0 (model.image.xpieces * model.image.ypieces - 1)

      in
        List.map (pieceGroupDiv << pidToArgs) allPids

    pieceGroupDiv {wh, tl, zlevel, id, display} =
      Html.div
      [
        Html.Attributes.style "z-index" zlevel
      , Html.Attributes.style "position" "absolute"
      ]
      [
        Html.div
        [
          Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "width" <| Point.xToPixel wh
        , Html.Attributes.style "height" <| Point.yToPixel wh
        , Html.Attributes.style "top" <| Point.yToPixel tl
        , Html.Attributes.style "left" <| Point.xToPixel tl
        , Html.Attributes.style "z-index" zlevel
        , Html.Attributes.style "display" display
        ]
        [
          Html.canvas
          [ Html.Attributes.id <| getCanvasId id
          , Html.Attributes.style "position" "absolute"
          , Html.Attributes.style "top" "0px"
          , Html.Attributes.style "left" "0px"
          , Html.Attributes.style "width" <| Point.xToPixel wh
          , Html.Attributes.style "height" <| Point.yToPixel wh
          ]
          []
        ]
      ]

  in
    [ Html.div
      ( visibility ::
        turnOffTheBloodyImageDragging )
      ( viewPieceGroups )
    ]


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
        [ Html.Events.onClick <| Cheat 1 ]
        [ Html.text "Cheat 1" ]
      , Html.a
        [ Html.Events.onClick <| Cheat 10 ]
        [ Html.text "Cheat 10" ]
      , Html.a
        [ Html.Events.onClick <| Cheat 100 ]
        [ Html.text "Cheat 100" ]
      , Html.a
        [ Html.Events.onClick <| SetOptionsVisibility True ]
        [ Html.text "[TODO] Options" ]
      ]
    ]
  ]


viewOptionsModal : Model -> Html Msg
viewOptionsModal model =
  let
    foobar =
      Debug.log "..." (pieceSettings 1024 768 30 0.1)
  in
  Html.div
  [ Html.Attributes.class "options-modal"
  , Html.Attributes.style "display" <| if model.viewOptions then "block" else "none"
--  , Html.Attributes.style "flex-direction" "row"
  ]
  [
    Html.div
    [ Html.Attributes.class "options-content" ]
    [
      Html.span
      [ Html.Attributes.class "options-close"
      , Html.Events.onClick <| SetOptionsVisibility False
      ]
      [ Html.text "×" ]
    , Html.h2
      []
      [ Html.text "Jigsaw options" ]
    , Html.button
      []
      [ Html.text "Choose image" ]
    , Html.button
      []
      [ Html.text "Choose dimensions " ]
    , Html.button
      []
      [ Html.text "Start" ]
    , Html.p
      []
      [ Html.text "hej!" ]
    ]
  ]

type alias ImageSetting =
  {
    n : Int
  , nx : Int
  , ny : Int
  }


pieceSettings : Int -> Int -> Int -> Float -> A.Array ImageSetting
pieceSettings w h n th =
  let
    r = toFloat w / toFloat h
    dist (x, y) =
      abs (r * toFloat y / toFloat x - 1)

  in
    List.Extra.lift2 (\x y -> (x, y)) (List.range 1 n) (List.range 1 n)
      |> List.filter ((>) th << dist)
      |> List.map (\(x, y) -> {n = x*y, nx = x, ny = y})
      |> A.fromList


-- PORTS
port drawPiecesInCanvas : MultipleContoursMessage -> Cmd msg
port canvasDrawn : ((Int, Int) -> msg) -> Sub msg
port drawPieceGroup : SingleContourMessage -> Cmd msg