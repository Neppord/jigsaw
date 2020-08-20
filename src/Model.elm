module Model exposing
    ( Box
    , Key(..)
    , Keyboard
    , Model
    , Msg(..)
    , NewModel(..)
    , Selected(..)
    , SelectionBox(..)
    , boxBottomRight
    , boxTopLeft
    , defaultPieceGroup
    , init
    , resetModel
    , toNewModel
    , toOldModel
    )

import Dict
import Edge exposing (Edge, generateEdgePoints)
import JigsawImage
    exposing
        ( JigsawImage
        , createPieceGroups
        , shufflePiecePositions
        )
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Random
import Random.List
import Set as S
import Html.Attributes exposing (selected)
import List


type Msg
    = MouseDown Point Keyboard
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
    , pieceGroups : Dict.Dict Int PieceGroup
    , selected : Selected
    , maxZLevel : Int
    , image : JigsawImage
    , width : Int
    , height : Int
    , snapDistance : Float
    , selectionBox : SelectionBox
    , seed : Random.Seed
    , edges : List (List Edge)
    , visibleGroups : S.Set Int
    , keyboard : Keyboard
    }


type NewModel
    = Identity Model
    | Moving
        { oldModel : Model
        , start : Point
        , current : Point
        , selected : List PieceGroup
        , unSelected : Dict.Dict Int PieceGroup
        }


toNewModel : Model -> NewModel
toNewModel oldModel =
    case ( oldModel.cursor, oldModel.selectionBox ) of
        ( Just current, NullBox ) ->
            let
                (selected, unSelected) =
                    Dict.partition (always .isSelected) oldModel.pieceGroups
            in
            Moving
                { oldModel = oldModel
                , start = current
                , current = current
                , selected = Dict.values selected
                , unSelected = unSelected
                }

        ( _, _ ) ->
            Identity oldModel


toOldModel : NewModel -> Model
toOldModel newModel =
    case newModel of
        Identity oldModel ->
            oldModel

        Moving { oldModel, start, current, selected, unSelected } ->
            {oldModel
                | pieceGroups =
                    selected
                        |> List.map (PieceGroup.move (Point.sub current start))
                        |> List.map (\pg -> (pg.id, pg))
                        |> Dict.fromList
                        |> Dict.union unSelected
                , cursor = Just current
            }


init : () -> ( Model, Cmd Msg )
init () =
    let
        image =
            { path = "../resources/kitten.png"
            , width = 533
            , height = 538
            , xpieces = 4
            , ypieces = 4
            , scale = 1.0
            }

        model =
            resetModel image (Random.initialSeed 0)
    in
    ( model, Cmd.none )


resetModel : JigsawImage -> Random.Seed -> Model
resetModel image seed =
    let
        ( w, h ) =
            ( image.width, image.height )

        ( nx, ny ) =
            ( image.xpieces, image.ypieces )

        numberOfEdges =
            2 * nx * ny - nx - ny

        ( positions, seed1 ) =
            Random.step (shufflePiecePositions w h image) seed

        ( zlevels, seed2 ) =
            shuffleZLevels (nx * ny) seed1

        ( edgePoints, seed3 ) =
            Random.step (generateEdgePoints numberOfEdges) seed2
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
    , edges =
        List.range 0 (image.xpieces * image.ypieces - 1)
            |> List.map (\id -> Edge.pieceEdges image.xpieces image.ypieces id edgePoints)
    , visibleGroups = S.fromList [ -1 ]
    , keyboard = { shift = False, ctrl = False }
    }


shuffleZLevels : Int -> Random.Seed -> ( List Int, Random.Seed )
shuffleZLevels n seed =
    Random.step (Random.List.shuffle <| List.range 0 (n - 1)) seed


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


type Key
    = Number Int
    | Control
    | Shift
    | Other


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
