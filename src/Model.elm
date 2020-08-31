module Model exposing
    ( Key(..)
    , Msg(..)
    , NewModel
    , defaultPieceGroup
    , generateModel
    , getEdges
    , getImage
    , getVisibilityGroups
    , init
    )

import Dict
import Edge exposing (Edge, generateEdgePoints)
import JigsawImage
    exposing
        ( JigsawImage
        , createPieceGroups
        , shufflePiecePositions
        )
import Keyboard exposing (Keyboard)
import List
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Random
import Seeded exposing (Seeded(..))
import Set as S exposing (Set)
import UI exposing (UI(..))


type Msg
    = MouseDown Point Keyboard
    | MouseMove Point
    | MouseUp
    | Scramble
    | KeyDown Keyboard (Maybe Key)


type alias Configuration =
    { snapDistance : Float
    , image : JigsawImage
    }


type alias NewModel =
    { visibleGroups : Set Int
    , edges : List (List Edge)
    , configuration : Configuration
    , selected : List PieceGroup
    , unSelected : List PieceGroup
    , ui : UI
    }


init : () -> ( Seeded NewModel, Cmd Msg )
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
    in
    ( image
        |> Seeded (Random.initialSeed 0)
        |> Seeded.map generateModel
        |> Seeded.step
    , Cmd.none
    )


buildModel :
    JigsawImage
    -> Dict.Dict Int PieceGroup
    -> List (List Edge)
    -> NewModel
buildModel image pieceGroups edges =
    { configuration =
        { snapDistance = 30.0
        , image = image
        }
    , visibleGroups = S.fromList [ -1 ]
    , edges = edges
    , selected = []
    , unSelected = Dict.values pieceGroups
    , ui = WaitingForInput
    }


generateModel : JigsawImage -> Random.Generator NewModel
generateModel image =
    let
        numberOfEdges =
            2 * image.xpieces * image.ypieces - image.xpieces - image.ypieces

        generatePositions =
            shufflePiecePositions image.width image.height image

        generatePieceGroups =
            generatePositions
                |> Random.map (createPieceGroups image)

        generateEdges =
            Random.map
                (\eps ->
                    List.range 0 (image.xpieces * image.ypieces - 1)
                        |> List.map (\id -> Edge.pieceEdges image.xpieces image.ypieces id eps)
                )
                (generateEdgePoints numberOfEdges)
    in
    Random.map2 (buildModel image) generatePieceGroups generateEdges


type Key
    = Number Int



-- Until I figure out how to handle index out of bounds
-- exceptions more elegantly


defaultPieceGroup : PieceGroup
defaultPieceGroup =
    { position = Point 0 0
    , isSelected = False
    , id = -10
    , neighbours = S.empty
    , members = []
    , visibilityGroup = -1
    }


getVisibilityGroups : NewModel -> Set Int
getVisibilityGroups =
    .visibleGroups


getEdges : NewModel -> List (List Edge)
getEdges =
    .edges


getImage : NewModel -> JigsawImage
getImage =
    .configuration >> .image
