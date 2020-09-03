module Model exposing
    ( Key(..)
    , Msg(..)
    , NewModel
    , generateModel
    , init
    )

import DB exposing (DB)
import Edge exposing (Edge, generateEdgePoints)
import JigsawImage
    exposing
        ( JigsawImage
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
    | ChangeImageUrl String


type alias Configuration =
    { snapDistance : Float
    , image : JigsawImage
    }


type alias NewModel =
    { visibleGroups : Set Int
    , edges : List (List Edge)
    , configuration : Configuration
    , db : DB
    , ui : UI
    }


init : () -> ( Seeded NewModel, Cmd Msg )
init () =
    let
        scale =
            1.0

        ( width, height ) =
            ( 533, 538 )

        ( xpieces, ypieces ) =
            ( 20, 20 )

        pieceWidth =
            floor <| scale * (toFloat width / xpieces)

        pieceHeight =
            floor <| scale * (toFloat height / ypieces)

        image =
            { path = "kitten.png"
            , width = width
            , height = height
            , xpieces = xpieces
            , ypieces = ypieces
            , scale = scale
            , pieceWidth = pieceWidth
            , pieceHeight = pieceHeight
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
    -> List PieceGroup
    -> List (List Edge)
    -> NewModel
buildModel image pieceGroups edges =
    { configuration =
        { snapDistance = 30.0
        , image = image
        }
    , visibleGroups = S.fromList [ -1 ]
    , edges = edges
    , db = DB.makeDb pieceGroups
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
            Random.map (PieceGroup.createPieceGroups image) generatePositions

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
