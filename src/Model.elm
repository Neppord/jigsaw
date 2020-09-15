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
    | SendToVisibility Int
    | ToggleVisibility Int
    | ChangeImageUrl String


type alias NewModel =
    { visibleGroups : Set Int
    , edges : List (List Edge)
    , image : JigsawImage
    , db : DB
    , ui : UI
    }


init : () -> Seeded NewModel
init () =
    let
        ( width, height ) =
            ( 1920, 1080 )

        ( xpieces, ypieces ) =
            ( 20, 20 )

        pieceWidth =
            width // xpieces

        pieceHeight =
            height // ypieces

        image =
            { path = "ship-1366926_1920.jpg"
            , width = width
            , height = height
            , xpieces = xpieces
            , ypieces = ypieces
            , pieceWidth = pieceWidth
            , pieceHeight = pieceHeight
            }
    in
    image
        |> Seeded (Random.initialSeed 0)
        |> Seeded.map generateModel
        |> Seeded.step


buildModel :
    JigsawImage
    -> List PieceGroup
    -> List (List Edge)
    -> NewModel
buildModel image pieceGroups edges =
    { image = image
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
                    PieceGroup.genIds image.xpieces image.ypieces
                        |> List.map (\id -> Edge.pieceEdges image.xpieces image.ypieces id eps)
                )
                (generateEdgePoints numberOfEdges)
    in
    Random.map2 (buildModel image) generatePieceGroups generateEdges


type Key
    = Number Int
