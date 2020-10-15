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
import PieceGroup exposing (Group)
import Point exposing (Point)
import Random
import Seeded exposing (Seeded(..))
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
    { edges : List (List Edge)
    , image : JigsawImage
    , db : DB
    , ui : UI
    }


init : () -> Seeded NewModel
init () =
    let
        ( width, height ) =
            ( 1920 * 2, 1080 * 2 )

        ( xpieces, ypieces ) =
            ( 16 * 4, 9 * 4 )

        pieceWidth =
            width // xpieces

        pieceHeight =
            height // ypieces

        image =
            { path = "ship-1366926_crop_4k.png"
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
    -> List Group
    -> List (List Edge)
    -> NewModel
buildModel image pieceGroups edges =
    { image = image
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
            Random.map (PieceGroup.createGroups image) generatePositions

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
