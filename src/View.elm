module View exposing (view)

import Edge exposing (Edge)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (height, style, width)
import Html.Events
import Html.Keyed
import Html.Lazy
import JigsawImage exposing (JigsawImage, pieceIdToOffset)
import Model
    exposing
        ( Box
        , Msg(..)
        , NewModel(..)
        , SelectionBox(..)
        , boxBottomRight
        , boxTopLeft
        , getEdges
        , getImage
        , getVisibilityGroups
        , toOldModel
        )
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy
import SvgUtil


view : NewModel -> Html Msg
view model =
    let
        image =
            getImage model

        oldModel =
            toOldModel model
    in
    Html.div
        turnOffTheBloodyImageDragging
        [ viewClipPath model
        , Html.button
            [ Html.Events.onClick Scramble ]
            [ Html.text "scramble" ]
        , Html.div
            [ style "width" <| String.fromInt image.width ++ "px"
            , style "height" <| String.fromInt image.height ++ "px"
            , style "position" "absolute"
            , style "top" "100px"
            , style "left" "0px"
            ]
            (viewDiv model ++ viewSelectionBox oldModel.selectionBox)
        ]


turnOffTheBloodyImageDragging : List (Attribute msg)
turnOffTheBloodyImageDragging =
    [ style "-webkit-user-select" "none"
    , style "-khtml-user-select" "none"
    , style "-moz-user-select" "none"
    , style "-o-user-select" "none"
    , style "user-select" "none"
    , Html.Attributes.draggable "false"
    ]


lazyDivSelectionBox : Box -> String -> Svg.Svg msg
lazyDivSelectionBox =
    Svg.Lazy.lazy2 divSelectionBox


divSelectionBox : Box -> String -> Svg.Svg msg
divSelectionBox box color =
    let
        topLeft =
            boxTopLeft box

        bottomRight =
            boxBottomRight box

        top =
            max 100 topLeft.y
    in
    Html.div
        ([ style "width" <| String.fromInt (bottomRight.x - topLeft.x) ++ "px"
         , style "height" <| String.fromInt (bottomRight.y - top) ++ "px"
         , style "background-color" color
         , style "border-style" "dotted"
         , style "top" <| String.fromInt (top - 100) ++ "px"
         , style "left" <| String.fromInt topLeft.x ++ "px"
         , style "position" "absolute"
         ]
            ++ turnOffTheBloodyImageDragging
        )
        []


viewSelectionBox : SelectionBox -> List (Html msg)
viewSelectionBox selectionBox =
    let
        hidden =
            { staticCorner = { x = -10, y = -10 }
            , movingCorner = { x = -10, y = -10 }
            , selectedIds = Set.empty
            }
    in
    case selectionBox of
        Normal box ->
            [ lazyDivSelectionBox box "rgba(0,0,255,0.2)" ]

        Inverted box ->
            [ lazyDivSelectionBox box "rgba(0,255,0,0.2)" ]

        NullBox ->
            [ lazyDivSelectionBox hidden "rgba(0,255,0,0.2)" ]


lazyPieceDiv : JigsawImage -> PieceGroup -> Int -> Html msg
lazyPieceDiv =
    Html.Lazy.lazy3 pieceDiv


pieceDiv : JigsawImage -> PieceGroup -> Int -> Html msg
pieceDiv image pg pid =
    let
        offset =
            pieceIdToOffset image pid

        w =
            floor <| image.scale * toFloat (2 * image.width // image.xpieces)

        h =
            floor <| image.scale * toFloat (2 * image.height // image.ypieces)

        top =
            String.fromInt (pg.position.y + offset.y - h // 4) ++ "px"

        left =
            String.fromInt (pg.position.x + offset.x - w // 4) ++ "px"
    in
    Html.div
        [ style "width" <| String.fromInt w ++ "px"
        , style "height" <| String.fromInt h ++ "px"
        , style "clipPath" <| clipPathRef pid
        , style "background-image" <| "url('" ++ image.path ++ "')"
        , style "background-size" <|
            String.fromInt (floor <| image.scale * toFloat image.width)
                ++ "px "
                ++ String.fromInt (floor <| image.scale * toFloat image.height)
                ++ "px"
        , style "background-position" <|
            String.fromInt (w // 4 - offset.x)
                ++ "px "
                ++ String.fromInt (h // 4 - offset.y)
                ++ "px"
        , style "position" "absolute"
        , style "transform" ("translate(" ++ left ++ "," ++ top ++ ")")
        ]
        []


shadow : String -> List (Html msg) -> Html msg
shadow color =
    Html.div
        [ style "filter" <|
            "drop-shadow(0px 0px 2px "
                ++ color
                ++ ")"
        ]


keyedDiv : List (Attribute msg) -> List ( String, Html msg ) -> Html msg
keyedDiv =
    Html.Keyed.node "div"


px : Int -> String
px int =
    String.fromInt int ++ "px"


viewClipPath : NewModel -> Html Msg
viewClipPath model =
    let
        image =
            getImage model

        edges =
            getEdges model
    in
    lazyclipPathDefs image edges


viewDiv : NewModel -> List (Html Msg)
viewDiv model =
    let
        image =
            getImage model

        visibleGroups =
            getVisibilityGroups model

        isVisible pieceGroup =
            Set.member
                pieceGroup.visibilityGroup
                visibleGroups

        shadowWithColor : String -> List ( String, Html msg ) -> List ( String, Html msg )
        shadowWithColor color keyedPieces =
            keyedPieces
                |> List.map (Tuple.mapSecond (shadow color << List.singleton))

        shadowBlack =
            shadowWithColor "black"

        shadowRed =
            shadowWithColor "red"
    in
    case model of
        Moving { selected, unSelected, current, start } ->
            let
                { x, y } =
                    Point.sub current start
            in
            [ keyedDiv
                []
                (unSelected
                    |> List.filter isVisible
                    |> renderPieces image
                    |> shadowBlack
                )
            , keyedDiv
                [ style
                    "transform"
                    ("translate(" ++ (x |> px) ++ "," ++ (y |> px) ++ ")")
                ]
                (selected
                    |> renderPieces image
                    |> shadowRed
                )
            ]

        SelectingWithBox { unSelected, alreadySelected, within } ->
            [ keyedDiv []
                (unSelected
                    |> List.filter isVisible
                    |> List.filter (\x -> not <| List.member x within)
                    |> renderPieces image
                    |> shadowBlack
                )
            , keyedDiv []
                (alreadySelected
                    |> List.append within
                    |> renderPieces image
                    |> shadowRed
                )
            ]

        DeselectingWithBox { unSelected, alreadySelected, within } ->
            [ keyedDiv []
                (unSelected
                    |> List.filter isVisible
                    |> List.filter (\x -> not <| List.member x within)
                    |> renderPieces image
                    |> shadowBlack
                )
            , keyedDiv []
                (alreadySelected
                    |> List.append within
                    |> renderPieces image
                    |> shadowRed
                )
            ]

        Identity { unSelected, selected } ->
            [ keyedDiv []
                (unSelected
                    |> List.filter isVisible
                    |> renderPieces image
                    |> shadowBlack
                )
            , keyedDiv []
                (selected
                    |> renderPieces image
                    |> shadowRed
                )
            ]


renderPieces : JigsawImage -> List PieceGroup -> List ( String, Html msg )
renderPieces image visiblePieces =
    let
        pieceGroupDiv : PieceGroup -> List ( String, Html msg )
        pieceGroupDiv pg =
            let
                render pid =
                    ( "piece-" ++ String.fromInt pid
                    , lazyPieceDiv image pg pid
                    )
            in
            List.map render pg.members
    in
    visiblePieces
        |> List.map pieceGroupDiv
        |> List.concat


lazyclipPathDefs : JigsawImage -> List (List Edge) -> Html msg
lazyclipPathDefs =
    Svg.Lazy.lazy2
        (\image edges ->
            definePieceClipPaths image edges
                |> Svg.defs []
                |> List.singleton
                |> Svg.svg [ width 0, height 0 ]
        )


definePieceClipPaths : JigsawImage -> List (List Edge) -> List (Svg msg)
definePieceClipPaths image edges =
    List.map2 (piecePath image) edges (List.range 0 (image.xpieces * image.ypieces - 1))


piecePath : JigsawImage -> List Edge -> Int -> Svg msg
piecePath image edges id =
    let
        w =
            image.scale * toFloat (image.width // image.xpieces)

        h =
            image.scale * toFloat (image.height // image.ypieces)

        offset =
            Point (floor (w / 2)) (floor (h / 2))

        curve =
            SvgUtil.pieceToSvg edges

        move =
            "translate(" ++ Point.toString offset ++ ") "

        scale =
            "scale(" ++ String.fromFloat (w / 200.0) ++ " " ++ String.fromFloat (h / 200.0) ++ ")"
    in
    Svg.clipPath
        [ Svg.Attributes.id <| pieceClipId id ]
        [ Svg.path
            [ Svg.Attributes.d curve
            , Svg.Attributes.transform <| move ++ scale
            ]
            []
        ]


pieceClipId : Int -> String
pieceClipId id =
    "piece-" ++ String.fromInt id ++ "-clip"


clipPathRef : Int -> String
clipPathRef id =
    "url(#" ++ pieceClipId id ++ ")"
