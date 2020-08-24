module View exposing (view)

import Dict
import Edge exposing (Edge)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy
import JigsawImage exposing (JigsawImage, pieceIdToOffset)
import Model
    exposing
        ( Box
        , Model
        , Msg(..)
        , NewModel(..)
        , SelectionBox(..)
        , boxBottomRight
        , boxTopLeft
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
            oldModel.image

        oldModel =
            toOldModel model
    in
    Html.div
        turnOffTheBloodyImageDragging
        [ Html.button
            [ Html.Events.onClick Scramble ]
            [ Html.text "scramble" ]
        , Html.div
            [ Html.Attributes.style "width" <| String.fromInt image.width ++ "px"
            , Html.Attributes.style "height" <| String.fromInt image.height ++ "px"
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "100px"
            , Html.Attributes.style "left" "0px"
            ]
            (viewSelectionBox (oldModel.maxZLevel + 1) oldModel.selectionBox ++ viewDiv model)
        ]


turnOffTheBloodyImageDragging : List (Attribute msg)
turnOffTheBloodyImageDragging =
    [ Html.Attributes.style "-webkit-user-select" "none"
    , Html.Attributes.style "-khtml-user-select" "none"
    , Html.Attributes.style "-moz-user-select" "none"
    , Html.Attributes.style "-o-user-select" "none"
    , Html.Attributes.style "user-select" "none"
    , Html.Attributes.draggable "false"
    ]


lazyDivSelectionBox : Int -> Box -> String -> Svg.Svg msg
lazyDivSelectionBox =
    Svg.Lazy.lazy3 divSelectionBox


divSelectionBox : Int -> Box -> String -> Svg.Svg msg
divSelectionBox zIndex box color =
    let
        topLeft =
            boxTopLeft box

        bottomRight =
            boxBottomRight box

        top =
            max 100 topLeft.y
    in
    Html.div
        ([ Html.Attributes.style "width" <| String.fromInt (bottomRight.x - topLeft.x) ++ "px"
         , Html.Attributes.style "height" <| String.fromInt (bottomRight.y - top) ++ "px"
         , Html.Attributes.style "background-color" color
         , Html.Attributes.style "border-style" "dotted"
         , Html.Attributes.style "top" <| String.fromInt (top - 100) ++ "px"
         , Html.Attributes.style "left" <| String.fromInt topLeft.x ++ "px"
         , Html.Attributes.style "z-index" <| String.fromInt zIndex
         , Html.Attributes.style "position" "absolute"
         ]
            ++ turnOffTheBloodyImageDragging
        )
        []


viewSelectionBox : Int -> SelectionBox -> List (Html msg)
viewSelectionBox zIndex selectionBox =
    let
        hidden =
            { staticCorner = { x = -10, y = -10 }
            , movingCorner = { x = -10, y = -10 }
            , selectedIds = Set.empty
            }
    in
    case selectionBox of
        Normal box ->
            [ lazyDivSelectionBox zIndex box "rgba(0,0,255,0.2)" ]

        Inverted box ->
            [ lazyDivSelectionBox zIndex box "rgba(0,255,0,0.2)" ]

        NullBox ->
            [ lazyDivSelectionBox zIndex hidden "rgba(0,255,0,0.2)" ]


lazyPieceDiv : JigsawImage -> PieceGroup -> Int -> Int -> Html msg
lazyPieceDiv =
    Html.Lazy.lazy4 pieceDiv


pieceDiv : JigsawImage -> PieceGroup -> Int -> Int -> Html msg
pieceDiv image pg zIndex pid =
    let
        color =
            if pg.isSelected then
                "red"

            else
                "black"
    in
    renderPiece image pg zIndex pid
        |> List.singleton
        |> shadow color


renderPiece : JigsawImage -> PieceGroup -> Int -> Int -> Html msg
renderPiece image pg zIndex pid =
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
        [ Html.Attributes.style "z-index" <| String.fromInt zIndex
        , Html.Attributes.style "width" <| String.fromInt w ++ "px"
        , Html.Attributes.style "height" <| String.fromInt h ++ "px"
        , Html.Attributes.style "clipPath" <| clipPathRef pid
        , Html.Attributes.style "background-image" <| "url('" ++ image.path ++ "')"
        , Html.Attributes.style "background-size" <|
            String.fromInt (floor <| image.scale * toFloat image.width)
                ++ "px "
                ++ String.fromInt (floor <| image.scale * toFloat image.height)
                ++ "px"
        , Html.Attributes.style "background-position" <|
            String.fromInt (w // 4 - offset.x)
                ++ "px "
                ++ String.fromInt (h // 4 - offset.y)
                ++ "px"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "transform" ("translate(" ++ left ++ "," ++ top ++ ")")
        ]
        []


shadow : String -> List (Html msg) -> Html msg
shadow color =
    Html.div
        [ Html.Attributes.style "filter" <|
            "drop-shadow(0px 0px 2px "
                ++ color
                ++ ")"
        ]


viewDiv : NewModel -> List (Html Msg)
viewDiv model =
    case model of
        Moving { selected, unSelected, current, start } ->
            let
                offset =
                    Point.sub current start

                top =
                    String.fromInt offset.y ++ "px"

                left =
                    String.fromInt offset.x ++ "px"

                oldModel =
                    toOldModel model

                edges =
                    oldModel.edges

                image =
                    oldModel.image

                visibleGroups =
                    oldModel.visibleGroups

                pieceGroupDiv : PieceGroup -> List ( String, Html msg )
                pieceGroupDiv pg =
                    let
                        render pid =
                            ( "piece-" ++ String.fromInt pid
                            , lazyPieceDiv image pg pg.zlevel pid
                            )
                    in
                    List.map render pg.members

                renderPieces pieces =
                    pieces
                        |> List.filter
                            (\pieceGroup ->
                                Set.member
                                    pieceGroup.visibilityGroup
                                    visibleGroups
                            )
                        |> List.map pieceGroupDiv
                        |> List.concat
            in
            [ Html.Keyed.node
                "div"
                []
                (renderPieces unSelected)
            , Html.Keyed.node
                "div"
                [ Html.Attributes.style
                    "transform"
                    ("translate(" ++ left ++ "," ++ top ++ ")")
                ]
                (renderPieces selected)
            , lazyclipPathDefs image edges
            ]

        SelectingWithBox _ ->
            oldViewDiv (toOldModel model)

        DeselectingWithBox _ ->
            oldViewDiv (toOldModel model)

        Identity { oldModel } ->
            oldViewDiv oldModel


oldViewDiv : Model -> List (Html Msg)
oldViewDiv model =
    let
        pieceGroupDiv : PieceGroup -> List ( String, Html msg )
        pieceGroupDiv pg =
            let
                render pid =
                    ( "piece-" ++ String.fromInt pid
                    , lazyPieceDiv model.image pg pg.zlevel pid
                    )
            in
            List.map render pg.members

        viewPieces =
            model.pieceGroups
                |> Dict.values
                |> List.filter
                    (\pieceGroup -> Set.member pieceGroup.visibilityGroup model.visibleGroups)
                |> List.map pieceGroupDiv
                |> List.concat
    in
    [ Html.Keyed.node
        "div"
        []
        viewPieces
    , lazyclipPathDefs model.image model.edges
    ]


lazyclipPathDefs : JigsawImage -> List (List Edge) -> Html msg
lazyclipPathDefs =
    Svg.Lazy.lazy2
        (\image edges ->
            definePieceClipPaths image edges
                |> Svg.defs []
                |> List.singleton
                |> Svg.svg []
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
            [ Svg.Attributes.id <| pieceOutlineId id
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
