module View exposing (view)

import DB
import Drag
import Edge exposing (Edge)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (height, style, width)
import Html.Events
import Html.Keyed
import Html.Lazy
import JigsawImage exposing (JigsawImage)
import Model
    exposing
        ( Msg(..)
        , NewModel
        )
import PieceGroup exposing (PieceGroup)
import Point exposing (Point)
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy
import SvgUtil
import UI


view : NewModel -> Html Msg
view model =
    let
        image =
            model.configuration.image
    in
    Html.div
        turnOffTheBloodyImageDragging
        [ viewClipPath model
        , Html.div
            [ style "width" <| String.fromInt image.width ++ "px"
            , style "height" <| String.fromInt image.height ++ "px"
            , style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            ]
            (viewDiv model)
        , viewSelectionBox model
        , Html.div
            [ style "position" "absolute"
            , style "top" "0"
            , style "left" "0"
            ]
            [ Html.button
                [ Html.Events.onClick Scramble ]
                [ Html.text "scramble" ]
            , Html.input
                [ Html.Attributes.placeholder "Image Url"
                , Html.Attributes.value model.configuration.image.path
                , Html.Events.onInput ChangeImageUrl
                ]
                []
            ]
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


viewSelectionBox : NewModel -> Html msg
viewSelectionBox model =
    let
        box x y w h color =
            Html.div
                ([ style "width" <| String.fromInt w ++ "px"
                 , style "height" <| String.fromInt h ++ "px"
                 , style "background-color" color
                 , style "border-style" "dotted"
                 , style "top" <| String.fromInt y ++ "px"
                 , style "left" <| String.fromInt x ++ "px"
                 , style "position" "absolute"
                 , style "will-change" "width"
                 , style "will-change" "height"
                 , style "will-change" "top"
                 , style "will-change" "left"
                 ]
                    ++ turnOffTheBloodyImageDragging
                )
                []
    in
    case model.ui of
        UI.Boxing mode drag ->
            let
                { x, y, w, h } =
                    Drag.getDimensions drag
            in
            box x
                y
                w
                h
                (case mode of
                    UI.Replace ->
                        "rgba(0,0,255,0.2)"

                    UI.Add ->
                        "rgba(0,0,255,0.2)"

                    UI.Remove ->
                        "rgba(0,255,0,0.2)"
                )

        _ ->
            box -10 -10 0 0 "rgba(0,255,0,0.2)"


lazyPieceDiv : JigsawImage -> PieceGroup -> PieceGroup.ID -> Html msg
lazyPieceDiv =
    Html.Lazy.lazy3 pieceDiv


pieceDiv : JigsawImage -> PieceGroup -> PieceGroup.ID -> Html msg
pieceDiv image pg ( x, y ) =
    let
        ( dx, dy ) =
            ( x * image.pieceWidth, y * image.pieceHeight )

        borderHeight =
            image.pieceHeight // 2

        borderWidth =
            image.pieceWidth // 2

        top =
            String.fromInt (pg.position.y + dy - borderHeight) ++ "px"

        left =
            String.fromInt (pg.position.x + dx - borderWidth) ++ "px"

        width =
            borderWidth + image.pieceWidth + borderWidth

        height =
            borderHeight + image.pieceHeight + borderHeight
    in
    Html.div
        [ style "width" <| String.fromInt width ++ "px"
        , style "height" <| String.fromInt height ++ "px"
        , style "clipPath" <| clipPathRef ( x, y )
        , style "background-image" <| "url('" ++ image.path ++ "')"
        , style "background-position" <|
            String.fromInt (borderWidth - dx)
                ++ "px "
                ++ String.fromInt (borderWidth - dy)
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
    lazyclipPathDefs model.configuration.image model.edges


viewDiv : NewModel -> List (Html Msg)
viewDiv model =
    let
        { db, visibleGroups } =
            model

        ( selected, unSelected ) =
            ( DB.getSelected db
            , DB.getUnSelected db
            )

        image =
            model.configuration.image

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
    [ keyedDiv
        []
        (unSelected
            |> List.filter isVisible
            |> renderPieces image
            |> shadowBlack
        )
    , keyedDiv
        (case model.ui of
            UI.Moving _ drag ->
                let
                    { x, y } =
                        Drag.distance drag
                in
                [ style
                    "transform"
                    ("translate(" ++ (x |> px) ++ "," ++ (y |> px) ++ ")")
                , style "will-change" "transform"
                ]

            _ ->
                [ style
                    "transform"
                    ("translate(" ++ (0 |> px) ++ "," ++ (0 |> px) ++ ")")
                ]
        )
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
                render ( x, y ) =
                    ( "piece-" ++ String.fromInt x ++ "-" ++ String.fromInt y
                    , lazyPieceDiv image pg ( x, y )
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
    List.map2 (piecePath image) edges (PieceGroup.genIds image.xpieces image.ypieces)


piecePath : JigsawImage -> List Edge -> PieceGroup.ID -> Svg msg
piecePath image edges id =
    let
        w =
            toFloat image.pieceWidth

        h =
            toFloat image.pieceHeight

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


pieceClipId : PieceGroup.ID -> String
pieceClipId ( x, y ) =
    "piece-" ++ String.fromInt x ++ "-" ++ String.fromInt y ++ "-clip"


clipPathRef : PieceGroup.ID -> String
clipPathRef id =
    "url(#" ++ pieceClipId id ++ ")"
