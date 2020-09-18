module View exposing (view)

import DB
import Drag
import Edge exposing (Edge)
import Html exposing (Attribute, Html, text)
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
            model.image
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
            [ style "position" "fixed"
            , style "top" "0"
            , style "left" "0"
            , style "background-color" "rgba(100%, 100%, 100%, 50%)"
            ]
            [ visibilityCheckbox 1 model
            , visibilityLabel 1
            , visibilityCheckbox 2 model
            , visibilityLabel 2
            , visibilityCheckbox 3 model
            , visibilityLabel 3
            , visibilityCheckbox 4 model
            , visibilityLabel 4
            , visibilityCheckbox 5 model
            , visibilityLabel 5
            , visibilityCheckbox 6 model
            , visibilityLabel 6
            , visibilityCheckbox 7 model
            , visibilityLabel 7
            , visibilityCheckbox 8 model
            , visibilityLabel 8
            , visibilityCheckbox 9 model
            , visibilityLabel 9
            ]
        , Html.div
            [ style "position" "fixed"
            , style "bottom" "0"
            , style "left" "0"
            , style "background-color" "rgba(100%, 100%, 100%, 50%)"
            ]
            [ renderStats model ]
        ]


visibilityGroupName : Int -> String
visibilityGroupName x =
    "group-" ++ String.fromInt x ++ "-visible"


visibilityLabel : Int -> Html msg
visibilityLabel x =
    Html.label
        [ Html.Attributes.for <| visibilityGroupName x ]
        [ text <| String.fromInt x ]


visibilityCheckbox : Int -> NewModel -> Html Msg
visibilityCheckbox x model =
    Html.input
        [ Html.Attributes.type_ "checkbox"
        , Html.Attributes.checked (Set.member x model.db.visibleGroups)
        , Html.Attributes.name <| visibilityGroupName x
        , Html.Events.onCheck (always <| ToggleVisibility x)
        ]
        []


renderStats : NewModel -> Html msg
renderStats { db, image } =
    let
        item string =
            string
                |> text
                |> List.singleton
                |> Html.li []

        size =
            image.xpieces * image.ypieces

        perf =
            String.fromFloat
                (toFloat (DB.height db) / toFloat (DB.optimalHeight db))
    in
    Html.ul
        [ style "margin" "0"
        ]
        [ item <| "puzzle size: " ++ String.fromInt size
        , item <| "pieces left: " ++ String.fromInt (DB.size db)
        , item <| "seek time: " ++ String.fromInt (DB.height db)
        , item <| "seek diff time: " ++ String.fromInt (DB.heightDifference db)
        , item <| "optimal seek time: " ++ String.fromInt (DB.optimalHeight db)
        , item <| "perf: " ++ perf
        , item <| "deleted nodes: " ++ String.fromInt (DB.countDeleted db)
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


lazyPieceDiv : String -> PieceGroup -> PieceGroup.Piece -> Html msg
lazyPieceDiv =
    Html.Lazy.lazy3 pieceDiv


pieceDiv : String -> PieceGroup -> PieceGroup.Piece -> Html msg
pieceDiv backgroundUrl pg piece =
    let
        ( borderWidth, borderHeight ) =
            ( piece.size.x // 2
            , piece.size.y // 2
            )

        width =
            borderWidth + piece.size.x + borderWidth

        height =
            borderHeight + piece.size.y + borderHeight

        ( left, top ) =
            pg.position
                |> Point.add piece.offset
                |> Point.add (Point -borderWidth -borderHeight)
                |> Point.toPair

        ( bgLeft, bgTop ) =
            piece.offset
                |> Point.sub (Point borderWidth borderHeight)
                |> Point.toPair

        translate =
            "translate(" ++ String.fromInt left ++ "px," ++ String.fromInt top ++ "px)"
    in
    Html.div
        [ style "width" <| String.fromInt width ++ "px"
        , style "height" <| String.fromInt height ++ "px"
        , style "clipPath" <| clipPathRef piece.id
        , style "background-image" <| "url('" ++ backgroundUrl ++ "')"
        , style "background-position" (String.fromInt bgLeft ++ "px " ++ String.fromInt bgTop ++ "px")
        , style "position" "absolute"
        , style "transform" translate
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
    lazyclipPathDefs model.image model.edges


viewDiv : NewModel -> List (Html Msg)
viewDiv model =
    let
        { db } =
            model

        ( selected, unSelected ) =
            ( DB.getSelected db
            , DB.getVisibleUnSelected db
            )

        image =
            model.image

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
                render piece =
                    let
                        ( x, y ) =
                            piece.id
                    in
                    ( "piece-" ++ String.fromInt x ++ "-" ++ String.fromInt y
                    , lazyPieceDiv image.path pg piece
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
