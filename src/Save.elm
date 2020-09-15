module Save exposing (Save, decode, encode, load, save)

import DB
import Json.Decode
import Json.Encode
import Model exposing (NewModel)
import PieceGroup exposing (ID)
import Point exposing (Point)
import Seeded exposing (Seeded)


type alias Save =
    List (List Point)


decode : Json.Decode.Decoder Save
decode =
    let
        pack : List Int -> List Point
        pack =
            Tuple.second << List.foldl do ( Nothing, [] )

        do : Int -> ( Maybe Int, List Point ) -> ( Maybe Int, List Point )
        do a ( m, l ) =
            case m of
                Nothing ->
                    ( Just a, l )

                Just b ->
                    ( Nothing, Point b a :: l )
    in
    Json.Decode.list Json.Decode.int
        |> Json.Decode.map pack
        |> Json.Decode.list


encode : Save -> Json.Encode.Value
encode s =
    s
        |> (List.map <| List.concatMap (\p -> [ p.x, p.y ]))
        |> (Json.Encode.list <| Json.Encode.list Json.Encode.int)


load : Save -> Seeded NewModel
load s =
    let
        newModel =
            Model.init ()

        configuration =
            (Seeded.unwrap newModel).configuration

        image =
            configuration.image

        createPG ( position, id ) =
            PieceGroup.createPieceGroup image id position

        db =
            s
                |> List.indexedMap
                    (\x l ->
                        List.indexedMap
                            (\y p -> ( p, ( x, y ) ))
                            l
                    )
                |> List.concat
                |> List.map createPG
                |> DB.makeDb
    in
    newModel
        |> Seeded.map
            (\m ->
                { m
                    | db = db
                }
            )


save : Seeded NewModel -> Save
save model =
    let
        do : ( PieceGroup.ID, Point ) -> List (List Point) -> List (List Point)
        do ( ( _, y ), point ) matrix =
            case ( y, matrix ) of
                ( 0, _ ) ->
                    [ point ] :: matrix

                ( _, row :: rows ) ->
                    (point :: row) :: rows

                ( _, [] ) ->
                    [ [ point ] ]
    in
    Seeded.unwrap model
        |> .db
        |> DB.getPieces
        |> List.sortBy Tuple.first
        |> List.foldl do []
        |> List.reverse
