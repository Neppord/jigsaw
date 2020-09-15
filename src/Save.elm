module Save exposing
    ( Save
    , deserialize
    , load
    , save
    , serialize
    )

import DB
import Json.Decode
import Json.Encode
import Model exposing (NewModel)
import PieceGroup exposing (ID)
import Point exposing (Point)
import Seeded exposing (Seeded)


type alias Save =
    List (List Point)


serialize : Save -> String
serialize =
    Json.Encode.encode 0 << jsonEncode


deserialize : String -> Maybe Save
deserialize =
    Json.Decode.decodeString jsonDecode >> Result.toMaybe


pack : List Point -> List Int
pack =
    List.concatMap (\p -> [ p.x, p.y ])


unpack : List Int -> List Point
unpack =
    let
        do : Int -> ( Maybe Int, List Point ) -> ( Maybe Int, List Point )
        do a ( m, l ) =
            case m of
                Nothing ->
                    ( Just a, l )

                Just b ->
                    ( Nothing, Point b a :: l )
    in
    Tuple.second << List.foldl do ( Nothing, [] )


jsonDecode : Json.Decode.Decoder Save
jsonDecode =
    Json.Decode.list Json.Decode.int
        |> Json.Decode.map unpack
        |> Json.Decode.list


jsonEncode : Save -> Json.Encode.Value
jsonEncode s =
    s
        |> (List.map <| pack)
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
