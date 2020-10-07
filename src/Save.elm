module Save exposing
    ( Save
    , deserialize
    , load
    , save
    , serialize
    )

import Base64
import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (fail)
import Bytes.Encode
import DB
import Model exposing (NewModel)
import PieceGroup
import Point exposing (Point)
import Seeded exposing (Seeded)


type alias Save =
    List (List Point)


serialize : Save -> Maybe String
serialize save_ =
    Bytes.Encode.encode (byteEncoder save_)
        |> Base64.fromBytes


deserialize : String -> Maybe Save
deserialize string =
    string
        |> Base64.toBytes
        |> Maybe.andThen (Bytes.Decode.decode byteDecode)


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


byteDecodeListAndLength : Bytes.Decode.Decoder Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder (List a)
byteDecodeListAndLength length a =
    length
        |> Bytes.Decode.andThen (\len -> byteDecodeList len a)


byteDecodeList : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder (List a)
byteDecodeList len a =
    let
        listStep :
            Bytes.Decode.Decoder a
            -> ( Int, List a )
            -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List a ) (List a))
        listStep decoder ( n, xs ) =
            if n <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse xs))

            else
                Bytes.Decode.map (\x -> Bytes.Decode.Loop ( n - 1, x :: xs )) decoder
    in
    Bytes.Decode.loop ( len, [] ) (listStep a)


byteDecode : Bytes.Decode.Decoder Save
byteDecode =
    let
        decodeVersion version =
            case version of
                1 ->
                    Bytes.Decode.unsignedInt8
                        |> byteDecodeListAndLength (Bytes.Decode.unsignedInt16 BE)
                        |> Bytes.Decode.map (List.map ((*) 32))
                        |> Bytes.Decode.map unpack
                        |> byteDecodeListAndLength (Bytes.Decode.unsignedInt16 BE)

                length ->
                    Bytes.Decode.unsignedInt16 BE
                        |> byteDecodeListAndLength (Bytes.Decode.unsignedInt16 BE)
                        |> Bytes.Decode.map unpack
                        |> byteDecodeListAndLength (Bytes.Decode.succeed length)
    in
    Bytes.Decode.unsignedInt16 BE
        |> Bytes.Decode.andThen decodeVersion


int16 : Int -> Bytes.Encode.Encoder
int16 =
    Bytes.Encode.unsignedInt16 BE


int8 : Int -> Bytes.Encode.Encoder
int8 =
    Bytes.Encode.unsignedInt8


byteEncodeList : List Bytes.Encode.Encoder -> Bytes.Encode.Encoder
byteEncodeList list =
    Bytes.Encode.sequence
        [ int16 (List.length list)
        , Bytes.Encode.sequence list
        ]


byteEncoder : Save -> Bytes.Encode.Encoder
byteEncoder save_ =
    let
        version =
            1
    in
    save_
        |> List.map pack
        |> (List.map <| List.map (int8 << (\x -> x // 32)))
        |> List.map byteEncodeList
        |> byteEncodeList
        |> (\data ->
                Bytes.Encode.sequence
                    [ int16 version
                    , data
                    ]
           )


load : Save -> Seeded NewModel
load s =
    let
        newModel =
            Model.init ()

        image =
            (Seeded.unwrap newModel).image

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
