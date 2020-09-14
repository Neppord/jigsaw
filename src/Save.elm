module Save exposing (Save, decode, encode, load, save)

import DB
import Json.Decode
import Json.Encode
import Model exposing (NewModel)
import PieceGroup exposing (ID)
import Point exposing (Point)
import Seeded exposing (Seeded)


type alias Save =
    { pieces : List ( Point, ID )
    , imageUrl : String
    }


decodePoint : Json.Decode.Decoder Point
decodePoint =
    Json.Decode.map2 Point
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)


decodeID : Json.Decode.Decoder ID
decodeID =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)


decodePair : Json.Decode.Decoder ( Point, ID )
decodePair =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "point" decodePoint)
        (Json.Decode.field "id" decodeID)


decode : Json.Decode.Decoder Save
decode =
    Json.Decode.map2 Save
        (Json.Decode.field "pieces" (Json.Decode.list decodePair))
        (Json.Decode.field "imageUrl" Json.Decode.string)


encode : Save -> Json.Encode.Value
encode s =
    Json.Encode.object <|
        [ ( "pieces"
          , Json.Encode.list
                encodePair
                s.pieces
          )
        , ( "imageUrl", Json.Encode.string s.imageUrl )
        ]


encodePair : ( Point, ID ) -> Json.Encode.Value
encodePair ( a, b ) =
    Json.Encode.object <|
        [ ( "point", encodePoint a )
        , ( "id", encodeID b )
        ]


encodeID : PieceGroup.ID -> Json.Encode.Value
encodeID ( x, y ) =
    Json.Encode.object <|
        [ ( "x", Json.Encode.int x )
        , ( "y", Json.Encode.int y )
        ]


encodePoint : Point -> Json.Encode.Value
encodePoint point =
    Json.Encode.object <|
        [ ( "x", Json.Encode.int point.x )
        , ( "y", Json.Encode.int point.y )
        ]


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
    in
    newModel
        |> Seeded.map
            (\m ->
                { m
                    | db =
                        s.pieces
                            |> List.map createPG
                            |> DB.makeDb
                    , configuration =
                        { configuration
                            | image = { image | path = s.imageUrl }
                        }
                }
            )


save : Seeded NewModel -> Save
save model =
    { pieces =
        Seeded.unwrap model
            |> .db
            |> DB.getPieces
    , imageUrl =
        Seeded.unwrap model
            |> .configuration
            |> .image
            |> .path
    }
