module Save exposing (Save, load, save)

import DB
import Model exposing (NewModel)
import PieceGroup exposing (ID)
import Point exposing (Point)
import Seeded exposing (Seeded)


type alias Save =
    { pieces : List ( Point, ID )
    , imageUrl : String
    }


load : Save -> Seeded NewModel
load s =
    let
        newModel =
            Model.init () |> Tuple.first

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
