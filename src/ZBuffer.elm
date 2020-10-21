module ZBuffer exposing (ZBuffer, empty, get, insert)

import Dict exposing (Dict)


type ZBuffer c
    = ZBuffer
        { resolution : Int
        , buffer : Dict ( Int, Int ) c
        }


empty : Int -> ZBuffer c
empty resolution =
    ZBuffer
        { resolution = resolution
        , buffer = Dict.empty
        }


insert : { a | x : Int, width : Int, y : Int, height : Int } -> b -> ZBuffer b -> ZBuffer b
insert dimensions item (ZBuffer { resolution, buffer }) =
    let
        xs =
            List.range dimensions.x (dimensions.x + dimensions.width)

        ys =
            List.range dimensions.y (dimensions.y + dimensions.height)

        points =
            List.concatMap (\x_ -> List.map (Tuple.pair x_) ys) xs
    in
    ZBuffer
        { resolution = resolution
        , buffer =
            List.foldl (\point dict -> Dict.insert point item dict) buffer points
        }


get : ( Int, Int ) -> ZBuffer c -> Maybe c
get ( x, y ) (ZBuffer { resolution, buffer }) =
    Dict.get ( x * resolution, y * resolution ) buffer
