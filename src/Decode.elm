module Decode exposing (key, keyboard, pageCoordinates)

import Json.Decode exposing (Decoder, bool, field, int, map, map2, string)
import Model exposing (Key(..))
import Point exposing (Point)
import Keyboard exposing (Keyboard)


pageCoordinates : Decoder Point
pageCoordinates =
    map2
        Point
        (field "pageX" int)
        (field "pageY" int)


keyboard : Decoder Keyboard
keyboard =
    map2
        Keyboard
        (field "shiftKey" bool)
        (field "ctrlKey" bool)


identifierToKey : String -> Maybe Model.Key
identifierToKey identifier =
    case identifier of
        "0" ->
            Just <| Number 0

        "1" ->
            Just <| Number 1

        "2" ->
            Just <| Number 2

        "3" ->
            Just <| Number 3

        "4" ->
            Just <| Number 4

        "5" ->
            Just <| Number 5

        "6" ->
            Just <| Number 6

        "7" ->
            Just <| Number 7

        "8" ->
            Just <| Number 8

        "9" ->
            Just <| Number 9

        _ ->
            Nothing


key : Decoder (Maybe Key)
key =
    field "key" string
        |> map identifierToKey
