module Decode exposing (key, keyboard, pageCoordinates)

import Json.Decode exposing (Decoder, bool, field, int, map, map2, string)
import Model exposing (Key(..), Keyboard)
import Point exposing (Point)


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


identifierToKey : String -> Model.Key
identifierToKey identifier =
    case identifier of
        "0" ->
            Number 0

        "1" ->
            Number 1

        "2" ->
            Number 2

        "3" ->
            Number 3

        "4" ->
            Number 4

        "5" ->
            Number 5

        "6" ->
            Number 6

        "7" ->
            Number 7

        "8" ->
            Number 8

        "9" ->
            Number 9

        "Control" ->
            Control

        "Shift" ->
            Shift

        _ ->
            Other


key : Decoder Key
key =
    field "key" string
        |> map identifierToKey
