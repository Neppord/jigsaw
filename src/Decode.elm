module Decode exposing (keyboard, pageCoordinates)
import Point exposing (Point)
import Model exposing (Keyboard)
import Json.Decode exposing (Decoder, map2, field, int, bool)

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