module Subscription exposing (subscriptions)

import Browser.Events
import Decode
import Json.Decode
import Model exposing (Msg(..), NewModel)
import UI


trackMouseMovement : Sub Msg
trackMouseMovement =
    Decode.pageCoordinates
        |> Json.Decode.map MouseMove
        |> Browser.Events.onMouseMove


trackMouseDown : Sub Msg
trackMouseDown =
    Json.Decode.map2 MouseDown Decode.pageCoordinates Decode.keyboard
        |> Browser.Events.onMouseDown


trackMouseUp : Sub Msg
trackMouseUp =
    Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)


keyDown : Sub Msg
keyDown =
    Json.Decode.map2 KeyDown Decode.keyboard Decode.key
        |> Browser.Events.onKeyDown


subscriptions : NewModel -> Sub Msg
subscriptions newModel =
    case newModel.ui of
        UI.Moving _ _ ->
            Sub.batch [ trackMouseMovement, trackMouseUp ]

        UI.Boxing _ _ ->
            Sub.batch [ trackMouseMovement, trackMouseUp ]

        _ ->
            Sub.batch [ trackMouseDown, keyDown ]
