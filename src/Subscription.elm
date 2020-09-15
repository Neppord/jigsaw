module Subscription exposing (subscriptions)

import Browser.Events
import Decode
import Json.Decode
import Model exposing (Key(..), Msg(..), NewModel)
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


keyDown : Sub (Maybe Msg)
keyDown =
    Json.Decode.map2
        (\keyboard key ->
            case key of
                Just (Number x) ->
                    if keyboard.ctrl then
                        Just <| SendToVisibility x

                    else
                        Just <| ToggleVisibility x

                _ ->
                    Nothing
        )
        Decode.keyboard
        Decode.key
        |> Browser.Events.onKeyDown


subscriptions : NewModel -> Sub (Maybe Msg)
subscriptions newModel =
    case newModel.ui of
        UI.Moving _ _ ->
            Sub.batch [ trackMouseMovement, trackMouseUp ] |> Sub.map Just

        UI.Boxing _ _ ->
            Sub.batch [ trackMouseMovement, trackMouseUp ] |> Sub.map Just

        _ ->
            Sub.batch [ trackMouseDown |> Sub.map Just, keyDown ]
