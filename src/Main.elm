module Main exposing (main)

import Browser
import Browser.Navigation
import DB
import Drag
import Html
import Keyboard exposing (Keyboard)
import Model
    exposing
        ( Key(..)
        , Msg(..)
        , NewModel
        , generateModel
        )
import PieceGroup
import Point exposing (Point)
import Seeded exposing (Seeded(..))
import Set exposing (Set)
import Subscription exposing (subscriptions)
import UI
import Url
import View


main : Program () (Seeded NewModel) (Maybe Msg)
main =
    Browser.application
        { init = init
        , update = update
        , view =
            Seeded.unwrap
                >> view
        , subscriptions =
            Seeded.unwrap
                >> subscriptions
                >> Sub.map Just
        , onUrlChange = always Nothing
        , onUrlRequest = always Nothing
        }


view : NewModel -> Browser.Document (Maybe Msg)
view model =
    Browser.Document "Jigsaw" [ View.view model |> Html.map Just ]


init : () -> Url.Url -> Browser.Navigation.Key -> ( Seeded NewModel, Cmd (Maybe Msg) )
init flags _ _ =
    Model.init flags
        |> Tuple.mapSecond (Cmd.map Just)



-- UPDATE


update : Maybe Msg -> Seeded NewModel -> ( Seeded NewModel, Cmd (Maybe Msg) )
update maybeMsg seededModel =
    let
        nextModel msg =
            case msg of
                Scramble ->
                    seededModel
                        |> Seeded.map (.configuration >> .image)
                        |> Seeded.map generateModel
                        |> Seeded.step

                KeyDown keyboard key ->
                    seededModel
                        |> Seeded.map (updateKeyChange keyboard key)

                MouseDown coordinate keyboard ->
                    seededModel
                        |> Seeded.map (updateMouseDown coordinate keyboard)

                MouseUp ->
                    seededModel
                        |> Seeded.map updateMouseUp

                MouseMove newPos ->
                    seededModel
                        |> Seeded.map (updateMoveMouse newPos)

                ChangeImageUrl url ->
                    let
                        updateModel model =
                            { model | configuration = updateConfiguration model.configuration }

                        updateConfiguration configuration =
                            { configuration | image = updateImage configuration.image }

                        updateImage image =
                            { image | path = url }
                    in
                    seededModel
                        |> Seeded.map updateModel
    in
    ( Maybe.map nextModel maybeMsg
        |> Maybe.withDefault seededModel
    , Cmd.none
    )



{- this could be replaced from Set.Extra -}


sToggle : comparable -> Set comparable -> Set comparable
sToggle a set =
    if Set.member a set then
        Set.remove a set

    else
        Set.insert a set


updateKeyChange : Keyboard -> Maybe Key -> NewModel -> NewModel
updateKeyChange keyboard key model =
    case key of
        Just (Number x) ->
            if keyboard.ctrl then
                { model
                    | db =
                        model.db
                            |> DB.modifySelected
                                (\pg ->
                                    { pg
                                        | visibilityGroup = x
                                        , isSelected = True
                                    }
                                )
                }

            else
                { model
                    | visibleGroups = sToggle x model.visibleGroups
                }

        _ ->
            model


updateMouseDown : Point -> Keyboard -> NewModel -> NewModel
updateMouseDown coordinate keyboard model =
    let
        { db, visibleGroups } =
            model

        { image } =
            model.configuration

        mode =
            if keyboard.shift then
                UI.Add

            else if keyboard.ctrl then
                UI.Remove

            else
                UI.Replace
    in
    case DB.clickedPieceGroup visibleGroups image db coordinate of
        Nothing ->
            { model | ui = UI.Boxing mode (Drag.from coordinate) }

        Just pg ->
            let
                ui =
                    UI.Moving UI.Snap (Drag.from coordinate)
            in
            if pg.isSelected && mode /= UI.Remove then
                { model | ui = ui }

            else
                { model
                    | ui = ui
                    , db = model.db |> DB.select mode pg
                }


updateMouseUp : NewModel -> NewModel
updateMouseUp model =
    case model.ui of
        UI.Boxing mode drag ->
            { model
                | ui = UI.WaitingForInput
                , db = DB.boxSelect model.visibleGroups mode drag model.db
            }

        UI.Moving _ drag ->
            let
                move =
                    PieceGroup.move (Drag.distance drag)

                updatedDb =
                    DB.modifySelected move model.db
            in
            { model
                | ui = UI.WaitingForInput
                , db =
                    DB.snap model.configuration.snapDistance updatedDb
            }

        _ ->
            model


updateMoveMouse : Point -> NewModel -> NewModel
updateMoveMouse newPos model =
    case model.ui of
        UI.Moving mode drag ->
            { model
                | ui =
                    drag
                        |> Drag.to newPos
                        |> UI.Moving mode
            }

        UI.Boxing mode drag ->
            { model
                | ui =
                    drag
                        |> Drag.to newPos
                        |> UI.Boxing mode
            }

        _ ->
            model
