module UI exposing ( SelectionMode(..), UI(..), SanpMode(..))
import Drag exposing (Drag)


type UI
    = Moving SanpMode Drag
    | Boxing SelectionMode Drag
    | Selecting SelectionMode
    | WaitingForInput

type SanpMode = Snap | NoSnap

type SelectionMode
    = Replace
    | Add
    | Remove
