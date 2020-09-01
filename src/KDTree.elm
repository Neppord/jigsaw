module KDTree exposing
    ( Axis
    , KDTree, remove, toList
    , addAxis
    , addKey
    , fromList
    , fromList_
    , keyBy
    , keyToAxis
    , makeConfig
    , singelton
    , singelton_
    , smallest
    )

import Html exposing (a)
import Maybe exposing (Maybe)
import Svg.Attributes exposing (x)


type KDTree a
    = KDTree (Axis a) (InnerTree a)


type InnerTree a
    = Node (InnerTree a) a (InnerTree a)
    | Empty


type Axis a
    = Axis (a -> a -> Bool) (List (a -> a -> Bool))


keyBy : (a -> comparable) -> (a -> a -> Bool)
keyBy f a b =
    f a < f b


keyToAxis : (a -> comparable) -> Axis a
keyToAxis key =
    makeConfig (keyBy key) []


makeConfig : (a -> a -> Bool) -> List (a -> a -> Bool) -> Axis a
makeConfig =
    Axis


addAxis : (a -> a -> Bool) -> Axis a -> Axis a
addAxis newHead (Axis head tail) =
    Axis newHead (head :: tail)


addKey : (a -> comparable) -> Axis a -> Axis a
addKey =
    keyBy >> addAxis


comparableAxis : Axis comparable
comparableAxis =
    makeConfig (<) []


singelton_ : comparable -> KDTree comparable
singelton_ =
    singelton comparableAxis


singelton : Axis a -> a -> KDTree a
singelton config value =
    KDTree config (Node Empty value Empty)


fromList_ : List comparable -> KDTree comparable
fromList_ =
    fromList comparableAxis


fromList : Axis a -> List a -> KDTree a
fromList config list =
    innerTreeFromList config config list
        |> KDTree config


walkAxis : Axis a -> List (a -> a -> Bool) -> Axis a
walkAxis reset rest =
    case rest of
        [] ->
            reset

        nextHead :: nextRest ->
            Axis nextHead nextRest


innerTreeFromList : Axis a -> Axis a -> List a -> InnerTree a
innerTreeFromList reset (Axis head rest) list =
    case list of
        [] ->
            Empty

        x :: xs ->
            let
                ( larger, smaller ) =
                    List.partition (head x) xs

                nextAxis =
                    walkAxis reset rest
            in
            Node
                (innerTreeFromList reset nextAxis smaller)
                x
                (innerTreeFromList reset nextAxis larger)


remove : a -> KDTree a -> KDTree a
remove item (KDTree axis tree) =
    removeFromInnerTree axis axis item tree
        |> KDTree axis


removeFromInnerTree : Axis a -> Axis a -> a -> InnerTree a -> InnerTree a
removeFromInnerTree reset (Axis smallerThen rest) item tree =
    case tree of
        Empty ->
            Empty

        Node smaller a larger ->
            if a == item then
                innerTreeToList smaller
                    ++ innerTreeToList larger
                    |> innerTreeFromList reset (Axis smallerThen rest)

            else
                let
                    nextAxis =
                        walkAxis reset rest
                in
                if smallerThen item a then
                    Node
                        (removeFromInnerTree reset nextAxis item smaller)
                        a
                        larger

                else
                    Node
                        smaller
                        a
                        (removeFromInnerTree reset nextAxis item larger)

toList : KDTree a -> List a
toList (KDTree _ tree) = innerTreeToList tree

innerTreeToList : InnerTree a -> List a
innerTreeToList tree =
    case tree of
        Empty ->
            []

        Node smaller a larger ->
            innerTreeToList smaller ++  a :: innerTreeToList larger


smallest : KDTree a -> Maybe a
smallest (KDTree axis tree) =
    case tree of
        Empty ->
            Nothing

        Node smaller value _ ->
            case smallest (KDTree axis smaller) of
                Nothing ->
                    Just value

                Just a ->
                    Just a
