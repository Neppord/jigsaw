module TestKDDict exposing (..)

import Expect exposing (equal)
import Fuzz exposing (Fuzzer)
import KDDict exposing (KDDict)
import Test exposing (describe, fuzz, test)


listAndElement : Fuzzer a -> Fuzzer ( List a, a )
listAndElement f =
    Fuzz.map3
        (\head list index ->
            ( head :: list
            , list
                |> List.drop (index |> modBy (List.length list + 1))
                |> List.head
                |> Maybe.withDefault head
            )
        )
        f
        (Fuzz.list f)
        Fuzz.int


tests =
    describe "KDDict"
        [ test "single item with single axis" <|
            \_ ->
                KDDict.fromList [ ( KDDict.key 1, 1 ) ]
                    |> KDDict.get (KDDict.key 1)
                    |> equal (Just 1)
        , fuzz (listAndElement Fuzz.int) "multiple items with single axis" <|
            \( list, el ) ->
                KDDict.fromListBy KDDict.key list
                    |> KDDict.get (KDDict.key el)
                    |> equal (Just el)
        , test "finding all" <|
            \_ ->
                KDDict.fromListBy KDDict.key [ 1, 1, 2, 1 ]
                    |> KDDict.findAll (KDDict.key <| Just 1)
                    |> equal [ 1, 1, 1 ]
        , test "remove" <|
            \_ ->
                KDDict.fromListBy KDDict.key [ 1, 2 ]
                    |> KDDict.remove (KDDict.key 1)
                    |> KDDict.toList
                    |> equal [ ( KDDict.key 2, 2 ) ]
        , test "insert" <|
            \_ ->
                KDDict.empty
                    |> KDDict.insert (KDDict.key 2) 1
                    |> KDDict.toList
                    |> equal [ ( KDDict.key 2, 1 ) ]
        , test "merge" <|
            \_ ->
                let
                    a =
                        KDDict.empty
                            |> KDDict.insert (KDDict.key 1) 1

                    b =
                        KDDict.empty
                            |> KDDict.insert (KDDict.key 2) 2
                in
                KDDict.merge a b
                    |> KDDict.toList
                    |> equal [ ( KDDict.key 1, 1 ), ( KDDict.key 2, 2 ) ]
        , test "range" <|
            \_ ->
                KDDict.fromListBy KDDict.key [ 1, 2, 3, 4, 5 ]
                    |> KDDict.findAllInRange (KDDict.key <| Just ( 2, 3 ))
                    |> equal [ 2, 3 ]
        ]
