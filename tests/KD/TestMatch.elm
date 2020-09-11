module KD.TestMatch exposing (..)

import Expect exposing (all, equal)
import KD.Match exposing (Match(..), compareWithMatch)
import Test exposing (Test, describe, test)


x =
    compare


suite : Test
suite =
    describe "CompareWithMatch"
        [ test "everything is equal to anything" <|
            \_ ->
                compareWithMatch 1 Anything |> equal EQ
        , test "Equal compares as normal" <|
            all
                [ \_ -> compareWithMatch 2 (EqualTo 3) |> equal LT
                , \_ -> compareWithMatch 3 (EqualTo 3) |> equal EQ
                , \_ -> compareWithMatch 4 (EqualTo 3) |> equal GT
                ]
        , test "smaller than is inclusive" <|
            \_ -> compareWithMatch 3 (SmallerThan 3) |> equal EQ
        , test "smaller than counts as equal if inside range" <|
            \_ -> compareWithMatch 2 (SmallerThan 3) |> equal EQ
        , test "smaller than counts as larger if outside range" <|
            \_ -> compareWithMatch 4 (SmallerThan 3) |> equal GT
        , test "larger than is inclusive" <|
            \_ -> compareWithMatch 3 (LargerThan 3) |> equal EQ
        , test "larger than counts as equal if inside range" <|
            \_ -> compareWithMatch 4 (LargerThan 3) |> equal EQ
        , test "larger than counts as smaller if outside range" <|
            \_ -> compareWithMatch 2 (LargerThan 3) |> equal LT
        , test "Withing range is inclusive" <|
            all
                [ \_ -> compareWithMatch 2 (WithinRange 2 4) |> equal EQ
                , \_ -> compareWithMatch 4 (WithinRange 2 4) |> equal EQ
                ]
        , test "Withing range is equal if in range" <|
            \_ -> compareWithMatch 3 (WithinRange 2 4) |> equal EQ
        , test "Withing range is less than when less then both range stops" <|
            all
                [ \_ -> compareWithMatch 1 (WithinRange 4 2) |> equal LT
                , \_ -> compareWithMatch 1 (WithinRange 2 4) |> equal LT
                ]
        , test "Withing range is greater than when greater then both range stops" <|
            all
                [ \_ -> compareWithMatch 5 (WithinRange 4 2) |> equal GT
                , \_ -> compareWithMatch 5 (WithinRange 2 4) |> equal GT
                ]
        ]
