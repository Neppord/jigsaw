module TestZBuffer exposing (suite)

import Expect exposing (all, equal)
import Test exposing (Test, describe, test)
import ZBuffer exposing (empty, get, insert)


suite : Test
suite =
    describe "zBuffer"
        [ test "top corner of one piece" <|
            \_ ->
                empty 1
                    |> insert
                        { x = 0, y = 0, width = 10, height = 10 }
                        1
                    |> get ( 0, 0 )
                    |> equal (Just 1)
        , test "center of one piece" <|
            \_ ->
                empty 1
                    |> insert
                        { x = 0, y = 0, width = 10, height = 10 }
                        1
                    |> get ( 5, 5 )
                    |> equal (Just 1)
        , test "outside of one piece" <|
            \_ ->
                empty 1
                    |> insert
                        { x = 0, y = 0, width = 10, height = 10 }
                        1
                    |> get ( 11, 11 )
                    |> equal Nothing
        ]
