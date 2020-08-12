module TestEdge exposing (indexOfSuite, cordsSuite)

import Edge exposing (Orientation(..))
import Expect exposing (all, equal)
import Test exposing (Test, describe, test)


indexOfSuite : Test
indexOfSuite =
     describe "indexOf"
        [ describe "There are no edges when there is only one piece"
            ([ North, East, South, West ]
                |> List.map
                    (\o ->
                        test
                            (Debug.toString o)
                            (\_ -> Edge.indexOf o 1 1 0 |> equal -1)
                    )
            )
        , test "Pieces that share a edge has the same index " <|
            all
                [ \_ -> Edge.indexOf North 3 3 4 |> equal (Edge.indexOf South 3 3 1)
                , \_ -> Edge.indexOf East 3 3 4 |> equal (Edge.indexOf West 3 3 5)
                , \_ -> Edge.indexOf South 3 3 4 |> equal (Edge.indexOf North 3 3 7)
                , \_ -> Edge.indexOf West 3 3 4 |> equal (Edge.indexOf East 3 3 3)
                , \_ -> Edge.indexOf North 3 3 4 |> equal 7
                , \_ -> Edge.indexOf East 3 3 4 |> equal 3
                , \_ -> Edge.indexOf South 3 3 4 |> equal 10
                , \_ -> Edge.indexOf West 3 3 4 |> equal 2
                ]
        ]
    
cordsSuite : Test
cordsSuite = describe "cords" 
    [ test "2 0" <| \ _ -> Edge.cords 2 0 |> equal (0, 0)
    , test "2 1" <| \ _ -> Edge.cords 2 1 |> equal (1, 0)
    , test "2 2" <| \ _ -> Edge.cords 2 2 |> equal (0, 1)
    , test "2 3" <| \ _ -> Edge.cords 2 3 |> equal (1, 1)
    ]