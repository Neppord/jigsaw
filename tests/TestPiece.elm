module TestPiece exposing (merge)

import Expect exposing (all, equal)
import Piece
import Test exposing (Test, test)

merge : Test
merge =
    test "merge" <|
        all
            [ \_ -> Piece.merge Piece.empty Piece.empty |> equal Piece.empty
            , \_ -> Piece.merge Piece.empty Piece.first |> equal Piece.first
            , \_ -> Piece.merge Piece.first Piece.empty |> equal Piece.first
            ]
