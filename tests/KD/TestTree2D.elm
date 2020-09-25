module KD.TestTree2D exposing (suite)

import Expect exposing (equal)
import KD.Tree2D exposing (Tree2D(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tree2D" [ test "true" <| \_ -> equal Empty Empty ]
