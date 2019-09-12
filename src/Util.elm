module Util exposing (..)

{-| Take the first element from the list that satisfies the condition.
If no such element exists, return Nothing.

    takeFirst (\x -> x > 0) [-1, 3, 4] == Just 3
    takeFirst (\x -> x > 0) [-2, -1, 0] == Nothing
-}
takeFirst : (a -> Bool) -> List a -> Maybe a
takeFirst condition list =
  case list of
    [] ->
      Nothing
    x :: xs ->
      if condition x then
        Just x
      else
        takeFirst condition xs

zip : List a -> List b -> List (a, b)
zip xs ys =
  List.map2 Tuple.pair xs ys