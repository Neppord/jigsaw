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


zip : List a -> List b -> List ( a, b )
zip xs ys =
    List.map2 Tuple.pair xs ys


{-| Convenience function that flips the order of arguments.

    flip (>) 2 3 == True
    flip (-) 5 10 == 5

-}
flip : (a -> b -> c) -> b -> a -> c
flip f y x =
    f x y


{-| Remove element of given index from a List.

    pop 3 [0, 1, 2, 3, 4, 5] == (Just 3, [0, 1, 2, 4, 5])
    pop 0 [] == (Nothing, [])
    pop 0 [1] == (Just 1, [])

-}
pop : Int -> List a -> ( Maybe a, List a )
pop ind lst =
    if ind < 0 then
        ( Nothing, lst )

    else if ind == 0 then
        case lst of
            x :: xs ->
                ( Just x, xs )

            [] ->
                ( Nothing, [] )

    else
        case lst of
            x :: xs ->
                let
                    ( popped, tail ) =
                        pop (ind - 1) xs
                in
                ( popped, x :: tail )

            [] ->
                ( Nothing, [] )
