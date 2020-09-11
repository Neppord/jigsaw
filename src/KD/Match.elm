module KD.Match exposing (Match(..), compareWithMatch, match)

{-| Match are used to create search criteria.

  - match anything
  - match smaller then including value
  - match exact values
  - match larger then including value
  - match range of values including ends

-}


type Match k
    = Anything
    | EqualTo k
    | SmallerThan k
    | LargerThan k
    | WithinRange k k


match : Match comparable -> comparable -> Bool
match m k =
    compareWithMatch k m == EQ


compareWithMatch : comparable -> Match comparable -> Order
compareWithMatch k m =
    case m of
        Anything ->
            EQ

        EqualTo k1 ->
            compare k k1

        SmallerThan k1 ->
            case compare k k1 of
                GT ->
                    GT

                _ ->
                    EQ

        LargerThan k1 ->
            case compare k k1 of
                LT ->
                    LT

                _ ->
                    EQ

        WithinRange k1 k2 ->
            let
                res =
                    compare k k1
            in
            if res == compare k k2 then
                res

            else
                EQ
