module Piece exposing (ID, Piece, areConnected, createPiece, empty, first, merge)

import Set exposing (Set)


intMax : Int
intMax =
    2147483647


type alias ID =
    ( Int, Int )


type alias Piece =
    { members : Set ID
    , neighbours : Set ID
    , start : ( Int, Int )
    , end : ( Int, Int )
    }


empty : Piece
empty =
    { members = Set.empty
    , neighbours = Set.empty
    , start = ( intMax, intMax )
    , end = ( 0, 0 )
    }


first : Piece
first =
    createPiece ( 0, 0 )

neighbours : ID -> Set ID
neighbours (x, y) = 
    Set.fromList
        [ ( x - 1, y )
        , ( x, y - 1 )
        , ( x + 1, y )
        , ( x, y + 1 )
        ]

createPiece : ID -> Piece
createPiece id =
    { members = Set.singleton id
    , neighbours = neighbours id
    , start = id
    , end = id
    }


merge : Piece -> Piece -> Piece
merge a b =
    let
        members =
            Set.union a.members b.members

        neighbours_ =
            Set.diff
                (Set.union a.neighbours b.neighbours)
                members
    in
    { members = members
    , neighbours = neighbours_
    , start = mergeStart a.start b.start
    , end = mergeEnd a.end b.end
    }


mergeStart : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
mergeStart ( x1, y1 ) ( x2, y2 ) =
    ( min x1 x2, min y1 y2 )


mergeEnd : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
mergeEnd ( x1, y1 ) ( x2, y2 ) =
    ( max x1 x2, max y1 y2 )

areConnected : Piece -> Piece -> Bool
areConnected this other =
    if Set.size this.neighbours > Set.size other.neighbours then
        areConnected other this
    else
        other.neighbours
            |> Set.intersect this.members
            |> Set.size
            |> (<) 0 
