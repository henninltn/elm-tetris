module Model.Location
    exposing
        ( reverse
        , add
        , rotateRight
        , rotateLeft
        )

import Matrix exposing (Location, loc, row, col)


reverse : Location -> Location
reverse l =
    ( -(row l), -(col l) )


add : Location -> Location -> Location
add ( r1, c1 ) ( r2, c2 ) =
    loc (r1 + r2) (c1 + c2)


rotateRight : Location -> Location
rotateRight l =
    ( -(col l), row l )


rotateLeft : Location -> Location
rotateLeft l =
    ( col l, -(row l) )
