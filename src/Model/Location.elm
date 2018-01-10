module Model.Location exposing (add, rotateRight, rotateLeft, reverse)

import Matrix exposing (Location, loc, row, col)


add : Location -> Location -> Location
add ( r1, c1 ) ( r2, c2 ) =
    loc (r1 + r2) (c1 + c2)


rotateRight : Location -> Location
rotateRight l =
    ( -(col l), row l )


rotateLeft : Location -> Location
rotateLeft l =
    ( col l, -(row l) )


reverse : Location -> Location
reverse l =
    ( -(row l), -(col l) )
