module Model.Position
    exposing
        ( Position
        , reverse
        , add
        , rotateRight
        , rotateLeft
        , up
        , right
        , down
        , left
        )


type alias Position =
    { x : Int
    , y : Int
    }


reverse : Position -> Position
reverse { x, y } =
    { x = -x, y = -y }


add : Position -> Position -> Position
add p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }


rotateRight : Position -> Position
rotateRight { x, y } =
    { x = y, y = -x }


rotateLeft : Position -> Position
rotateLeft { x, y } =
    { x = -y, y = x }


up : Int -> Position -> Position
up d { x, y } =
    { x = x, y = y - d }


right : Int -> Position -> Position
right d { x, y } =
    { x = x + d, y = y }


down : Int -> Position -> Position
down d { x, y } =
    { x = x, y = y + d }


left : Int -> Position -> Position
left d { x, y } =
    { x = x - d, y = y }
