module Model.Direction
    exposing
        ( Direction(..)
        , rotateRight
        , rotateLeft
        , reverse
        )


type Direction
    = Up
    | Right
    | Down
    | Left


rotateRight : Direction -> Direction
rotateRight direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


rotateLeft : Direction -> Direction
rotateLeft direction =
    case direction of
        Up ->
            Left

        Right ->
            Up

        Down ->
            Right

        Left ->
            Down


reverse : Direction -> Direction
reverse direction =
    rotateRight <| rotateRight <| direction
