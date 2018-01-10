module Model.Direction exposing (Direction(..), rotateRight, rotateLeft, reverse)


type Direction
    = Top
    | Right
    | Bottom
    | Left


rotateRight : Direction -> Direction
rotateRight direction =
    case direction of
        Top ->
            Right

        Right ->
            Bottom

        Bottom ->
            Left

        Left ->
            Top


rotateLeft : Direction -> Direction
rotateLeft direction =
    case direction of
        Top ->
            Left

        Right ->
            Top

        Bottom ->
            Right

        Left ->
            Bottom


reverse : Direction -> Direction
reverse direction =
    rotateRight <| rotateRight <| direction
