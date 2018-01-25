module Model.Tetrimino
    exposing
        ( Tetrimino
        , tetrimino
        , moveTo
        , moveRight
        , moveDown
        , moveLeft
        , rotateRight
        , rotateLeft
        , toList
        )

import Model.Color exposing (Color(..))
import Model.Direction as Direction exposing (Direction(..))
import Model.Kind as Kind exposing (Kind(..))
import Model.Position as Position exposing (Position)


type alias Tetrimino =
    { kind : Kind
    , position : Position
    , direction : Direction
    }


tetrimino : Kind -> Tetrimino
tetrimino kind =
    { kind = kind
    , position = { x = 0, y = 0 }
    , direction = Up
    }


moveTo : Int -> Int -> Tetrimino -> Tetrimino
moveTo x y tetrimino =
    { tetrimino | position = { x = x, y = y } }


moveUp : Tetrimino -> Tetrimino
moveUp tetrimino =
    { tetrimino | position = Position.up 1 tetrimino.position }


moveRight : Tetrimino -> Tetrimino
moveRight tetrimino =
    { tetrimino | position = Position.right 1 tetrimino.position }


moveDown : Tetrimino -> Tetrimino
moveDown tetrimino =
    { tetrimino | position = Position.down 1 tetrimino.position }


moveLeft : Tetrimino -> Tetrimino
moveLeft tetrimino =
    { tetrimino | position = Position.left 1 tetrimino.position }


rotateRight : Tetrimino -> Tetrimino
rotateRight ({ kind, position, direction } as tetrimino) =
    { tetrimino
        | direction = Direction.rotateRight tetrimino.direction
    }
        |> case kind of
            I ->
                case direction of
                    Up ->
                        moveDown

                    Right ->
                        moveLeft

                    Down ->
                        moveUp

                    Left ->
                        moveRight

            _ ->
                identity


rotateLeft : Tetrimino -> Tetrimino
rotateLeft ({ kind, position, direction } as tetrimino) =
    { tetrimino
        | direction = Direction.rotateLeft tetrimino.direction
    }
        |> case kind of
            I ->
                case direction of
                    Up ->
                        moveLeft

                    Right ->
                        moveUp

                    Down ->
                        moveRight

                    Left ->
                        moveDown

            _ ->
                identity


toList : Tetrimino -> List ( ( Int, Int ), Color )
toList { kind, position, direction } =
    let
        color =
            Kind.toColor kind
    in
        kind
            |> initialPositionList
            |> (setRotate kind direction)
            |> List.map (\p -> ( ( p.x + position.x, p.y + position.y ), color ))


setRotate : Kind -> Direction -> List Position -> List Position
setRotate kind direction positionList =
    positionList
        |> List.map
            (case kind of
                O ->
                    identity

                _ ->
                    case direction of
                        Up ->
                            identity

                        Right ->
                            Position.rotateRight

                        Down ->
                            Position.reverse

                        Left ->
                            Position.rotateLeft
            )


initialPositionList : Kind -> List Position
initialPositionList kind =
    case kind of
        I ->
            i

        O ->
            o

        S ->
            s

        Z ->
            z

        J ->
            j

        L ->
            l

        T ->
            t


i : List Position
i =
    [ { x = 2, y = -1 }
    , { x = 1, y = -1 }
    , { x = 0, y = -1 }
    , { x = -1, y = -1 }
    ]


o : List Position
o =
    [ { x = 0, y = -1 }
    , { x = -1, y = -1 }
    , { x = 0, y = 0 }
    , { x = -1, y = 0 }
    ]


s : List Position
s =
    [ { x = 0, y = -1 }
    , { x = -1, y = -1 }
    , { x = 1, y = 0 }
    , { x = 0, y = 0 }
    ]


z : List Position
z =
    [ { x = 1, y = -1 }
    , { x = 0, y = -1 }
    , { x = 0, y = 0 }
    , { x = -1, y = 0 }
    ]


j : List Position
j =
    [ { x = 1, y = -1 }
    , { x = 1, y = 0 }
    , { x = 0, y = 0 }
    , { x = -1, y = 0 }
    ]


l : List Position
l =
    [ { x = -1, y = -1 }
    , { x = 1, y = 0 }
    , { x = 0, y = 0 }
    , { x = -1, y = 0 }
    ]


t : List Position
t =
    [ { x = 0, y = -1 }
    , { x = 1, y = 0 }
    , { x = 0, y = 0 }
    , { x = -1, y = 0 }
    ]
