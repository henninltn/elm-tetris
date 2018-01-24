module Model.Tetrimino
    exposing
        ( Tetrimino
        , Kind(..)
        , intToKind
        , moveRight
        , moveDown
        , moveLeft
        , rotateRight
        , rotateLeft
        , toPositionColorPairList
        )

import Model.Color exposing (Color(..))
import Model.Direction as Direction exposing (Direction(..))
import Model.Position as Position exposing (Position)


type alias Tetrimino =
    { kind : Kind
    , position : Position
    , direction : Direction
    }


type Kind
    = I
    | O
    | S
    | Z
    | J
    | L
    | T


intToKind : Int -> Maybe Kind
intToKind i =
    if i == 0 then
        Just I
    else if i == 1 then
        Just O
    else if i == 2 then
        Just S
    else if i == 3 then
        Just Z
    else if i == 4 then
        Just J
    else if i == 5 then
        Just L
    else if i == 6 then
        Just T
    else
        Nothing


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


toPositionColorPairList : Tetrimino -> List ( Position, Color )
toPositionColorPairList { kind, position, direction } =
    List.map (\( p, c ) -> ( Position.add p position, c )) <|
        List.map (setRotate kind direction) <|
            initPositionList kind


setRotate : Kind -> Direction -> ( Position, Color ) -> ( Position, Color )
setRotate kind direction posColorList =
    case kind of
        O ->
            posColorList

        _ ->
            case direction of
                Up ->
                    posColorList

                Right ->
                    (\( l, c ) -> ( Position.rotateRight l, c )) <|
                        posColorList

                Down ->
                    (\( l, c ) -> ( Position.reverse l, c )) <|
                        posColorList

                Left ->
                    (\( l, c ) -> ( Position.rotateLeft l, c )) <|
                        posColorList


initPositionList : Kind -> List ( Position, Color )
initPositionList kind =
    case kind of
        I ->
            List.map (\position -> ( position, Lightblue )) <|
                i

        O ->
            List.map (\position -> ( position, Yellow )) <|
                o

        S ->
            List.map (\position -> ( position, Yellowgreen )) <|
                s

        Z ->
            List.map (\position -> ( position, Red )) <|
                z

        J ->
            List.map (\position -> ( position, Blue )) <|
                j

        L ->
            List.map (\position -> ( position, Orange )) <|
                l

        T ->
            List.map (\position -> ( position, Purple )) <|
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
