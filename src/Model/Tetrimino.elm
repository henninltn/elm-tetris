module Model.Tetrimino
    exposing
        ( Tetrimino
        , Kind(..)
        , intToKind
        , toLocationColorPairList
        , moveRight
        , moveBottom
        , moveLeft
        )

import Matrix exposing (Location, loc)
import Model.Color exposing (Color(..))
import Model.Direction exposing (Direction(..))
import Model.Location as Location


type Kind
    = I
    | O
    | S
    | Z
    | J
    | L
    | T


type alias Tetrimino =
    { kind : Kind
    , location : Location
    , direction : Direction
    }


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


moveRight : Tetrimino -> Tetrimino
moveRight { kind, location, direction } =
    { kind = kind
    , location = Location.add location (loc 0 1)
    , direction = direction
    }


moveBottom : Tetrimino -> Tetrimino
moveBottom { kind, location, direction } =
    { kind = kind
    , location = Location.add location (loc 1 0)
    , direction = direction
    }


moveLeft : Tetrimino -> Tetrimino
moveLeft { kind, location, direction } =
    { kind = kind
    , location = Location.add location (loc 0 -1)
    , direction = direction
    }


toLocationColorPairList : Tetrimino -> List ( Location, Color )
toLocationColorPairList { kind, location, direction } =
    List.map (\( l, c ) -> ( Location.add l location, c )) <|
        List.map (setRotate kind direction) <|
            initLocationList kind


setRotate : Kind -> Direction -> ( Location, Color ) -> ( Location, Color )
setRotate kind direction locColorList =
    case kind of
        O ->
            locColorList

        _ ->
            case direction of
                Top ->
                    locColorList

                Right ->
                    (\( l, c ) -> ( Location.rotateRight l, c )) <|
                        locColorList

                Bottom ->
                    (\( l, c ) -> ( Location.reverse l, c )) <|
                        locColorList

                Left ->
                    (\( l, c ) -> ( Location.rotateLeft l, c )) <|
                        locColorList


initLocationList : Kind -> List ( Location, Color )
initLocationList kind =
    case kind of
        I ->
            List.map (\location -> ( location, Lightblue )) <|
                i

        O ->
            List.map (\location -> ( location, Yellow )) <|
                o

        S ->
            List.map (\location -> ( location, Yellowgreen )) <|
                s

        Z ->
            List.map (\location -> ( location, Red )) <|
                z

        J ->
            List.map (\location -> ( location, Blue )) <|
                j

        L ->
            List.map (\location -> ( location, Orange )) <|
                l

        T ->
            List.map (\location -> ( location, Purple )) <|
                t


i : List Location
i =
    [ loc 0 2
    , loc 0 1
    , loc 0 0
    , loc 0 -1
    ]


o : List Location
o =
    [ loc 0 0
    , loc 0 -1
    , loc 1 0
    , loc 1 -1
    ]


s : List Location
s =
    [ loc 0 0
    , loc 0 -1
    , loc 1 1
    , loc 1 0
    ]


z : List Location
z =
    [ loc 0 1
    , loc 0 0
    , loc 1 0
    , loc 1 -1
    ]


j : List Location
j =
    [ loc 0 1
    , loc 1 1
    , loc 1 0
    , loc 1 -1
    ]


l : List Location
l =
    [ loc 0 -1
    , loc 1 1
    , loc 1 0
    , loc 1 -1
    ]


t : List Location
t =
    [ loc 0 0
    , loc 1 1
    , loc 1 0
    , loc 1 -1
    ]
