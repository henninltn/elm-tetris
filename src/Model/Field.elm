module Model.Field
    exposing
        ( Msg(..)
        , Model
        , init
        , Field
        , setCurrent
        , fixTetrimino
        , toList
        , moveRight
        , moveDown
        , moveLeft
        , rotateRight
        , rotateLeft
        , initTetrimino
        )

import Array.Hamt as Array
import Keyboard.Extra exposing (Key)
import Matrix exposing (Matrix)
import Model.Color exposing (Color(..))
import Model.Kind exposing (Kind(..))
import Model.Tetrimino as Tetrimino exposing (Tetrimino)
import Time exposing (Time)
import Queue exposing (Queue)


type Msg
    = Init
    | FillQueue ( Kind, List Kind )
    | NewTetrimino Kind
    | FreeFall Time
    | KeyDowns Key


type alias Model =
    { field : Field
    , queue : Queue Tetrimino
    , isGameOver : Bool
    }


init : Model
init =
    { field =
        { colors = Matrix.repeat 10 21 Invisible
        , current = Nothing
        }
    , queue = Queue.empty
    , isGameOver = False
    }


type alias Field =
    { colors : Matrix Color
    , current : Maybe Tetrimino
    }


setCurrent : Tetrimino -> Field -> Field
setCurrent tetrimino field =
    { field | current = Just tetrimino }


isValid : Field -> Bool
isValid field =
    case field.current of
        Just tetrimino ->
            tetrimino
                |> Tetrimino.toList
                |> List.all
                    (\( ( x, y ), c ) ->
                        case (Matrix.get x y field.colors) of
                            Just color ->
                                case color of
                                    Invisible ->
                                        True

                                    _ ->
                                        False

                            Nothing ->
                                False
                    )

        Nothing ->
            True


fixTetrimino : Field -> Maybe Field
fixTetrimino field =
    if isValid field then
        case field.current of
            Just tetrimino ->
                Just
                    { field
                        | colors =
                            tetrimino
                                |> Tetrimino.toList
                                |> List.foldr
                                    (\( ( x, y ), c ) colors ->
                                        Matrix.set x y c colors
                                    )
                                    field.colors
                        , current = Nothing
                    }

            Nothing ->
                Nothing
    else
        Nothing


toList : Field -> List ( ( Int, Int ), Color )
toList field =
    (field
        |> fixTetrimino
        |> Maybe.withDefault field
    ).colors
        |> Matrix.toIndexedArray
        |> Array.toList


moveRight : Field -> Maybe Field
moveRight field =
    case field.current of
        Just tetrimino ->
            let
                movedField =
                    { field
                        | current = Just (Tetrimino.moveRight tetrimino)
                    }
            in
                if isValid movedField then
                    Just movedField
                else
                    Nothing

        Nothing ->
            Nothing


moveDown : Field -> Maybe Field
moveDown field =
    case field.current of
        Just tetrimino ->
            let
                movedField =
                    { field
                        | current = Just (Tetrimino.moveDown tetrimino)
                    }
            in
                if isValid movedField then
                    Just movedField
                else
                    Nothing

        Nothing ->
            Nothing


moveLeft : Field -> Maybe Field
moveLeft field =
    case field.current of
        Just tetrimino ->
            let
                movedField =
                    { field
                        | current = Just (Tetrimino.moveLeft tetrimino)
                    }
            in
                if isValid movedField then
                    Just movedField
                else
                    Nothing

        Nothing ->
            Nothing


rotateRight : Field -> Maybe Field
rotateRight field =
    case field.current of
        Just tetrimino ->
            let
                rotatedField =
                    { field
                        | current = Just (Tetrimino.rotateRight tetrimino)
                    }
            in
                if isValid rotatedField then
                    Just rotatedField
                else
                    Nothing

        Nothing ->
            Nothing


rotateLeft : Field -> Maybe Field
rotateLeft field =
    case field.current of
        Just tetrimino ->
            let
                rotatedField =
                    { field
                        | current = Just (Tetrimino.rotateLeft tetrimino)
                    }
            in
                if isValid rotatedField then
                    Just rotatedField
                else
                    Nothing

        Nothing ->
            Nothing


initTetrimino : Kind -> Tetrimino
initTetrimino kind =
    kind
        |> Tetrimino.tetrimino
        |> Tetrimino.moveTo 4 1
