module Model.Field
    exposing
        ( Msg(..)
        , Model
        , Field
        , init
        , isValidPosition
        , fixTetrimino
        )

import Keyboard exposing (KeyCode)
import Matrix exposing (Matrix)
import Model.Color exposing (Color(..))
import Model.Kind exposing (Kind(..))
import Model.Tetrimino as Tetrimino exposing (Tetrimino)
import Time exposing (Time)


type Msg
    = FreeFall Time
    | Init (List Kind)
    | AddTetrimino Kind
    | KeyPresses KeyCode


type alias Model =
    { field : Field
    , tetrimino : Maybe Tetrimino
    , nextTetriminoQueue : List Tetrimino
    , isGameOver : Bool
    }


init : Model
init =
    { field = Matrix.repeat 10 21 Invisible
    , tetrimino = Nothing
    , nextTetriminoQueue = []
    , isGameOver = False
    }


type alias Field =
    Matrix Color


isValidPosition : Tetrimino -> Field -> Bool
isValidPosition tetrimino field =
    tetrimino
        |> Tetrimino.toList
        |> List.all
            (\( x, y, c ) ->
                case (Matrix.get x y field) of
                    Just color ->
                        case color of
                            Invisible ->
                                True

                            _ ->
                                False

                    Nothing ->
                        False
            )


fixTetrimino : Tetrimino -> Field -> Maybe Field
fixTetrimino tetrimino field =
    if isValidPosition tetrimino field then
        Just
            (tetrimino
                |> Tetrimino.toList
                |> List.foldr
                    (\( x, y, c ) f -> Matrix.set x y c f)
                    field
            )
    else
        Nothing
