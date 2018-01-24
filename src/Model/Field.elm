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
import Model.Tetrimino as Tetrimino exposing (Tetrimino, Kind)
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
        |> Tetrimino.toPositionColorPairList
        |> List.all
            (\( p, c ) ->
                case (Matrix.get p.x p.y field) of
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
                |> Tetrimino.toPositionColorPairList
                |> List.foldr
                    (\( p, c ) f -> Matrix.set p.x p.y c f)
                    field
            )
    else
        Nothing
