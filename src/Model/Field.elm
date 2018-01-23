module Model.Field
    exposing
        ( Msg(..)
        , Model
        , Field
        , init
        , isValidLocation
        , fixTetrimino
        )

import Keyboard exposing (KeyCode)
import Matrix exposing (Location, Matrix)
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
    { field = Matrix.matrix 21 10 (\_ -> Invisible)
    , tetrimino = Nothing
    , nextTetriminoQueue = []
    , isGameOver = False
    }


type alias Field =
    Matrix Color


isValidLocation : Tetrimino -> Field -> Bool
isValidLocation tetrimino field =
    tetrimino
        |> Tetrimino.toLocationColorPairList
        |> List.all
            (\( l, c ) ->
                case (Matrix.get l field) of
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
    if isValidLocation tetrimino field then
        Just
            (tetrimino
                |> Tetrimino.toLocationColorPairList
                |> List.foldr
                    (\( l, c ) f -> Matrix.set l c f)
                    field
            )
    else
        Nothing
