module Model.Field
    exposing
        ( Msg(..)
        , Model
        , Field
        , init
        , initCurrentTetriminoLocation
        , initNextTetriminoLocation
        , isValidLocation
        , fixTetrimino
        )

import Matrix exposing (Location, Matrix)
import Model.Color exposing (Color(..))
import Model.Tetrimino as Tetrimino exposing (Tetrimino, Kind)
import Time exposing (Time)


type Msg
    = FreeFall Time
    | Init (List Kind)
    | AddTetrimino Kind


type alias Model =
    { field : Field
    , tetrimino : Maybe Tetrimino
    , nextTetriminoQueue : List Tetrimino
    }


init : Model
init =
    { field = Matrix.matrix 24 10 (\_ -> Invisible)
    , tetrimino = Nothing
    , nextTetriminoQueue = []
    }


initCurrentTetriminoLocation : Location
initCurrentTetriminoLocation =
    ( 3, 4 )


initNextTetriminoLocation : Location
initNextTetriminoLocation =
    ( 6, 4 )


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
