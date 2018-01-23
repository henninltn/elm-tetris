module Update.Field exposing (update)

import Char
import Matrix exposing (loc)
import Model.Direction exposing (Direction(..))
import Model.Field as Field exposing (Msg(..), Model)
import Model.Tetrimino as Tetrimino exposing (Tetrimino, Kind(..))
import Random exposing (Generator)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.isGameOver then
        ( model, Cmd.none )
    else
        case msg of
            FreeFall _ ->
                case model.tetrimino of
                    Just tetrimino ->
                        if Field.isValidLocation tetrimino model.field then
                            let
                                updatedTetrimino =
                                    Tetrimino.moveDown tetrimino
                            in
                                if
                                    Field.isValidLocation
                                        updatedTetrimino
                                        model.field
                                then
                                    ( { model
                                        | tetrimino = Just updatedTetrimino
                                      }
                                    , Cmd.none
                                    )
                                else
                                    ( { model
                                        | tetrimino = List.head model.nextTetriminoQueue
                                        , nextTetriminoQueue = List.drop 1 model.nextTetriminoQueue
                                        , field =
                                            model.field
                                                |> Field.fixTetrimino tetrimino
                                                |> Maybe.withDefault
                                                    model.field
                                      }
                                    , Random.generate
                                        AddTetrimino
                                        generateKind
                                    )
                        else
                            ( { model | isGameOver = True }, Cmd.none )

                    Nothing ->
                        ( model
                        , Random.generate
                            Init
                            generateKindList
                        )

            Init kindList ->
                ( { model
                    | tetrimino =
                        kindList
                            |> List.head
                            |> Maybe.map
                                (\kind ->
                                    { kind = kind
                                    , location = loc 1 4
                                    , direction = Up
                                    }
                                )
                    , nextTetriminoQueue =
                        kindList
                            |> List.drop 1
                            |> List.map
                                (\kind ->
                                    { kind = kind
                                    , location = loc 1 4
                                    , direction = Up
                                    }
                                )
                  }
                , Cmd.none
                )

            AddTetrimino kind ->
                ( { model
                    | nextTetriminoQueue =
                        model.nextTetriminoQueue
                            |> List.append
                                [ { kind = kind
                                  , location = loc 1 4
                                  , direction = Up
                                  }
                                ]
                  }
                , Cmd.none
                )

            KeyPresses keyCode ->
                case model.tetrimino of
                    Just tetrimino ->
                        let
                            updatedTetrimino =
                                if
                                    keyCode
                                        == 13
                                        || keyCode
                                        == (Char.toCode 'h')
                                        || keyCode
                                        == (Char.toCode 'a')
                                then
                                    tetrimino |> Tetrimino.moveLeft
                                else if
                                    keyCode
                                        == 17
                                        || keyCode
                                        == (Char.toCode 'l')
                                        || keyCode
                                        == (Char.toCode 'd')
                                then
                                    tetrimino |> Tetrimino.moveRight
                                else if
                                    keyCode
                                        == 18
                                        || keyCode
                                        == (Char.toCode 'j')
                                        || keyCode
                                        == (Char.toCode 's')
                                then
                                    tetrimino |> Tetrimino.moveDown
                                else
                                    tetrimino
                        in
                            if Field.isValidLocation updatedTetrimino model.field then
                                ( { model | tetrimino = Just updatedTetrimino }
                                , Cmd.none
                                )
                            else
                                ( model, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )


generateKind : Generator Kind
generateKind =
    (Random.int 0 6)
        |> Random.map
            (\i ->
                i
                    |> Tetrimino.intToKind
                    |> Maybe.withDefault I
            )


generateKindList : Generator (List Kind)
generateKindList =
    (Random.list 6 (Random.int 0 6))
        |> Random.map
            (\list ->
                list
                    |> List.map (\i -> i |> Tetrimino.intToKind |> Maybe.withDefault I)
            )
