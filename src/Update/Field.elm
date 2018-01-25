module Update.Field exposing (update)

import Char
import Model.Direction exposing (Direction(..))
import Model.Field as Field exposing (Msg(..), Model)
import Model.Tetrimino as Tetrimino exposing (Tetrimino)
import Model.Kind as Kind exposing (Kind(..))
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
                        if Field.isValidPosition tetrimino model.field then
                            let
                                updatedTetrimino =
                                    Tetrimino.moveDown tetrimino
                            in
                                if
                                    Field.isValidPosition
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
                                    , position = { x = 4, y = 1 }
                                    , direction = Up
                                    }
                                )
                    , nextTetriminoQueue =
                        kindList
                            |> List.drop 1
                            |> List.map
                                (\kind ->
                                    { kind = kind
                                    , position = { x = 4, y = 1 }
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
                                  , position = { x = 4, y = 1 }
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
                            key =
                                Char.fromCode keyCode

                            updatedTetrimino =
                                if
                                    key
                                        == 'h'
                                        || key
                                        == 'a'
                                then
                                    tetrimino |> Tetrimino.moveLeft
                                else if
                                    key
                                        == 'l'
                                        || key
                                        == 'd'
                                then
                                    tetrimino |> Tetrimino.moveRight
                                else if
                                    key
                                        == 'j'
                                        || key
                                        == 's'
                                then
                                    tetrimino |> Tetrimino.moveDown
                                else if
                                    key
                                        == 'r'
                                        || key
                                        == 'i'
                                then
                                    tetrimino |> Tetrimino.rotateRight
                                else if
                                    key
                                        == 'e'
                                        || key
                                        == 'u'
                                then
                                    tetrimino |> Tetrimino.rotateLeft
                                else
                                    tetrimino
                        in
                            if Field.isValidPosition updatedTetrimino model.field then
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
                    |> Kind.fromInt
            )


generateKindList : Generator (List Kind)
generateKindList =
    (Random.list 6 (Random.int 0 6))
        |> Random.map
            (\list ->
                list
                    |> List.map (\i -> i |> Kind.fromInt)
            )
