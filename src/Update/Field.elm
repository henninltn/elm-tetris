module Update.Field exposing (update)

import Model.Direction exposing (Direction(..))
import Model.Field as Field exposing (Msg(..), Model)
import Model.Tetrimino as Tetrimino exposing (Tetrimino, Kind(..))
import Random exposing (Generator)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FreeFall _ ->
            case model.tetrimino of
                Just tetrimino ->
                    let
                        updatedTetrimino =
                            Tetrimino.moveBottom tetrimino
                    in
                        if
                            Field.isValidLocation
                                updatedTetrimino
                                model.field
                        then
                            ( { model | tetrimino = Just updatedTetrimino }
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
                                , location = Field.initCurrentTetriminoLocation
                                , direction = Top
                                }
                            )
                , nextTetriminoQueue =
                    kindList
                        |> List.drop 1
                        |> List.map
                            (\kind ->
                                { kind = kind
                                , location = Field.initNextTetriminoLocation
                                , direction = Top
                                }
                            )
              }
            , Cmd.none
            )

        AddTetrimino kind ->
            ( Debug.log "AddTetrimino: "
                { model
                    | nextTetriminoQueue =
                        model.nextTetriminoQueue
                            |> List.append
                                [ { kind = kind
                                  , location = Field.initNextTetriminoLocation
                                  , direction = Top
                                  }
                                ]
                }
            , Cmd.none
            )


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
