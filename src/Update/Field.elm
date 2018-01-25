module Update.Field exposing (update)

import Char
import Commands.Field as Commands
import Model.Field as Field exposing (Msg(..), Model, Field)
import Queue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.isGameOver then
        ( model, Cmd.none )
    else
        case msg of
            Init ->
                ( model
                , Commands.newTetrimino
                )

            NewTetrimino kind ->
                let
                    ( nextTetrimino, updatedQueue ) =
                        Queue.deq model.queue
                in
                    case nextTetrimino of
                        Just tetrimino ->
                            ( { model
                                | field =
                                    model.field
                                        |> Field.setCurrent
                                            tetrimino
                                , queue =
                                    updatedQueue
                                        |> Queue.enq
                                            (Field.initTetrimino kind)
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model
                            , Commands.fillQueue
                            )

            FillQueue ( kind, kindList ) ->
                ( { model
                    | queue =
                        kind
                            :: kindList
                            |> List.foldr
                                (\kind queue ->
                                    queue
                                        |> Queue.enq
                                            (Field.initTetrimino kind)
                                )
                                Queue.empty
                  }
                , Commands.newTetrimino
                )

            FreeFall _ ->
                case Field.fixTetrimino model.field of
                    Just fixedField ->
                        case model.field.current of
                            Just _ ->
                                case Field.moveDown model.field of
                                    Just movedField ->
                                        ( { model
                                            | field = movedField
                                          }
                                        , Cmd.none
                                        )

                                    Nothing ->
                                        ( { model
                                            | field = fixedField
                                          }
                                        , Commands.newTetrimino
                                        )

                            Nothing ->
                                ( model, Cmd.none )

                    Nothing ->
                        ( { model | isGameOver = True }, Cmd.none )

            KeyDowns keyCode ->
                case model.field.current of
                    Just _ ->
                        let
                            key =
                                Char.fromCode keyCode

                            updatedField =
                                model.field
                                    |> if
                                        key
                                            == 'L'
                                            || key
                                            == 'D'
                                            || keyCode
                                            == 39
                                       then
                                        Field.moveRight
                                       else if
                                        key
                                            == 'J'
                                            || key
                                            == 'S'
                                            || keyCode
                                            == 40
                                       then
                                        Field.moveDown
                                       else if
                                        key
                                            == 'H'
                                            || key
                                            == 'A'
                                            || keyCode
                                            == 37
                                       then
                                        Field.moveLeft
                                       else if
                                        key
                                            == 'R'
                                            || key
                                            == 'I'
                                       then
                                        Field.rotateRight
                                       else if
                                        key
                                            == 'E'
                                            || key
                                            == 'U'
                                       then
                                        Field.rotateLeft
                                       else
                                        Just
                        in
                            case updatedField of
                                Just field ->
                                    ( { model | field = field }
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )
