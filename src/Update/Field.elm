module Update.Field exposing (update)

import Commands.Field as Commands
import Keyboard.Extra exposing (Key(..))
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

            KeyDowns key ->
                case model.field.current of
                    Just _ ->
                        let
                            updatedField =
                                model.field
                                    |> if
                                        List.member
                                            key
                                            [ ArrowRight, CharL, CharD ]
                                       then
                                        Field.moveRight
                                       else if
                                        List.member
                                            key
                                            [ ArrowDown, CharJ, CharS ]
                                       then
                                        Field.moveDown
                                       else if
                                        List.member
                                            key
                                            [ ArrowLeft, CharH, CharA ]
                                       then
                                        Field.moveLeft
                                       else if
                                        List.member
                                            key
                                            [ CharR, CharI ]
                                       then
                                        Field.rotateRight
                                       else if
                                        List.member
                                            key
                                            [ CharE, CharU ]
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
