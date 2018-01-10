module Update exposing (update)

import Model exposing (Msg(..), Model)
import Update.Field as Field


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FieldMsg subMsg ->
            let
                ( updatedFieldModel, fieldCmd ) =
                    Field.update subMsg model.fieldModel
            in
                ( { model | fieldModel = updatedFieldModel }
                , Cmd.map FieldMsg fieldCmd
                )
