module Subscriptions exposing (subscriptions)

import Model exposing (Model, Msg(..))
import Subscriptions.Field as Field


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map
            FieldMsg
            (Field.subscriptions model.fieldModel)
        ]
