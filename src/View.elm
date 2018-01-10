module View exposing (view)

import Html exposing (Html, div)
import Model exposing (Msg(..), Model)
import View.Field as Field


view : Model -> Html Msg
view { fieldModel } =
    div []
        [ Html.map FieldMsg (Field.view fieldModel)
        ]
