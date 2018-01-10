module Model exposing (Msg(..), Model, init)

import Model.Field as Field


type Msg
    = FieldMsg Field.Msg


type alias Model =
    { fieldModel : Field.Model
    }


init : Model
init =
    { fieldModel = Field.init
    }
