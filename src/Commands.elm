module Commands exposing (initCmd)

import Model exposing (Msg(..))
import Commands.Field as Field


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ Cmd.map
            FieldMsg
            Field.initCmd
        ]
