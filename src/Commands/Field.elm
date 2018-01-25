module Commands.Field
    exposing
        ( initCmd
        , newTetrimino
        , fillQueue
        )

import Model.Field exposing (Msg(..))
import Model.Kind as Kind
import Random


initCmd : Cmd Msg
initCmd =
    newTetrimino


newTetrimino : Cmd Msg
newTetrimino =
    Random.generate
        NewTetrimino
        (Random.map
            Kind.fromInt
            (Random.int 0 6)
        )


fillQueue : Cmd Msg
fillQueue =
    Random.generate
        FillQueue
        (Random.map2
            (\n nList ->
                ( Kind.fromInt n
                , nList |> List.map Kind.fromInt
                )
            )
            (Random.int 0 6)
            (Random.list
                4
                (Random.int 0 6)
            )
        )
