module Subscriptions.Field exposing (subscriptions)

import Keyboard.Extra as Keyboard
import Model.Field exposing (Msg(..), Model)
import Time exposing (Time)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (500 * Time.millisecond) FreeFall
        , Keyboard.downs KeyDowns
        ]
