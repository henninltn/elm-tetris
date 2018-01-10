module Subscriptions.Field exposing (subscriptions)

import Model.Field exposing (Msg(..), Model)
import Time exposing (Time)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (500 * Time.millisecond) FreeFall
