module Model.Kind
    exposing
        ( Kind(..)
        , fromInt
        , toColor
        )

import Model.Color exposing (Color(..))


type Kind
    = I
    | O
    | S
    | Z
    | J
    | L
    | T


fromInt : Int -> Kind
fromInt number =
    let
        r =
            rem number 7
    in
        if r == 0 then
            I
        else if r == 1 then
            O
        else if r == 2 then
            S
        else if r == 3 then
            Z
        else if r == 4 then
            J
        else if r == 5 then
            L
        else
            T


toColor : Kind -> Color
toColor kind =
    case kind of
        I ->
            Lightblue

        O ->
            Yellow

        S ->
            Yellowgreen

        Z ->
            Red

        J ->
            Blue

        L ->
            Orange

        T ->
            Purple
