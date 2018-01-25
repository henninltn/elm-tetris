module View.Field exposing (view)

import Html exposing (..)
import Html.Attributes as HA
import Model.Color exposing (Color)
import Model.Field as Field exposing (Msg(..), Model, Field)
import Svg exposing (..)
import Svg.Attributes as SA


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg
            [ HA.width 200
            , HA.height 420
            , HA.style
                [ ( "background-color", "black" )
                ]
            ]
            [ model.field
                |> fieldSvg
            , model.isGameOver
                |> gameOverSvg
            ]
        ]


fieldSvg : Field -> Svg Msg
fieldSvg field =
    g []
        (field
            |> Field.toList
            |> List.map
                blockSvg
        )


blockSize : Int
blockSize =
    20


blockSvg : ( ( Int, Int ), Color ) -> Svg Msg
blockSvg ( ( x, y ), color ) =
    rect
        [ SA.x (x * blockSize |> toString)
        , SA.y (y * blockSize |> toString)
        , SA.width (blockSize |> toString)
        , SA.height (blockSize |> toString)
        , SA.fill (color |> toString)
        ]
        []


gameOverSvg : Bool -> Svg Msg
gameOverSvg isGameOver =
    if isGameOver then
        g []
            [ rect
                [ SA.x "10"
                , SA.y "160"
                , SA.width "180"
                , SA.height "80"
                , SA.fill "Black"
                , SA.stroke "White"
                ]
                []
            , text_
                [ SA.x "100"
                , SA.y "210"
                , SA.fill "DeepPink"
                , SA.fontSize "30"
                , SA.textAnchor "middle"
                ]
                [ Svg.text "Game Over" ]
            ]
    else
        g [] []
