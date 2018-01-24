module View.Field exposing (view)

import Array.Hamt as Array
import Html exposing (..)
import Html.Attributes as HA
import Matrix exposing (Matrix)
import Model.Color exposing (Color)
import Model.Field as Field exposing (Msg(..), Model, Field)
import Model.Position exposing (Position)
import Model.Tetrimino as Tetrimino exposing (Tetrimino)
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
            , model.tetrimino
                |> maybeTetriminoSvg
            , model.isGameOver
                |> gameOverSvg
            ]
        ]


blockSize : Int
blockSize =
    20


blockSvg : Position -> Color -> Svg Msg
blockSvg position color =
    rect
        [ SA.x (position.x * blockSize |> toString)
        , SA.y (position.y * blockSize |> toString)
        , SA.width (blockSize |> toString)
        , SA.height (blockSize |> toString)
        , SA.fill (color |> toString)
        ]
        []


maybeTetriminoSvg : Maybe Tetrimino -> Svg Msg
maybeTetriminoSvg maybeTetrimino =
    maybeTetrimino
        |> Maybe.map
            (\t -> tetriminoSvg t)
        |> Maybe.withDefault
            (g [] [])


tetriminoSvg : Tetrimino -> Svg Msg
tetriminoSvg tetrimino =
    let
        posX =
            tetrimino.position.x * blockSize

        posY =
            tetrimino.position.y * blockSize
    in
        g
            [ SA.x (posX |> toString)
            , SA.y (posY |> toString)
            ]
            (tetrimino
                |> Tetrimino.toPositionColorPairList
                |> List.map
                    (\( p, c ) ->
                        rect
                            [ SA.x (p.x * blockSize |> toString)
                            , SA.y (p.y * blockSize |> toString)
                            , SA.width (blockSize |> toString)
                            , SA.height (blockSize |> toString)
                            , SA.fill (c |> toString)
                            ]
                            []
                    )
            )


fieldSvg : Field -> Svg Msg
fieldSvg field =
    g []
        (field
            |> Matrix.toIndexedArray
            |> Array.toList
            |> List.map
                (\( ( x, y ), c ) -> blockSvg { x = x, y = y } c)
        )


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
