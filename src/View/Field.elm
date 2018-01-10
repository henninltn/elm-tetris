module View.Field exposing (view)

import Html exposing (..)
import Html.Attributes as HA
import Matrix exposing (Matrix, Location)
import Model.Color exposing (Color)
import Model.Field as Field exposing (Msg(..), Model, Field)
import Model.Tetrimino as Tetrimino exposing (Tetrimino)
import Svg exposing (..)
import Svg.Attributes as SA


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg
            [ HA.width 300
            , HA.height 500
            , HA.style
                [ ( "background-color", "black" )
                ]
            ]
            [ model.field
                |> fieldSvg
            , model.tetrimino
                |> Maybe.map (\t -> tetriminoSvg t)
                |> Maybe.withDefault (g [] [])
            ]
        ]


blockSize : Int
blockSize =
    20


getX : Location -> Int
getX location =
    location |> Matrix.col |> ((*) blockSize)


getY : Location -> Int
getY location =
    location |> Matrix.row |> ((*) blockSize)


blockSvg : Location -> Color -> Svg Msg
blockSvg location color =
    rect
        [ SA.x (location |> getX |> toString)
        , SA.y (location |> getY |> toString)
        , SA.width (blockSize |> toString)
        , SA.height (blockSize |> toString)
        , SA.fill (color |> toString)
        ]
        []


tetriminoSvg : Tetrimino -> Svg Msg
tetriminoSvg tetrimino =
    let
        posX =
            tetrimino.location |> getX

        posY =
            tetrimino.location |> getY
    in
        g
            [ SA.x (posX |> toString)
            , SA.y (posY |> toString)
            ]
            (tetrimino
                |> Tetrimino.toLocationColorPairList
                |> List.map
                    (\( l, c ) ->
                        rect
                            [ SA.x (l |> getX |> toString)
                            , SA.y (l |> getY |> toString)
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
            |> Matrix.mapWithLocation
                (\l c -> blockSvg l c)
            |> Matrix.flatten
        )
