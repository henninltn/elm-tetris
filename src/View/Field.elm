module View.Field exposing (view)

import Html exposing (..)
import Html.Attributes as HA
import Matrix exposing (Matrix, Location)
import Model.Field as Field exposing (Msg(..), Model, Field)
import Model.Tetrimino as Tetrimino exposing (Tetrimino)
import Svg exposing (..)
import Svg.Attributes as SA


view : Model -> Html Msg
view model =
    div []
        [ Svg.svg
            [ HA.width 1000
            , HA.height 1000
            , HA.style
                [ ( "background-color", "black" )
                ]
            ]
            [ model.tetrimino
                |> Maybe.map (\t -> tetriminoSvg t)
                |> Maybe.withDefault (g [] [])
            ]
        ]


blockSize : Int
blockSize =
    10


getX : Location -> Int
getX location =
    location |> Matrix.col |> ((*) blockSize)


getY : Location -> Int
getY location =
    location |> Matrix.row |> ((*) blockSize)


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
                            [ SA.x (l |> getX |> ((+) posX) |> toString)
                            , SA.y (l |> getY |> ((+) posY) |> toString)
                            , SA.width (blockSize |> toString)
                            , SA.height (blockSize |> toString)
                            , SA.fill (c |> toString)
                            ]
                            []
                    )
            )
