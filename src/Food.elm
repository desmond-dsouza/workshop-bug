module Food exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (graphicsApp)
import Grid exposing (grid, toGrid)
import Random
import Types exposing (Food, Msg(..), Position)


randomFoodCmd : Cmd Msg
randomFoodCmd =
    let
        walls =
            Grid.walls

        positionGenerator =
            Random.pair (Random.int walls.left walls.right) (Random.int walls.bottom walls.top)
    in
    positionGenerator |> Random.generate NewFood


view : Food -> List (Shape msg)
view ( i, j ) =
    [ apple (1 |> toGrid)
        |> move ( i |> toGrid, j |> toGrid )
    ]


apple : Float -> Shape msg
apple size =
    group
        [ line ( -0.1 * size, 0.2 * size ) ( 0.2 * size, 0.8 * size )
            |> outlined (solid (size * 0.1)) darkGreen
        , oval (size * 0.6) size |> filled darkRed |> move ( -size * 0.2, 0 )
        , oval (size * 0.6) size |> filled darkRed |> move ( size * 0.2, 0 )
        ]


main =
    graphicsApp
        { view =
            Grid.viewport
                (Grid.view ++ view ( 5, 5 ))
        }
