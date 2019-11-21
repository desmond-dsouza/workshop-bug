module Food exposing (..)

-- module Food exposing (randomFoodCmd, renderFood, repositionFood)

import GraphicSVG exposing (Shape, circle, collage, darkGreen, filled, move)
import Grid exposing (grid)
import Lib.App as App
import Random
import Types exposing (..)


randomFoodCmd : Cmd Msg
randomFoodCmd =
    randomPosition NewFood


repositionFood : Bool -> Cmd Msg
repositionFood snakeAteFood =
    if snakeAteFood then
        randomPosition NewFood

    else
        Cmd.none


randomPosition : (Position -> msg) -> Cmd msg
randomPosition msg =
    Random.pair
        (Random.int -(grid.numColumns // 2) (grid.numColumns // 2))
        (Random.int -(grid.numRows // 2) (grid.numRows // 2))
        |> Random.generate msg


renderFood : Position -> List (Shape msg)
renderFood ( i, j ) =
    [ circle (grid.cellSize / 2)
        |> filled darkGreen
        |> move ( toFloat i * grid.cellSize, toFloat j * grid.cellSize )
    ]


main =
    App.graphicsApp
        { view =
            Grid.viewport
                (Grid.view ++ renderFood ( 5, 5 ))
        }
