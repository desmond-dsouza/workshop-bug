module Food exposing (..)

-- module Food exposing (randomFoodCmd, view, repositionFood)
-- Shape, circle, collage, darkGreen, darkRed, filled, move, oval)

import GraphicSVG exposing (..)
import Grid exposing (grid)
import Lib.App as App
import Random
import Types


randomFoodCmd : Cmd Types.Msg
randomFoodCmd =
    randomPosition Types.NewFood


repositionFood : Bool -> Cmd Types.Msg
repositionFood snakeAteFood =
    if snakeAteFood then
        randomPosition Types.NewFood

    else
        Cmd.none


randomPosition : (Types.Position -> msg) -> Cmd msg
randomPosition msg =
    Random.pair
        (Random.int -(grid.numColumns // 2) (grid.numColumns // 2))
        (Random.int -(grid.numRows // 2) (grid.numRows // 2))
        |> Random.generate msg


view : Types.Food -> List (Shape msg)
view ( i, j ) =
    [ group (apple grid.cellSize)
        |> move ( toFloat i * grid.cellSize, toFloat j * grid.cellSize )
    ]


apple : Float -> List (Shape msg)
apple size =
    [ line ( -0.1 * size, 0.2 * size ) ( 0.2 * size, 0.8 * size )
        |> outlined (solid (size * 0.1)) darkGreen
    , oval (size * 0.6) size |> filled darkRed |> move ( -size * 0.2, 0 )
    , oval (size * 0.6) size |> filled darkRed |> move ( size * 0.2, 0 )
    ]


main =
    App.graphicsApp
        { view =
            Grid.viewport
                (Grid.view ++ view ( 5, 5 ))
        }
