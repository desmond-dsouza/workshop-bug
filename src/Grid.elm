module Grid exposing (fracToGrid, grid, toGrid, view, viewport, walls)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (graphicsApp)
import Types exposing (..)


grid : Grid
grid =
    -- best to use even numbers for cols & rows
    { numColumns = 14, numRows = 14, cellSize = 20.0 }


toGrid : Int -> Float
toGrid i =
    grid.cellSize * toFloat i


fracToGrid : Float -> Float
fracToGrid f =
    grid.cellSize * f


walls : Walls
walls =
    { left = -grid.numColumns // 2
    , right = grid.numColumns // 2
    , top = grid.numRows // 2
    , bottom = -grid.numRows // 2
    }


view : List (Shape msg)
view =
    [ graphPaperCustom grid.cellSize 0.5 lightGrey
    , rect
        ((grid.numColumns + 1) |> toGrid)
        ((grid.numRows + 1) |> toGrid)
        |> outlined (solid 3) darkGrey
    ]


viewport =
    collage ((grid.numColumns + 6) |> toGrid) ((grid.numRows + 6) |> toGrid)


main =
    graphicsApp
        { view = viewport view }
