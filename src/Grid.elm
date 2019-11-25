module Grid exposing (fracToGrid, grid, toGrid, view, viewport, walls)

import Debug
import GraphicSVG exposing (..)
import Html exposing (Html)
import Lib.App as App
import Types exposing (..)


grid : Grid
grid =
    -- best to use even numbers for cols & rows
    { numColumns = 14, numRows = 14 }


cellSize : Float
cellSize =
    20.0


toGrid : Int -> Float
toGrid i =
    cellSize * toFloat i


fracToGrid : Float -> Float
fracToGrid f =
    cellSize * f


walls : Walls
walls =
    { left = -grid.numColumns // 2
    , right = grid.numColumns // 2
    , top = grid.numRows // 2
    , bottom = -grid.numRows // 2
    }


viewCoordinates : List (Shape msg)
viewCoordinates =
    let
        { numColumns, numRows } =
            grid

        ( i0, j0 ) =
            ( -numColumns // 2, -numRows // 2 )

        ( i1, j1 ) =
            ( numColumns // 2, numRows // 2 )
    in
    [ text "(0,0)" |> filled black |> move ( 0, 0 )
    , text (Debug.toString ( i0, j0 )) |> filled black |> move ( i0 |> toGrid, j0 |> toGrid )
    , text (Debug.toString ( i1, j1 )) |> filled black |> move ( i1 |> toGrid, j1 |> toGrid )
    ]


view : List (Shape msg)
view =
    [ graphPaper cellSize
    , rect ((grid.numColumns + 1) |> toGrid)
        ((grid.numRows + 1) |> toGrid)
        |> outlined (solid 2) black
    ]


viewport =
    collage ((grid.numColumns + 4) |> toGrid) ((grid.numRows + 4) |> toGrid)


main =
    App.graphicsApp
        { view = viewport view }
