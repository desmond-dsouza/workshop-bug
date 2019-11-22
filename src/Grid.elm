module Grid exposing (grid, view, viewport, walls)

import Debug
import GraphicSVG exposing (..)
import Html exposing (Html)
import Lib.App as App
import Types exposing (..)


grid : Grid
grid =
    -- best to use even numbers for cols & rows
    { numColumns = 14, numRows = 14, cellSize = 20 }


walls =
    { left = -grid.numColumns // 2
    , right = grid.numColumns // 2
    , top = grid.numRows // 2
    , bottom = -grid.numRows // 2
    }


viewCoordinates : List (Shape msg)
viewCoordinates =
    let
        { numColumns, numRows, cellSize } =
            grid

        ( i0, j0 ) =
            ( -numColumns // 2, -numRows // 2 )

        ( i1, j1 ) =
            ( numColumns // 2, numRows // 2 )
    in
    [ text "(0,0)" |> filled black |> move ( 0, 0 )
    , text (Debug.toString ( i0, j0 )) |> filled black |> move ( cellSize * toFloat i0, cellSize * toFloat j0 )
    , text (Debug.toString ( i1, j1 )) |> filled black |> move ( cellSize * toFloat i1, cellSize * toFloat j1 )
    ]


view : List (Shape msg)
view =
    [ graphPaper grid.cellSize
    , rect (grid.cellSize * toFloat (grid.numColumns + 1))
        (grid.cellSize * toFloat (grid.numRows + 1))
        |> outlined (solid 2) black
    ]


viewport =
    collage (grid.cellSize * toFloat (grid.numColumns + 4)) (grid.cellSize * toFloat (grid.numRows + 4))


main =
    App.graphicsApp
        { view = viewport view }
