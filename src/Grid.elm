module Grid exposing (cellSize, fracToGrid, grid, toGrid, view, viewport, walls)

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

        downY =
            -12.0
    in
    [ text "Black: i-j coordinates" |> centered |> filled black |> move ( 0, 62 )
    , text "Red: grid distances" |> centered |> filled red |> move ( 0, 42 )
    , text "Unit cell = 20x20 (grid)" |> centered |> filled red |> move ( 0, 22 )
    , text "(0,0)" |> centered |> filled black |> move ( 0, 0 )
    , text (Debug.toString ( i0, j0 )) |> centered |> filled black |> move ( i0 |> toGrid, j0 |> toGrid )
    , text (Debug.toString ( i1, j1 )) |> centered |> filled black |> move ( i1 |> toGrid, j1 |> toGrid )
    , text (Debug.toString ( i0 |> toGrid, j0 |> toGrid )) |> centered |> filled red |> move ( i0 |> toGrid, downY + toGrid j0 )
    , text (Debug.toString ( i1 |> toGrid, j1 |> toGrid )) |> centered |> filled red |> move ( i1 |> toGrid, downY + toGrid j1 )
    ]


view : List (Shape msg)
view =
    [ graphPaperCustom cellSize 0.5 lightGrey
    , rect ((grid.numColumns + 1) |> toGrid)
        ((grid.numRows + 1) |> toGrid)
        |> outlined (solid 3) darkGrey
    ]


viewport =
    collage ((grid.numColumns + 4) |> toGrid) ((grid.numRows + 4) |> toGrid)


main =
    App.graphicsApp
        { view = viewport (view ++ viewCoordinates) }
