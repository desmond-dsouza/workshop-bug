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

        toString ( i, j ) =
            "(" ++ String.fromInt i ++ "," ++ String.fromInt j ++ ")"

        toStringF ( x, y ) =
            "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"
    in
    [ text "Black: i-j coordinates" |> centered |> filled black |> move ( 0, 62 )
    , text "Red: grid distances" |> centered |> filled red |> move ( 0, 42 )
    , text "Unit cell = 20x20 (grid)" |> centered |> filled red |> move ( 0, 22 )
    , text "(0,0)" |> centered |> filled black |> move ( 0, 0 )
    , text (toString ( i0, j0 )) |> centered |> filled black |> move ( i0 |> toGrid, j0 |> toGrid )
    , text (toString ( i1, j1 )) |> centered |> filled black |> move ( i1 |> toGrid, j1 |> toGrid )
    , text (toStringF ( i0 |> toGrid, j0 |> toGrid )) |> centered |> filled red |> move ( i0 |> toGrid, downY + toGrid j0 )
    , text (toStringF ( i1 |> toGrid, j1 |> toGrid )) |> centered |> filled red |> move ( i1 |> toGrid, downY + toGrid j1 )
    ]


view : List (Shape msg)
view =
    [ graphPaperCustom grid.cellSize 0.5 lightGrey

    -- , circle (0.05 * grid.cellSize) |> filled black
    , rect
        ((grid.numColumns + 1) |> toGrid)
        ((grid.numRows + 1) |> toGrid)
        |> outlined (solid 3) darkGrey
    ]


viewport =
    collage ((grid.numColumns + 8) |> toGrid) ((grid.numRows + 8) |> toGrid)


main =
    graphicsApp
        { view = viewport (view ++ viewCoordinates) }
