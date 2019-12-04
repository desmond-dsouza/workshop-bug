module Blotter exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)



-- in this example, we
-- 1) get taps from anywhere on the screen,
-- 2) add to a list, and
-- 3) use List.map to draw circles at all the points in our list
-- The interesting thing here is that List.map has two inputs, and the first is
-- a function which takes an (x,y) position as an input and outputs a shape.


myShapes model =
    List.map drawAt model.positions
        ++ [ rect 192 128 |> filled (rgba 0 0 0 0) |> addOutline (solid 1) grey |> notifyTapAt TapAt
           ]



-- Maybe you haven't seen (rgba 0 0 0 0) it is a completely transparent colour,
-- which we want so nobody can see the rect we put on top of the shapes for
-- the user to tap on.
-- This function will draw a red circle at every point you tap.
-- For fun you can make it into something more fun (like a happy face).
-- Remember to use "group" to group a list of shapes into one shape.


drawAt : ( Float, Float ) -> Shape Msg
drawAt pos =
    circle 10 |> filled red |> move pos


type Msg
    = Tick Float GetKeyState
    | TapAt ( Float, Float )


update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }

        TapAt pos ->
            { model | positions = pos :: model.positions }


init : { time : Float, positions : List ( Float, Float ) }
init =
    { time = 0, positions = [] }


main =
    gameApp Tick
        { model = init
        , title = "Title"
        , view = \m -> Collage 500 500 (myShapes m)
        , update = update
        }
