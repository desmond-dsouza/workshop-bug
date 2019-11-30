module SnakePic exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Html


main =
    GraphicSVG.App.gameApp
        Tick
        { view = viewSnake
        , model = init
        , update = update
        , title = ""
        }


type alias Model =
    { dir : Dir, dx : Float }


init =
    { dir = Left, dx = 0.0 }


update (Tick t ( keys, _, _ )) snake =
    let
        _ =
            Debug.log "keys" keys
    in
    { snake | dx = snake.dx + 0.5 }


type Dir
    = Up
    | Down
    | Left
    | Right


type Msg
    = Tick Float GetKeyState



-- viewSnake : Snake -> Collage ()


viewSnake snake =
    let
        field =
            rect 450 250 |> filled lightGrey

        serpent =
            group
                [ roundedRect 250 40 20 |> filled green
                , roundedRect 40 60 20 |> filled green |> move ( 80, 0 )
                , circle 10 |> filled white |> move ( 85, 12 )
                , circle 10 |> filled white |> move ( 85, -12 )
                , circle 5 |> filled black |> move ( 90, 12 )
                , circle 5 |> filled black |> move ( 90, -12 )
                , wedge 15 0.5 |> filled red |> move ( 105, 0 )
                ]
                
        coords =
            group
                [ text "0,0" |> size 8 |> filled black |> move ( 0, 0 )
                , text "0,100" |> size 8 |> filled black |> move ( 0, 100 )
                , text "100,0" |> size 8 |> filled black |> move ( 100, 0 )
                , text "100,100" |> size 8 |> filled black |> move ( 100, 100 )
                ]
    in
    collage 450
        250
        [ field, coords, serpent |> move ( snake.dx, 0 ) ]
