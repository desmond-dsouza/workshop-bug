module Main exposing (main)

-- import Html exposing (Html, div, h1, p)
-- import Browser
-- import Svg exposing (svg)
-- import Svg.Attributes exposing (class, fill, fontSize, fontWeight, height, textAnchor, viewBox, width, x, y)

import Food
import GraphicSVG exposing (Shape, blue, collage, filled, move, red, size, text)
import Grid
import Keyboard exposing (Key(..))
import Lib.App as App
import Snake
import Time
import Types exposing (..)


main : GraphicSVG.App () Model Msg
main =
    GraphicSVG.app
        { init = \_ _ _ -> init
        , view = \m -> { title = "SNEK", body = collage 800 800 (view m) } -- Grid.viewport (view m) }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \u -> NoOp
        }



-- INIT ----------------


init : ( Model, Cmd Msg )
init =
    ( initialModel, Food.randomFoodCmd )


initialModel : Model
initialModel =
    { snake =
        { head = ( -7, 0 )
        , body =
            [ ( -8, 0 )
            , ( -9, 0 )
            , ( -10, 0 )
            ]
        , direction = Right
        , state = Normal
        }
    , food = ( 10, 0 )
    }



-- VIEW ----------------


view : Model -> List (Shape Msg)
view model =
    Grid.view
        ++ Snake.renderSnake model.snake
        ++ Food.renderFood model.food
        ++ (if isGameOver model then
                renderGameOver

            else
                []
           )


isGameOver : Model -> Bool
isGameOver g =
    g.snake.state == Types.HitSelf || g.snake.state == Types.HitWall


renderGameOver : List (Shape Msg)
renderGameOver =
    [ text "GAME OVER" |> size 24 |> filled red ]



-- UPDATE ----------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            step model

        KeyDown k ->
            let
                key =
                    Keyboard.anyKeyOriginal k
            in
            case ( model.snake.state, key ) of
                ( HitSelf, Just Keyboard.Spacebar ) ->
                    ( initialModel, Food.randomFoodCmd )

                ( HitWall, Just Keyboard.Spacebar ) ->
                    ( initialModel, Food.randomFoodCmd )

                ( HitSelf, _ ) ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | snake =
                            Snake.updateSnakeDirection
                                model.snake
                                (Types.arrowToDirection key)
                      }
                    , Cmd.none
                    )

        NewFood food ->
            ( { model | food = food }, Cmd.none )


step : Model -> ( Model, Cmd Msg )
step model =
    let
        newSnake =
            Snake.stepSnake model.snake model.food
    in
    ( { model | snake = newSnake }
    , Food.repositionFood
        (if newSnake.state == Eating then
            True

         else
            False
        )
    )



-- SUBSCRIPTIONS ----------------


tickDelta =
    200


subscriptions : Model -> Sub Msg
subscriptions m =
    if isGameOver m then
        Keyboard.downs KeyDown

    else
        Sub.batch
            [ Keyboard.downs KeyDown
            , Time.every tickDelta Tick
            ]
