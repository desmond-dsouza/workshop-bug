module Main exposing (..)

import Food
import GraphicSVG exposing (Shape, blue, centered, collage, filled, move, red, size, text)
import Grid
import Keyboard exposing (Key(..))
import Lib.App as App
import Snake
import Time
import Types exposing (..)



-- MAIN -------------------


main : GraphicSVG.App () Model Msg
main =
    GraphicSVG.app
        { init = \_ _ _ -> init
        , view =
            \m ->
                { title = "SNEK"
                , body = Grid.viewport (view m)
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \u -> NoOp
        }



-- SUBSCRIPTIONS ----------------


tickDelay =
    500


subscriptions : Model -> Sub Msg
subscriptions m =
    if isGameOver m then
        Keyboard.downs KeyDown

    else
        Sub.batch
            [ Keyboard.downs KeyDown
            , Time.every tickDelay Tick
            ]



-- INIT ----------------


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { snake =
        { head = ( -2, 0 )
        , body =
            [ ( -3, 0 ) ]
        , direction = Right
        , state = Normal
        }
    , food = ( Grid.grid.numColumns // 2 - 1, 0 )
    }



-- VIEW ----------------


view : Model -> List (Shape Msg)
view model =
    Grid.view
        ++ Snake.view model.snake
        ++ Food.view model.food
        ++ (if isGameOver model then
                viewGameOver

            else
                []
           )


isGameOver : Model -> Bool
isGameOver g =
    g.snake.state == Types.HitSelf || g.snake.state == Types.HitWall


viewGameOver : List (Shape Msg)
viewGameOver =
    [ text "GAME OVER" |> size 24 |> centered |> filled red ]



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
                    let
                        snake =
                            model.snake
                    in
                    ( { model | snake = Snake.changeDirection key snake }
                    , Cmd.none
                    )

        NewFood food ->
            ( { model | food = food }, Cmd.none )


step : Model -> ( Model, Cmd Msg )
step model =
    let
        newSnake =
            Snake.stepSnake model.food model.snake
    in
    ( { model | snake = newSnake }
    , if newSnake.state == Eating then
        Food.randomFoodCmd

      else
        Cmd.none
    )
