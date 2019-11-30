module Main exposing (..)

import Food
import GraphicSVG exposing (Shape, app, blue, centered, collage, filled, move, red, size, text)
import Grid
import Keyboard exposing (Key(..))
import Snake
import Time
import Types exposing (..)



-- MAIN -------------------


main : GraphicSVG.App () Model Msg
main =
    let
        initF _ _ _ =
            initialModelCmd

        viewF model =
            { title = "Snake", body = Grid.viewport (view model) }

        emptyF _ =
            NoOp
    in
    app
        { init = initF
        , view = viewF
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = emptyF
        , onUrlRequest = emptyF
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


initialModelCmd : ( Model, Cmd Msg )
initialModelCmd =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { snake =
        { head = ( -2, 0 )
        , body =
            [ ( -3, 0 ), ( -4, 0 ) ]
        , direction = Right
        , state = Normal
        }
    , food = ( Grid.grid.numColumns // 2 - 1, 0 )
    }



-- VIEW ----------------


view : Model -> List (Shape Msg)
view model =
    Grid.view
        ++ Food.view model.food
        ++ Snake.view model.snake
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
    [ text "GAME OVER" |> size (1.5 |> Grid.fracToGrid) |> centered |> filled red ]



-- UPDATE ----------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            step Grid.walls model

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
                    ( { model | snake = Snake.turn key snake }
                    , Cmd.none
                    )

        NewFood food ->
            ( { model | food = food }, Cmd.none )


step : Walls -> Model -> ( Model, Cmd Msg )
step walls model =
    let
        newSnake =
            Snake.stepSnake model.food walls model.snake
    in
    ( { model | snake = newSnake }
    , if newSnake.state == Eating then
        Food.randomFoodCmd

      else
        Cmd.none
    )
