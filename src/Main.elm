module Main exposing (..)

-- Collage, Shape, black, blue, centered, circle, collage, filled, move, notifyTap, outlined, red, size, text)

import Food
import GraphicSVG exposing (..)
import Grid
import Lib.WkApp as App exposing (KeyState(..), Keys(..))
import Snake
import Types exposing (..)



-- MAIN -------------------


main =
    App.cmdGameApp
        (App.Every 150)
        Tick
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , title = "Snake"
        }



-- INIT ----------------


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


view : Model -> Collage Types.Msg
view model =
    Grid.viewport
        (Grid.view
            ++ maybeNewGameButton model
            ++ Food.view model.food
            ++ Snake.view model.snake
            ++ (if isGameOver model then
                    viewGameOver

                else
                    []
               )
        )


maybeNewGameButton model =
    if isGameOver model then
        [ text "Click to Play Again"
            |> sansserif
            |> centered
            |> filled black
            |> notifyTap Types.NewGame
            |> move ( 0, 180 )
        ]

    else
        []


isGameOver : Model -> Bool
isGameOver g =
    g.snake.state == Types.HitSelf || g.snake.state == Types.HitWall


viewGameOver : List (Shape Types.Msg)
viewGameOver =
    [ text "GAME OVER" |> size (1.5 |> Grid.fracToGrid) |> centered |> filled red ]



-- UPDATE ----------------


type UserRequest
    = NewGame
    | Turn Direction
    | None


decodeKeys : (Keys -> KeyState) -> UserRequest
decodeKeys keyF =
    if keyF Space == JustDown then
        NewGame

    else if keyF LeftArrow == JustDown then
        Turn Types.Left

    else if keyF DownArrow == JustDown then
        Turn Types.Down

    else if keyF RightArrow == JustDown then
        Turn Types.Right

    else if keyF UpArrow == JustDown then
        Turn Types.Up

    else
        None


update : Types.Msg -> Model -> ( Model, Cmd Types.Msg )
update msg model =
    case msg of
        Tick time ( keyFunc, sumOfArrows1, sumOfArrows2 ) ->
            case ( model.snake.state, decodeKeys keyFunc ) of
                ( HitSelf, NewGame ) ->
                    ( initialModel, Food.randomFoodCmd )

                ( HitWall, NewGame ) ->
                    ( initialModel, Food.randomFoodCmd )

                ( HitSelf, _ ) ->
                    ( model, Cmd.none )

                ( HitWall, _ ) ->
                    ( model, Cmd.none )

                ( _, Turn direction ) ->
                    let
                        snake =
                            model.snake
                    in
                    { model | snake = Snake.turn direction snake }
                        |> step Grid.walls

                ( _, _ ) ->
                    model |> step Grid.walls

        NewFood food ->
            ( { model | food = food }, Cmd.none )

        Types.NewGame ->
            ( initialModel, Cmd.none )


step : Walls -> Model -> ( Model, Cmd Types.Msg )
step walls model =
    let
        newSnake =
            Snake.stepSnake model.food walls model.snake
    in
    ( { model | snake = newSnake }
    , case newSnake.state of
        Eating ->
            Cmd.batch
                [ Food.randomFoodCmd
                , App.playSound "Sounds/success.wav"
                ]

        HitSelf ->
            App.playSound "Sounds/failure.wav"

        HitWall ->
            App.playSound "Sounds/failure.wav"

        _ ->
            Cmd.none
    )
