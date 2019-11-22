module Snake exposing (renderSnake, stepSnake, updateSnakeDirection)

-- import Html exposing (Html)
-- import Svg exposing (circle, rect)
-- import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, transform, width, x, y)

import Debug
import GraphicSVG exposing (..)
import Grid exposing (grid)
import Keyboard exposing (Key(..))
import Lib.App as App
import Types exposing (Direction(..), Food, Position, Snake, SnakeState(..))


renderSnake : Snake -> List (Shape msg)
renderSnake snake =
    List.map renderSnakePart snake.body ++ renderSnakeHead snake


renderSnakeHead : Snake -> List (Shape msg)
renderSnakeHead snake =
    let
        ( i, j ) =
            snake.head

        ( headX0, headY0 ) =
            ( grid.cellSize * toFloat i, grid.cellSize * toFloat j )

        eyeRadius =
            grid.cellSize * 0.15

        rotation =
            degrees
                (case snake.direction of
                    Right ->
                        0

                    Up ->
                        90

                    Left ->
                        180

                    Down ->
                        270
                )

        head =
            rect grid.cellSize grid.cellSize
                |> filled
                    (case snake.state of
                        Normal ->
                            brown

                        Eating ->
                            pink

                        HitSelf ->
                            red

                        HitWall ->
                            purple
                    )
                |> move ( headX0, headY0 )

        eyeLeft =
            circle eyeRadius |> filled blue |> move ( grid.cellSize * 0.3, grid.cellSize * 0.3 ) |> rotate rotation |> move ( headX0, headY0 )

        eyeRight =
            circle eyeRadius |> filled blue |> move ( grid.cellSize * 0.3, grid.cellSize * -0.3 ) |> rotate rotation |> move ( headX0, headY0 )
    in
    [ head, eyeLeft, eyeRight ]


renderSnakePart : Position -> Shape msg
renderSnakePart ( posX, posY ) =
    rect grid.cellSize grid.cellSize |> filled black |> move ( grid.cellSize * toFloat posX, grid.cellSize * toFloat posY )


invalidTransitions : List ( Direction, Direction )
invalidTransitions =
    [ ( Left, Right ), ( Right, Left ), ( Up, Down ), ( Down, Up ) ]


updateSnakeDirection : Snake -> Maybe Direction -> Snake
updateSnakeDirection snake direction =
    direction
        |> Maybe.map (validateNewDirection snake)
        |> Maybe.withDefault snake


validateNewDirection : Snake -> Direction -> Snake
validateNewDirection snake direction =
    if List.member ( snake.direction, direction ) invalidTransitions then
        snake

    else
        { snake | direction = direction }


stepSnake : Snake -> Food -> Snake
stepSnake snake food =
    let
        nextHead =
            let
                ( i, j ) =
                    snake.head
            in
            case snake.direction of
                Up ->
                    ( i, j + 1 )

                Right ->
                    ( i + 1, j )

                Down ->
                    ( i, j - 1 )

                Left ->
                    ( i - 1, j )

        gotFood =
            nextHead == food

        nextBody =
            snake.head
                :: (case gotFood of
                        True ->
                            snake.body

                        False ->
                            removeLast snake.body
                   )

        hitSelf =
            List.member nextHead nextBody

        hitWall =
            let
                ( i, j ) =
                    nextHead

                walls =
                    Grid.walls
            in
            i < walls.left || i > walls.right || j < walls.bottom || j > walls.top

        nextState =
            if hitSelf then
                HitSelf

            else if hitWall then
                HitWall

            else if gotFood then
                Eating

            else
                Normal
    in
    { snake | head = nextHead, body = nextBody, state = nextState }


removeLast : List a -> List a
removeLast list =
    List.take (List.length list - 1) list


main =
    let
        initialSnake =
            { head = ( 4, 2 )
            , body =
                [ ( 4, 3 )
                , ( 4, 4 )
                ]
            , direction = Right
            , state = Normal
            }
    in
    App.graphicsApp
        { view = Grid.viewport (Grid.view ++ renderSnake initialSnake) }
