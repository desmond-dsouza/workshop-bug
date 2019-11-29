module Snake exposing (..)

import Debug
import GraphicSVG exposing (..)
import Grid exposing (fracToGrid, grid, toGrid)
import Keyboard exposing (Key(..))
import Lib.App as App
import Types exposing (Body, Direction(..), Food, Head, Position, Segment, Snake, SnakeState(..), Walls)


view : Snake -> List (Shape msg)
view snake =
    List.map viewSnakeSegment snake.body ++ viewSnakeHead snake


viewSnakeHead : Snake -> List (Shape msg)
viewSnakeHead snake =
    let
        ( i, j ) =
            snake.head

        ( headX0, headY0 ) =
            ( i |> toGrid, j |> toGrid )

        head =
            roundedRect (1 |> toGrid) (1 |> toGrid) (0.25 |> fracToGrid)
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

        eyeRadius =
            0.15 |> fracToGrid

        eyeRotation =
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

        eyeLeft =
            circle eyeRadius
                |> filled black
                |> move ( 0.25 |> fracToGrid, 0.25 |> fracToGrid )
                |> rotate eyeRotation
                |> move ( headX0, headY0 )

        eyeRight =
            circle eyeRadius
                |> filled black
                |> move ( 0.25 |> fracToGrid, -0.25 |> fracToGrid )
                |> rotate eyeRotation
                |> move ( headX0, headY0 )
    in
    [ head, eyeLeft, eyeRight ]


viewSnakeSegment : Segment -> Shape msg
viewSnakeSegment ( posX, posY ) =
    circle (0.5 |> fracToGrid) |> filled black |> move ( posX |> toGrid, posY |> toGrid )


invalidTransitions : List ( Direction, Direction )
invalidTransitions =
    [ ( Left, Right ), ( Right, Left ), ( Up, Down ), ( Down, Up ) ]


nextDirection : Direction -> Maybe Key -> Direction
nextDirection oldDir key =
    let
        newDir =
            case key of
                Just ArrowRight ->
                    Right

                Just ArrowLeft ->
                    Left

                Just ArrowUp ->
                    Up

                Just ArrowDown ->
                    Down

                _ ->
                    oldDir
    in
    if List.member ( oldDir, newDir ) invalidTransitions then
        oldDir

    else
        newDir


changeDirection : Maybe Key -> Snake -> Snake
changeDirection key snake =
    { snake | direction = nextDirection snake.direction key }


stepHead : Head -> Direction -> Head
stepHead ( i, j ) direction =
    case direction of
        Up ->
            ( i, j + 1 )

        Right ->
            ( i + 1, j )

        Down ->
            ( i, j - 1 )

        Left ->
            ( i - 1, j )


stepBody : Head -> Bool -> Body -> Body
stepBody currHead gotFoodNext currBody =
    currHead
        :: (case gotFoodNext of
                True ->
                    currBody

                False ->
                    removeLast currBody
           )


gotFood : Head -> Food -> Bool
gotFood h f =
    h == f


hitSelf : Head -> Body -> Bool
hitSelf head body =
    List.member head body


hitWall : Head -> Walls -> Bool
hitWall ( i, j ) walls =
    i < walls.left || i > walls.right || j < walls.bottom || j > walls.top


stepSnake : Food -> Walls -> Snake -> Snake
stepSnake food walls snake =
    let
        nextHead =
            stepHead snake.head snake.direction

        nextGotFood =
            gotFood nextHead food

        nextBody =
            stepBody snake.head nextGotFood snake.body

        nextHitWall =
            hitWall nextHead walls

        nextState =
            if hitSelf nextHead nextBody then
                HitSelf

            else if hitWall nextHead walls then
                HitWall

            else if nextGotFood then
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
        { view = Grid.viewport (Grid.view ++ view initialSnake) }
