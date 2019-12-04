module BugSoln exposing (..)

import GraphicSVG exposing (..)
import Lib.WkApp as App exposing (KeyState(..), Keys(..))


type alias Model =
    { x : Float, y : Float, direction : Direction }


type Direction
    = Left
    | Right


initialModel =
    { x = 0, y = -4, direction = Right }


view model =
    let
        tapSurface =
            rect 10 9
                |> filled (rgba 0 0 0 0)
                |> addOutline (dashed 0.1) blue
                |> move ( 0, -2 )
                |> notifyTapAt TapAt

        body =
            roundedRect 1 0.5 0.2
                |> filled green
                |> move ( model.x, model.y )

        eye =
            circle 0.05
                |> filled black
                |> move
                    ( model.x
                        + (case model.direction of
                            Right ->
                                0.35

                            Left ->
                                -0.35
                          )
                    , model.y + 0.1
                    )

        resetButton =
            circle 0.5 |> filled purple |> notifyTap Reset |> move ( 0, 4 )
    in
    collage 10
        10
        [ graphPaperCustom 1 0.05 lightGrey
        , body
        , eye
        , resetButton
        , tapSurface
        ]


type Msg
    = Tick Float App.GetKeyState
    | TapAt ( Float, Float )
    | Reset


type UserRequest
    = Go Direction
    | Jump
    | None


decodeKeys : (Keys -> KeyState) -> UserRequest
decodeKeys keyF =
    if keyF Space == JustDown then
        Jump

    else if keyF LeftArrow == JustDown then
        Go Left

    else if keyF RightArrow == JustDown then
        Go Right

    else
        None


step model =
    case model.direction of
        Left ->
            { model | x = model.x - 0.2 }

        Right ->
            { model | x = model.x + 0.2 }


jump model =
    { model | y = model.y + 0.2 }


go dir model =
    { model | direction = dir }


update msg model =
    let
        { x, y, direction } =
            model
    in
    case msg of
        Tick seconds ( keyFunction, _, _ ) ->
            case ( decodeKeys keyFunction, direction ) of
                ( Jump, _ ) ->
                    jump model |> step

                ( Go Left, Right ) ->
                    go Left model

                ( Go Right, Left ) ->
                    go Right model

                ( _, _ ) ->
                    model |> step

        Reset ->
            initialModel

        TapAt ( tapX, tapY ) ->
            case ( model.direction, tapX <= model.x ) of
                ( Right, True ) ->
                    { model | direction = Left }

                ( Left, False ) ->
                    { model | direction = Right }

                _ ->
                    model


main =
    App.simpleGameApp
        (App.Every 300)
        Tick
        { title = "Game!"
        , view = view
        , update = update
        , init = initialModel
        }
