module BugSoln exposing (..)

import GraphicSVG exposing (..)
import Lib.WkApp as App exposing (KeyState(..), Keys(..))



------- MODEL -------


type alias Model =
    { x : Float, y : Float, direction : Direction }


type Direction
    = Left
    | Right


initialModel =
    { x = 0, y = -4, direction = Right }



------- VIEW -------


viewBug ( x, y ) direction =
    let
        body =
            roundedRect 1 0.5 0.2
                |> filled green
                |> move ( x, y )

        eye =
            circle 0.05
                |> filled black
                |> move
                    ( x
                        + (case direction of
                            Right ->
                                0.35

                            Left ->
                                -0.35
                          )
                    , y + 0.1
                    )
    in
    group [ body, eye ]


view model =
    let
        tapSurface =
            rect 10 8
                |> filled (rgba 0 0 0 0)
                |> addOutline (dashed 0.05) lightBlue
                |> move ( 0, -1 )
                |> notifyTapAt BoardTapAt

        resetButton =
            circle 0.5 |> filled purple |> notifyTap ResetBtnTap |> move ( 0, 4 )

        jumpButton =
            circle 0.5 |> filled orange |> notifyTap JumpBtnTap |> move ( 2, 4 )

        bug =
            viewBug ( model.x, model.y ) model.direction
    in
    collage 10
        10
        [ graphPaperCustom 1 0.05 lightGrey
        , bug
        , resetButton
        , jumpButton
        , tapSurface
        ]



------- INTERACTION -------


type UserRequest
    = Jump
    | Go Direction
    | Reset
    | None


type Msg
    = Tick Float App.GetKeyState
    | BoardTapAt ( Float, Float )
    | ResetBtnTap
    | JumpBtnTap



------- DECODE KEYS, MOUSE -> UserRequest ------


decodeKeys : (Keys -> KeyState) -> UserRequest
decodeKeys keyF =
    if keyF Space == JustDown || keyF Space == Down then
        Jump

    else
        None


decodeTap : ( Float, Float ) -> Model -> UserRequest
decodeTap ( tapX, tapY ) model =
    case ( model.direction, tapX <= model.x ) of
        ( Right, True ) ->
            Go Left

        ( Left, False ) ->
            Go Right

        _ ->
            None



------- ACTIONS ON MODEL -------


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


reset model =
    initialModel



------- UPDATE -------


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

                ( _, _ ) ->
                    model |> step

        ResetBtnTap ->
            reset model

        JumpBtnTap ->
            jump model |> step

        BoardTapAt ( tapX, tapY ) ->
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
