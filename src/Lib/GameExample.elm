module Lib.GameExample exposing (..)

import GraphicSVG exposing (..)
import Lib.WkApp as App


main =
    --  game: tick, throttle, minimal model/view/update
    App.game
        (App.Every 1000)
        Tick
        { init = initialModel
        , view = view
        , update = update
        , title = "Pausable + Up/Down"
        }


type alias Model =
    { state : State, size : Float, pos : ( Float, Float ) }


type State
    = Running
    | Paused


type Msg
    = Inc
    | Pause
    | Go
    | Restart
    | Tick Float App.GetKeyState
    | NoOp


initialModel : Model
initialModel =
    { state = Running, size = 10, pos = ( 0, 0 ) }


update : Msg -> Model -> Model
update msg ({ state, size, pos } as model) =
    let
        ( x, y ) =
            pos
    in
    case ( state, msg ) of
        ( _, Pause ) ->
            { model | state = Paused }

        ( Running, Inc ) ->
            { model | size = size + 10 }

        ( Running, Tick secs ( keyFunc, ( xArrow1, yArrow1 ), ( xArrow2, yArrow2 ) ) ) ->
            -- let
            --     _ =
            --         Debug.log "keys" ( keyFunc, ( xArrow1, yArrow1 ), ( xArrow2, yArrow2 ) )
            -- in
            { model | pos = ( x + 0.2, Debug.log "y" (y + yArrow1 + yArrow2) ) }

        ( Paused, Go ) ->
            { model | state = Running }

        ( _, Restart ) ->
            initialModel

        ( Paused, _ ) ->
            model

        ( _, NoOp ) ->
            model

        ( Running, Go ) ->
            model


view : Model -> Collage Msg
view { state, size, pos } =
    let
        ( x, y ) =
            pos
    in
    Collage 500
        500
        [ square size |> filled red |> move ( 50 * sin (x / pi), y )
        , text "+" |> filled blue |> move ( 0, 100 ) |> notifyTap Inc
        , case state of
            Running ->
                text "P" |> filled red |> move ( 20, 100 ) |> notifyTap Pause

            Paused ->
                text "G" |> filled green |> move ( 20, 100 ) |> notifyTap Go
        , text "R" |> filled blue |> move ( 40, 100 ) |> notifyTap Restart
        ]
