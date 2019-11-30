module TryThrottle exposing (..)

import Browser.Events
import Browser.Navigation exposing (Key)
import Debug
import GraphicSVG exposing (..)
import GraphicSVG.App
import Time exposing (Posix)
import Url exposing (Url)
import Lib.App as WkApp exposing (GetKeyState, TickRate(..), gameAppThrottle)



{--}
-- main =
--     -- gameApp without throttle, with keys etc
--     WkApp.gameApp
--         Tick
--         { view = boxView
--         , model = initialBox
--         , update = boxUpdate
--         , title = ""
--         }


{--}
-- main =
--     --  gameApp with throttle - provides keys, adds EXTRA layer wrapped around Model (e.g. in Debugger)
--     WkApp.gameAppThrottle
--         (Every 1000)
--         -- or Max
--         Tick
--         { view = boxView
--         , model = initialBox
--         , update = boxUpdate
--         , title = ""
--         }


{--}
main =
    --  appWithTick without throttle, with keys etc
    let
        init : () -> Url -> Key -> ( BoxModel, Cmd msg )
        init =
            \flags url key -> ( initialBox, Cmd.none )
    in
    WkApp.appWithTick
        -- (\secs keys -> Tick (Time.millisToPosix 0))
        Tick
        { init = init
        , view = \model -> { title = "duh", body = boxView model }
        , update = \msg mod -> ( boxUpdate msg mod, Cmd.none )
        , subscriptions = \mod -> Sub.none
        , onUrlRequest = \u -> NoOp
        , onUrlChange = \u -> NoOp
        }


type BoxModel
    = Box { size : Float, pos : ( Float, Float ) }


type BoxMsg
    = Inc
    | Tick Float GetKeyState
    | NoOp


initialBox : BoxModel
initialBox =
    Box { size = 10, pos = ( 0, 0 ) }


boxUpdate : BoxMsg -> BoxModel -> BoxModel
boxUpdate msg ((Box { size, pos }) as model) =
    let
        ( x, y ) =
            pos
    in
    case msg of
        Inc ->
            Box { size = size + 10, pos = ( x, y ) }

        Tick posix keyState ->
            Box { size = size, pos = ( x + 0.2, y ) }

        NoOp ->
            model


boxView : BoxModel -> Collage BoxMsg
boxView (Box { size, pos }) =
    let
        ( x, y ) =
            pos
    in
    Collage 500
        500
        [ square size |> filled red |> move ( 50 * sin (x / pi), 0 )
        , text "Tap to Enlarge" |> filled blue |> move ( 0, 100 ) |> notifyTap Inc
        ]
