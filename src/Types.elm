module Types exposing (..)

import Keyboard exposing (Key(..))
import Time


type alias Food =
    Position


type alias Model =
    { snake : Snake
    , food : Food
    }


type alias Position =
    ( Int, Int )


type alias Grid =
    { numRows : Int, numColumns : Int, cellSize : Float }


type Msg
    = Tick Time.Posix
    | KeyDown Keyboard.RawKey
    | NewFood Position
    | NoOp


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Snake =
    { head : Position
    , body : List Position
    , direction : Direction
    , state : SnakeState
    }


type SnakeState
    = Normal
    | Eating
    | HitSelf
    | HitWall
