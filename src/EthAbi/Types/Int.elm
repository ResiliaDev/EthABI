module EthAbi.Types.Int
    exposing
        ( Int256
        , static_int
        , int8
        , int16
        , int32
        , int64
        , int128
        , int256
        , toBigInt
        )

import BigInt exposing (BigInt)
import EthAbi.Internal exposing (ensure)


type Int256
    = Int256 BigInt


static_int : Int -> BigInt -> Result String Int256
static_int len integer =
    let
        appropriateLength len _ =
            len > 0 && len <= 256 && len % 8 == 0
    in
        integer
            |> ensure (appropriateLength len) ("length should be in range 0..256 and be a multiple of 8, but it is " ++ (toString len))
            |> Result.andThen (ensure (EthAbi.Internal.integerFits (len - 1)) ("Integer too large to fit in " ++ toString len ++ " bits"))
            |> Result.map (Int256)



-- TODO: Constructor shorthand for every `int<M>`?
int8 =
    static_int 8


int16 =
    static_int 16


int32 =
    static_int 32


int64 =
    static_int 64


int128 =
    static_int 128


int256 =
    static_int 256


toBigInt (Int256 big_int) =
    big_int
