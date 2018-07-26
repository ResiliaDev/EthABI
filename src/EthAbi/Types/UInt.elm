module EthAbi.Types.UInt
    exposing
        ( UInt256
        , static_uint
        , uint8
        , uint16
        , uint32
        , uint64
        , uint128
        , uint256
        , toBigInt
        )

import BigInt exposing (BigInt)
import EthAbi.Internal exposing (ensure)


type UInt256
    = UInt256 BigInt


static_uint : Int -> BigInt -> Result String UInt256
static_uint len integer =
    let
        appropriateLength len _ =
            len > 0 && len <= 256 && len % 8 == 0

        isPositive integer =
            BigInt.gte integer (BigInt.fromInt 0)

        ensurePositive integer =
            if BigInt.gte integer (BigInt.fromInt 0) then
                Ok integer
            else
                Err ("UInt256 called with a negative value: " ++ toString integer)
    in
        integer
            |> ensure (appropriateLength len) ("length should be in range 0..256 and be a multiple of 8, but it is " ++ (toString len))
            |> Result.andThen (ensure isPositive ("UInt256 called with a negative value: " ++ toString integer))
            |> Result.andThen (ensure (EthAbi.Internal.integerFits len) ("Unsigned Integer too large to fit in " ++ toString len ++ " bits"))
            |> Result.map (UInt256)



-- TODO: Constructor shorthand for every `uint<M>`?


uint8 =
    static_uint 8


uint16 =
    static_uint 16


uint32 =
    static_uint 32


uint64 =
    static_uint 64


uint128 =
    static_uint 128


uint256 =
    static_uint 256


toBigInt (UInt256 big_int) =
    big_int
