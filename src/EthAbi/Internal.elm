module EthAbi.Internal exposing (..)
{-| Internal module; exposing everything to this library, but is not in the public packages, so outside of the EthAbi library its contents are not visible.
-}

import BigInt exposing (BigInt)


{-| Ensures that `condition value`, is `True`.
Returns a Result depending on the outcome.
 -}
ensure : (a -> Bool) -> e -> a -> Result e a
ensure condition error value =
    if (condition value) then
        Ok value
    else
        Err error

type Hexstring
    = Hexstring String

type Bytes32
    = Bytes32 String


type Bytes = Bytes String



integerFits : Int -> BigInt -> Bool
integerFits len integer =
    let
        len_bits len =
            BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt len)
    in
        BigInt.lte (BigInt.abs integer) (len_bits len)
