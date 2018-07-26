module Tests.EthABI.Fuzz exposing (..)

{-| Contains fuzzers to test the rest of EthABI with.
-}

import Fuzz exposing (Fuzzer)
import EthABI.Types.Int
import EthABI.Types.UInt
import EthABI.Types.Bytes32
import EthABI.Types.Bytes
import EthABI.Types.Hexstring exposing (Hexstring)
import EthABI.Encode as Encode
import EthABI.Decode as Decode
import BigInt
import Random
import String


int256 : Fuzzer (Result String EthABI.Types.Int.Int256)
int256 =
    Fuzz.int |> Fuzz.map (BigInt.fromInt >> EthABI.Types.Int.int256)


uint256 : Fuzzer (Result String EthABI.Types.UInt.UInt256)
uint256 =
    Fuzz.intRange 0 Random.maxInt |> Fuzz.map (BigInt.fromInt >> EthABI.Types.UInt.uint256)


{-| TODO: Probably more useful if we can read the actual length we created. -}
static_bytes : Fuzzer (Result String EthABI.Types.Bytes32.Bytes32)
static_bytes =
    let
        bytes32Fuzzer str len =
            str |> String.left len >> EthABI.Types.Bytes32.fromString len
    in
        Fuzz.map2 bytes32Fuzzer (Fuzz.string) (Fuzz.intRange 0 32)


bytes32 : Fuzzer (Result String EthABI.Types.Bytes32.Bytes32)
bytes32 =
    let
        bytes32Fuzzer str len =
            str |> String.left len >> EthABI.Types.Bytes32.fromString len
    in
        Fuzz.string |> Fuzz.map (String.left 32 >> EthABI.Types.Bytes32.fromString 32)


bytes : Fuzzer EthABI.Types.Bytes.Bytes
bytes =
    Fuzz.string |> Fuzz.map (EthABI.Types.Bytes.fromString)


encoder_decoder_triple : Fuzzer (String, Result String Hexstring, String)
encoder_decoder_triple =
    let
        triple : Fuzzer (Result String a) -> (a -> Encode.Encoder) -> Decode.Decoder a -> Fuzzer (String, Result String Hexstring, String)
        triple fuzzer encoder decoder =
            let
                encoded val = val |> Result.map (encoder >> Encode.encode)
                decoded val = encoded val |> (Result.andThen (Decode.decodeHexstring decoder)) |> toString
            in
                fuzzer |> Fuzz.map (\val -> ( toString val, encoded val, decoded val))
    in
        Fuzz.oneOf
            [ triple int256 Encode.int256 Decode.int256
            , triple uint256 Encode.uint256 Decode.uint256
            , triple (Fuzz.bool |> Fuzz.map Ok) Encode.bool Decode.bool
            , triple bytes32 Encode.bytes32 Decode.bytes32
            -- , triple (Fuzz.string |> Fuzz.map Ok) Decode.string
            ]
