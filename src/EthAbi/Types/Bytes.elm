module EthAbi.Types.Bytes
    exposing
        ( Bytes
        , bytes
        , fromString
        )

import Char
import Hex
import EthAbi.Internal exposing (Bytes32(..), Bytes(..), ensure)


type alias Bytes =
    EthAbi.Internal.Bytes


{-| Creates a new `Bytes` object from an existing hexadecimal string that is interpretable as a byte-string:

  - Only allowed to contain hexadecimal characters
  - String should have an even length, since otherwise it cannot possibly store bytes.

-}
bytes : String -> Result String Bytes
bytes bstr =
    let
        isHexadecimal str =
            String.all Char.isHexDigit str

        isRepresentableAsBytes str =
            String.length bstr % 2 == 0
    in
        bstr
            |> ensure isHexadecimal "Not a hexadecimal string, cannot interpret as already-encoded bytes"
            |> Result.andThen (ensure isRepresentableAsBytes "Hexadecimal string has an odd number of characters, so it cannot possibly store bytes")
            |> Result.map Bytes


{-| Encodes an arbitrary string into a `Bytes` string,

by interpreting every char as number between [0..255] and writing that as
two hexdigits [00..ff].

 This can never fail.

-}
fromString : String -> Bytes
fromString str =
    str
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""
        |> Bytes


toString : Bytes -> String
toString (Bytes str) =
    str
