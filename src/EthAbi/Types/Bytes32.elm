module EthAbi.Types.Bytes32
    exposing
        ( Bytes32
        , bytes
        , bytes1
        , bytes32
        , fromString
        )

import Char
import Hex
import EthAbi.Internal exposing (Bytes32(..), Bytes(..), ensure)


type alias Bytes32 =
    EthAbi.Internal.Bytes32



-- TODO maybe restrict shorter strings, so user should pad to bytes/string length exactly?


bytes : Int -> String -> Result String Bytes32
bytes len bstr =
    let
        isHexadecimal str =
            String.all Char.isHexDigit str

        ensureLenIsInRange len str =
            if len > 0 && len <= 32 then
                Ok str
            else
                Err ("Bytes length should be in range 0..32, but it is " ++ (Basics.toString len))

        ensureBytestringFits str =
            if String.length str // 2 <= len then
                Ok str
            else
                Err "String is too large to fit in a Bytes32."
    in
        bstr
            |> ensure isHexadecimal "Not a hexadecimal string, cannot interpret as already-encoded bytes"
            |> Result.andThen (ensureLenIsInRange len)
            |> Result.andThen ensureBytestringFits
            |> Result.map Bytes32


{-| Encodes a string into a `Bytes32` string,

by interpreting every char as number between [0..255] and writing that as
two hexdigits [00..ff]

-}
fromString : Int -> String -> Result String Bytes32
fromString len str =
    let
        ensureLenIsInRange len str =
            if len > 0 && len <= 32 then
                Ok str
            else
                Err ("Bytes length should be in range 0..32, but it is " ++ (Basics.toString len))

        ensureStringFits str =
            if String.length str <= len then
                Ok str
            else
                Err "String is too large to fit in a Bytes32."

        stringToHexstring string =
            string
                |> String.toList
                |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
                |> String.join ""
    in
        str
            |> stringToHexstring
            |> bytes len


-- TODO: Constructor shorthand for every `bytes<M>`?


bytes1 =
    bytes 1


bytes32 =
    bytes 32


toString : Bytes32 -> String
toString (Bytes32 str) =
    str
