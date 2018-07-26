module EthAbi.Types.Hexstring exposing(hexstring, Hexstring, toString)

import Char

import EthAbi.Internal exposing (ensure, Hexstring(..))

type alias Hexstring =
    EthAbi.Internal.Hexstring


{-| Creates a hexadecimal string from some input string.

  - makes sure that it is actually hexadecimal
  - Ensures that it is actually a multiple of 32 bytes (64 hexadecimal characters), because all ABI requests and responses are this way.

Do note, that the extra four bytes (8 hexchars) that are prepended to a request to represent the function signature hash mean that those requests are *not* `EthAbi.Hexstring`'s!

TODO have a special data structure for function calls.

-}
hexstring : String -> Result String Hexstring
hexstring raw_str =
    let
        hexadecimal str =
            String.all Char.isHexDigit str

        multipleOf32Bytes str =
            String.length str % 64 == 0
    in
        raw_str
            |> ensure hexadecimal "Not a hexadecimal string"
            |> Result.andThen (ensure multipleOf32Bytes "String not a multiple of 32 bytes (64 hexadecimal characters)")
            |> Result.map Hexstring


toString : Hexstring -> String
toString (Hexstring hexstr) =
    hexstr
