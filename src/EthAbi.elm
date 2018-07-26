module EthAbi exposing (..)

import Char
import Hex
import List.Extra
import Result.Extra
import BigInt exposing (BigInt)


{-| TODO using BigInts where required
TODO testing!

Examples:

format_output = String.toList >> List.Extra.groupsOf 64 >> (List.map (String.fromList))

example1 = EthAbi.encode_args [Static (AbiUint 69), Static (AbiBool True)] |> format_output

example2 = EthAbi.encode_args [Static (AbiStaticBytes 3 "abc"), Static (AbiStaticBytes 3 "def")] |> format_output

example3 = EthAbi.encode_args [Dynamic (AbiBytes "dave"), Static (AbiBool True), Dynamic (AbiDynamicArray [Static (AbiUint 1), Static (AbiUint 2), Static (AbiUint 3)])] |> format_output

dynamic_example = EthAbi.encode_args [Static (AbiUint 291), Dynamic (AbiDynamicArray [Static (AbiUint 1110), Static (AbiUint 1929)]), Static (AbiStaticBytes 10 "1234567890"), Dynamic (AbiBytes "Hello, world!")] |> format_output

dynamic_example2 = EthAbi.encode_args [Dynamic (AbiDynamicArray [Dynamic (AbiDynamicArray [Static (AbiUint 1), Static (AbiUint 2)]), Dynamic (AbiDynamicArray [Static (AbiUint 3)])]), Dynamic (AbiDynamicArray [Dynamic (AbiString "one"), Dynamic (AbiString "two"), Dynamic (AbiString "three")])] |> format_output

-}
type AbiType
    = Static AbiStaticType
    | Dynamic AbiDynamicType


type
    AbiStaticType
    -- An unsigned (positive) integer:
    = AbiUint BigInt
      -- A signed integer:
    | AbiInt BigInt
      -- A boolean value:
    | AbiBool Bool
      --| A bytes array of known length; i.e bytes3, bytes10, etc:
    | AbiStaticBytes Int String
      --| An array of static length; all elements of the same type:
    | AbiStaticArray Int (List AbiStaticType)
      --| A tuple where none of the elements is a dynamic type:
    | AbiStaticTuple Int (List AbiStaticType)


type
    AbiDynamicType
    -- A bytes array of dynamic length:
    = AbiBytes String
      -- A string of dynamic length:
    | AbiString String
      -- An array with dynamic length; all elements of the same type:
    | AbiDynamicArray (List AbiType)
      -- An array with static length; all elements of the same type:
    | AbiArray Int (List AbiDynamicType)
      -- An tuple where one of the elements has to be Dynamic:
    | AbiDynamicTuple Int (List AbiType)

type AbiSpec
    = AbiSUint
    | AbiSInt
    | AbiSBool
    | AbiSStaticBytes Int
    | AbiSStaticArray Int AbiSpec
    | AbiSStaticTuple Int AbiSpec
    | AbiSBytes
    | AbiSString
    | AbiSDynamicArray AbiSpec
    | AbiSArray Int AbiSpec
    | AbiSDynamicTuple Int

-- TODO introduce Int256 and Uint256 types, that are convertable to and from normal Ints and BigInts, with the resp. calls returning Results in the case this is not possible because they are too big or negative.
uint : BigInt -> Result String AbiStaticType
uint int =
    if BigInt.lt int (BigInt.fromInt 0) then
        Err "Negative integer cannot be converted to Unsigned Integer"
    else
        Ok (AbiUint int)

static_array len vals =
    -- ensure not empty
    -- ensure length same as given length
    -- ensure all elements the same as first element
    case vals of
        [] -> Err "Empty list passed to static_array"
        (hd :: rest) ->
            let
                all_same_type =
                    rest
                        |> List.map caseNameStatic
                        |> List.all (\x -> x == (caseNameStatic hd))
            in
                if not all_same_type then
                    Err "All values passed to static_array should be of the same type"
                else
                    if not (len == List.length vals) then
                        Err "Given length is different than the amount of passed values"
                    else
                    Ok (AbiStaticArray len vals)


caseNameStatic : AbiStaticType -> String
caseNameStatic val =
    case val of
        AbiUint _ -> "AbiUint"
        AbiInt _ -> "AbiInt"
        AbiBool _ -> "AbiBool"
        AbiStaticBytes len _ -> "AbiStaticBytes " ++ (toString len)
        AbiStaticArray len _ -> "AbiStaticArray " ++ (toString len)
        AbiStaticTuple len _ -> "AbiStaticTuple " ++ (toString len) -- TODO?

caseNameDynamic : AbiDynamicType -> String
caseNameDynamic val =
    case val of
        AbiBytes _ -> "AbiBytes"
        AbiString _ -> "AbiString"
        AbiDynamicArray _ -> "AbiDynamicArray"
        AbiArray len _ -> "AbiArray " ++ (toString len)
        AbiDynamicTuple len _ -> "AbiDynamicTuple " ++ (toString len) -- TODO?

caseName : AbiType -> String
caseName val =
    case val of
        Static s -> "Static " ++ caseNameStatic s
        Dynamic d -> "Dynamic " ++ caseNameDynamic d

{-
   32 bytes == 64 hexadecimal characters
   This means that when counting hexchars, we get twice the number as when counting bytes.
-}


encode_args : List AbiType -> String
encode_args args =
    let
        concat_tuple ( a, b ) =
            String.concat [ a, b ]

        n_args =
            List.length args

        head_length =
            n_args * 32

        encode_elem elem ( prev_head, prev_tail ) =
            Debug.log "encode_elem"
                (case elem of
                    Static val ->
                        ( prev_head ++ (static_encode val), prev_tail )

                    Dynamic val ->
                        let
                            dynamic_address =
                                head_length + (bytesInHexString prev_tail)
                        in
                            ( prev_head ++ (static_encode (AbiUint (BigInt.fromInt dynamic_address))), prev_tail ++ dynamic_tail val )
                )
    in
        args
            |> List.foldl encode_elem ( "", "" )
            |> concat_tuple


bytesInHexString : String -> Int
bytesInHexString str =
    String.length str // 2


dynamic_tail : AbiDynamicType -> String
dynamic_tail val =
    case val of
        AbiDynamicArray vals ->
            let
                len =
                    List.length vals
            in
                (static_encode (AbiUint (BigInt.fromInt len))) ++ encode_args (vals)

        AbiArray n_elems vals ->
            vals
                |> List.map (Dynamic)
                |> encode_args

        AbiBytes bytes ->
            let
                len =
                    String.length bytes

                lhs =
                    static_encode (AbiUint (BigInt.fromInt len))

                rhs =
                    bytes
                        |> strToBytes
                        |> String.padRight 64 '0'
            in
                lhs ++ rhs

        AbiString str ->
            str
                |> AbiBytes
                |> dynamic_tail

        AbiDynamicTuple _ vals ->
            vals
                |> encode_args


padLeftTo32Bytes : Char -> String -> String
padLeftTo32Bytes char str =
    String.padLeft 64 char str


padRightTo32Bytes : Char -> String -> String
padRightTo32Bytes char str =
    String.padRight 64 char str


strToBytes : String -> String
strToBytes str =
    str
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""

bytesToStr : String -> Result String String
bytesToStr bytes =
    let
        -- two hexchars -> 0..255 -> char
        byteToChar = (Hex.fromString >> Result.map (Char.fromCode))
    in
    bytes
        |> stringGroupsOf 2
        |> List.map byteToChar
        |> Result.Extra.combine
        |> Result.map String.fromList

static_encode : AbiStaticType -> String
static_encode val =
    case val of
        AbiUint int ->
            int
                |> BigInt.toHexString
                |> padLeftTo32Bytes '0'

        AbiInt int ->
            if
                BigInt.gte int (BigInt.fromInt 0) then static_encode (AbiUint int)
            else
                let
                    twosComplementPow =
                        BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 256)
                    negToUint bigint = BigInt.add twosComplementPow bigint
                in
                static_encode (AbiUint (negToUint int))
            -- let
            --     hexint =
            --         Hex.toString int
            -- in
            --     if String.startsWith "-" hexint then
            --         hexint
            --             |> String.dropLeft 1
            --             |> padLeftTo32Bytes 'f'
            --     else
            --         hexint
            --             |> padLeftTo32Bytes '0'

        AbiBool bool ->
            if bool then
                static_encode (AbiUint (BigInt.fromInt 1))
            else
                static_encode (AbiUint (BigInt.fromInt 0))

        AbiStaticBytes n_elems bytes ->
            bytes
                |> strToBytes
                |> padRightTo32Bytes '0'
        AbiStaticArray len vals ->
            static_encode (AbiStaticTuple len vals)

        AbiStaticTuple _ vals ->
            vals
                |> List.map static_encode
                |> String.concat


-- decode_args : List AbiSpec -> String -> Result String (List AbiType)
-- decode_args spec val =
--     let
--         words = val |> stringGroupsOf 64
--         spec_length = List.length spec
--         -- TODO: Following line is wrong. Static array and tuples take multiple words from the head at once.
--         -- args_head = List.Extra.zip spec words
--         args_head = group_spec_with_proper_head_words words spec []
--         args_tail = List.drop spec_length words
--     in
--         args_head
--             |> List.map (\(spec, head) -> decode_arg spec head words)
--             |> Result.Extra.combine

-- TODO test implementation
group_spec_with_proper_head_words : List String -> List AbiSpec -> List (AbiSpec, List String) -> List (AbiSpec, List String)

group_spec_with_proper_head_words words specs acc =
    case specs of
        [] -> List.reverse acc
        (spec :: specs) ->
            let
                (spec_words, other_words) = words |> List.Extra.splitAt (num_head_elems spec)
            in
                group_spec_with_proper_head_words other_words specs ((spec, spec_words) :: acc)


num_head_elems : AbiSpec -> Int
num_head_elems spec =
    case spec of
        -- simple static types:
        AbiSUint -> 1
        AbiSInt -> 1
        AbiSBool -> 1
        AbiSStaticBytes _ -> 1
        -- complex static types:
        AbiSStaticArray len _ -> len
        AbiSStaticTuple len _ -> len
        -- dynamic types:
        AbiSBytes -> 1
        AbiSString -> 1
        AbiSDynamicArray _ -> 1
        AbiSArray _ _ -> 1 -- is this correct?
        AbiSDynamicTuple _ -> 1

-- decode_arg : AbiSpec -> List String -> List String -> Result String AbiType
-- decode_arg spec val words =
--     let
--         intToBool num = case num of
--                          0 -> Ok False
--                          1 -> Ok True
--                          _ -> Err "Impossible to convert ABI-encoded value to boolean, not '0' or '1'"
--     in
--         case spec of
--             AbiSUint ->
--                 case val of
--                     [val] ->
--                         val
--                             |> Hex.fromString
--                             |> Result.map (Static << AbiUint)
--                     _ -> Err "AbiSUint argument larger than expected"
--             AbiSInt ->
--               -- TODO two's complement!
--                 case val of
--                     [val] ->
--                         val
--                             |> Hex.fromString
--                             |> Result.map (Static << AbiInt)
--                     _ -> Err "AbiSInt argument larger than expected"
--             AbiSBool ->
--                 case val of
--                     [val] ->
--                         val
--                             |> Hex.fromString
--                             |> Result.andThen intToBool
--                             |> Result.map (Static << AbiBool)
--                     _ -> Err "AbiSBool argument larger than expected"
--             AbiSStaticBytes len ->
--                 case val of
--                     [val] ->
--                         val
--                             |> trimBytesRight len
--                             |> bytesToStr
--                             |> Result.map(Static << AbiStaticBytes len)
--                     _ -> Err "AbiSStaticBytes argument larger than expected"
--             _ -> Err "Not supported yet"

trimBytesLeft len str = String.dropLeft (64 - (2 * len)) str
trimBytesRight len str = String.dropRight (64 - (2 * len)) str

stringGroupsOf : Int -> String -> List String
stringGroupsOf num str =
    str
        |> String.toList
        |> List.Extra.groupsOf num
        |> List.map String.fromList
