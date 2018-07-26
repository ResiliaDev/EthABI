module EthAbi.Encode
    exposing
        ( encode
        , int256
        , uint256
        , bytes32
        , bool
        , append
        , tuple
        , array
        , dynamic_array
        , bytes
        , string
        -- , unsafeBigIntToHexStr
        -- , padRightToNearestMultipleOf32Bytes
          -- , partialEncode
          -- TODO
        )

import BigInt exposing (BigInt)
import Char
import Hex
import Result.Extra
import List.Extra
import EthAbi.Types.Hexstring
import EthAbi.Types.Bytes
import EthAbi.Types.Int exposing (Int256)
import EthAbi.Types.UInt exposing (UInt256)
-- import EthAbi.Types exposing (hexstring, Int256, UInt256, int256ToBigInt, uint256ToBigInt, bytes)
import EthAbi.Internal exposing (ensure, Hexstring, Bytes32(..), Bytes(..))


-- for internal use only

-- TODO better naming
type alias Hexstring =
    String


{-| An Encoder turns an 'a' into a pair of Hexstrings.
The final 'encode' function finally concatenates these two, but it is important for them to be kept separate because if multiple encoders run one-after-the-other, content needs to be added in-between the two.
-}
type alias Encoder =
    List EncodedValue


type EncodedValue
    = Normal Hexstring
    | DynamicReference Hexstring


encode : Encoder -> EthAbi.Types.Hexstring.Hexstring
encode encoder =
        encoder
            |> resolveDynamicReferences
            |> EthAbi.Internal.Hexstring

resolveDynamicReferences : List EncodedValue -> String
resolveDynamicReferences values =
    let
        head_length =
            bytesLength values
                |> Debug.log "head_length"

        concatTuple ( a, b ) =
            a ++ b
    in
        values
        |> List.foldl (resolveDynamicReferencesImpl head_length) ("", "")
        |> concatTuple

resolveDynamicReferencesImpl : BigInt -> EncodedValue -> (String, String) -> (String, String)
resolveDynamicReferencesImpl head_length elem (resolved_head, resolved_tail) =
    case elem of
        Normal str ->
            (resolved_head ++ str, resolved_tail)
        DynamicReference tail_str ->
            let
                reference_location =
                    BigInt.add head_length (BigInt.fromInt <| (String.length resolved_tail) // 2)
                        |> Debug.log "reference_location"
                _ = Debug.log "resolved_head" resolved_head
                _ = Debug.log "resolved_tail" resolved_tail
            in
                (resolved_head ++ (unsafeBigIntToHexStr reference_location), resolved_tail ++ tail_str)


-- resolveDynamicReferences ( head, tail ) =
--     let
--         -- Number of bytes in the head
--         head_length =
--             bytesLength head
--     in
--         Debug.crash "TODO"
-- resolveDynamicReference elem (acc_head, acc_tail) head_length tail =
--     case elem of
--         Normal hexstr ->
--             resolveDynamicReference elem (acc_head ++ hexstr, acc_tail) head_length tail
--         DynamicReference ->


bytesLength : List EncodedValue -> BigInt
bytesLength encoded_values =
    let
        elemLength elem =
            case elem of
                Normal str ->
                    BigInt.fromInt <| (String.length str) // 2

                DynamicReference tail ->
                    BigInt.fromInt 32

        bigIntSum =
            List.foldl BigInt.add (BigInt.fromInt 0)
    in
        encoded_values
            |> List.map elemLength
            |> bigIntSum
            |> Debug.log "bytesLength"



-- partialEncode : Encoder -> List EncodedValue -> List EncodedValue
-- partialEncode encoder encoded_values =
--     encoder encoded_values


{-| Runs two encoders one after the other.

Note that `enc_a |> append enc_b |> append enc_c` will give you
the result of appending enc_c and then enc_b and then enc_a.

Usually, though, don't use this function but instead use `tuple`, `dynamic_array` or `array`.

-}
append : Encoder -> Encoder -> Encoder
append enc_a enc_b =
    enc_a ++ enc_b



-- foo : (c -> (a, b)) -> Encoder a -> Encoder b -> Encoder c
-- foo fun encodera encoderb = contramap (,) (encodera >> encoderb)
-- contramap2 : (c -> (a, b)) -> Encoder a -> Encoder b -> Encoder c
-- contramap2 fun enc_a enc_b =
--     let
--         combine_results ((a_head, a_tail), (b_head, b_tail)) = (a_head ++ b_head, a_tail ++ b_tail)
--         in_order (a, b) hexstr_tuple = \val val2 ->
--                                        (a val hexstr_tuple)
--                                      |> (b val hexstr_tuple2)
-- in
--     \val hexstr ->
--         val
--             |> fun
--             |> Tuple2.mapFirst (\val -> enc_a val hexstr)
--             |> combine_results


uint256 : UInt256 -> Encoder
uint256 integer =
    integer
        |> EthAbi.Types.UInt.toBigInt
        |> unsafeBigInt


int256 : Int256 -> Encoder
int256 integer =
    let
        bigint =
            EthAbi.Types.Int.toBigInt integer

        twosComplementPow =
            BigInt.pow (BigInt.fromInt 2) (BigInt.fromInt 255)

        twos_complement =
            if BigInt.gte bigint (BigInt.fromInt 0) then
                bigint
            else
                BigInt.add (BigInt.mul twosComplementPow (BigInt.fromInt 2)) (bigint)
    in
        unsafeBigInt twos_complement


bool : Bool -> Encoder
bool boolean =
    if boolean then
        unsafeInt 1
    else
        unsafeInt 0


bytes32 : Bytes32 -> Encoder
bytes32 bytes =
    let
        head =
            bytes
                |> bytes32ToHex
                |> padRightTo32Bytes '0'
    in
        [ Normal head ]


tuple : List Encoder -> Encoder
tuple encoders =
    let
        tuple_body =
            tupleBody encoders
    in
        if isDynamicType tuple_body then
            [ DynamicReference (resolveDynamicReferences tuple_body) ]
        else
            tuple_body



-- \( encoded_values, hexstr_tail ) ->
-- case tupleBody encoders of
--     ( only_head, [] ) ->
--         -- Static contents, so static tuple
--         ( encoded_values ++ only_head, hexstr_tail )
--     ( head, tail ) ->
--         -- Dynamic contents, so dynamic tuple
--         ( encoded_values ++ [ DynamicReference "TODO, call resolveReferences here recursively"], hexstr_tail)


isDynamicElem : EncodedValue -> Bool
isDynamicElem elem =
    case elem of
        DynamicReference _ ->
            True

        _ ->
            False


isDynamicType =
    List.any isDynamicElem


{-| An array of as of a non-statically declared size.
TODO distinguish 'variable length' arrays from 'dynamic' datatypes.
Current naming might be confusing.
-}
dynamic_array : (a -> Encoder) -> List a -> Encoder
dynamic_array encoder_fun array =
    let
        dynamic_array_body =
            dynamicArrayBody encoder_fun array

        -- concatTuple ( a, b ) =
        --     a ++ b
    in
        -- \( encoded_values, hexstr_tail ) ->
        [ DynamicReference (dynamic_array_body)

        {- (concatTuple dynamic_array_body) -}
        ]


{-| An array with a static length.

It will be a static type if all contents are static,
and a dynamic type if it contains a dynamic type.

-}
array : Int -> (a -> Encoder) -> List a -> Encoder
array length encoder_fun array =
    let
        array_body =
            arrayBody encoder_fun array

        concatTuple ( a, b ) =
            a ++ b
    in
        if isDynamicType array_body then
            [ DynamicReference (resolveDynamicReferences array_body) ]
        else
            array_body

bytes : Bytes -> Encoder
bytes (Bytes bstr) =
    let
        bytes_length = String.length bstr
        padded_bytestring = padRightToNearestMultipleOf32Bytes '0' bstr
        encoded_length = unsafeIntToHexStr bytes_length
    in
            [DynamicReference (encoded_length ++ padded_bytestring)]

string : String -> Encoder
string str =
    str
        |> EthAbi.Types.Bytes.fromString
        |> bytes


-- Internal helper functions:


tupleBody : List Encoder -> Encoder
tupleBody encoders =
    List.concat encoders
    -- List.foldr append identity encoders


dynamicArrayBody : (a -> Encoder) -> List a -> String
dynamicArrayBody encoding_fun array =
    let
        encodedLength =
            array
                |> List.length
                |> unsafeIntToHexStr
    in
        encodedLength ++ resolveDynamicReferences (arrayBody encoding_fun array)


{-| Used by both static and dynamic arrays to encode the elements of the array
-}
arrayBody : (a -> Encoder) -> List a -> Encoder
arrayBody encoding_fun array =
    array
        |> List.map encoding_fun
        |> tupleBody


unsafeInt : Int -> Encoder
unsafeInt int =
    int
        |> BigInt.fromInt
        |> unsafeBigInt

unsafeIntToHexStr int =
    int
        |> BigInt.fromInt
        |> unsafeBigIntToHexStr

elementSize : Hexstring -> Int
elementSize hexstr =
    (String.length hexstr) // 64


{-| Unsafe!
Only call after making sure that BigInt is expressible in Int256 resp Uint256!
(This function exists for the overlap in functionality between int256 and uint256 and bool)
-}
unsafeBigInt : BigInt -> Encoder
unsafeBigInt bigint =
    let
        head =
            bigint
                |> BigInt.toHexString
                |> padLeftTo32Bytes '0'
    in
        [ Normal head ]


unsafeBigIntToHexStr bigint =
    bigint
        |> BigInt.toHexString
        |> padLeftTo32Bytes '0'


padLeftTo32Bytes : Char -> String -> String
padLeftTo32Bytes char str =
    String.padLeft 64 char str


padRightToNearestMultipleOf32Bytes : Char -> String -> String
padRightToNearestMultipleOf32Bytes char str =
    let
        string_length = String.length str
        pad_amount = string_length + (64 - (string_length % 64))
    in
        String.padRight pad_amount char str

padRightTo32Bytes : Char -> String -> String
padRightTo32Bytes char str =
    String.padRight 64 char str


bytes32ToHex : Bytes32 -> String
bytes32ToHex (Bytes32 str) =
    str
        |> String.toList
        |> List.map (Char.toCode >> Hex.toString >> String.padLeft 2 '0')
        |> String.join ""


bytes32ToString : String -> Result String String
bytes32ToString bytes =
    let
        -- two hexchars -> 0..255 -> char
        byteToChar =
            (Hex.fromString >> Result.map (Char.fromCode))
    in
        bytes
            |> stringGroupsOf 2
            |> List.map byteToChar
            |> Result.Extra.combine
            |> Result.map String.fromList


trimBytesLeft len str =
    String.dropLeft (64 - (2 * len)) str


trimBytesRight len str =
    String.dropRight (64 - (2 * len)) str


stringGroupsOf : Int -> String -> List String
stringGroupsOf num str =
    str
        |> String.toList
        |> List.Extra.groupsOf num
        |> List.map String.fromList
