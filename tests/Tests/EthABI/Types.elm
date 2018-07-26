module Tests.EthABI.Types exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import EthABI.Types.Hexstring


suite : Test
suite =
    describe "The EthAbi.Types module"
        [ describe "Hexstring"
            [ let
                creationFun str =
                    (str
                        |> EthABI.Types.Hexstring.hexstring
                        |> Result.map EthABI.Types.Hexstring.toString
                    )
              in
                describe "Creation"
                    [ test "Correct for multiple of 32 bytes" <|
                        \_ ->
                            let
                                str =
                                    String.repeat 32 "fa"
                            in
                                Expect.equal (creationFun str) (Ok str)
                    , test "Incorrect for strings with wrong length" <|
                        \_ ->
                            let
                                str =
                                    String.repeat 42 "fa"
                            in
                                Expect.err (creationFun str)
                    , test "Incorrect for strings with non-hex chars" <|
                        \_ ->
                            let
                                str =
                                    String.repeat 32 "ok"
                            in
                                Expect.err (creationFun str)
                    ]
            ]
        ]
