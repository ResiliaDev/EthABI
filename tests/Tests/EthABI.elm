module Tests.EthABI exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Tests.EthABI.Fuzz


suite : Test
suite =
    describe "Round-robin tests to ensure encoding + decoding works as intended."
        [ fuzz Tests.EthABI.Fuzz.encoder_decoder_triple
            "Encoding <-> Decoding Round-Robin"
            (\( input_str, encoded_hexstr, decoded_str ) ->
                 let
                     _ = Debug.log "triple" (input_str, encoded_hexstr, decoded_str)
                 in
                  Expect.equal input_str decoded_str
                  |> (Expect.onFail <|
                  """
                    The input string:
                    """
                    ++ input_str ++
                    """

                    did not match decoded output:
                    """
                    ++ decoded_str ++
                    """

                    The intermediate encoding was:
                    """
                    ++ toString encoded_hexstr ++
                    """
                    )
                    """
            ))
        ]
