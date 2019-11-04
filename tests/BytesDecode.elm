module BytesDecode exposing (suite)

import Bytes exposing (Endianness(..))
import Bytes.Decode as BD
import Bytes.Encode as BE
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import VRM exposing (jsonDecoder)
import VRM.Decode as VRM


suite : Test
suite =
    describe "Decodeできる"
        [ test "普通のJson" <|
            \_ ->
                let
                    expected =
                        "{ \"hoge\": 1, \"huga\": false }"

                    actual =
                        BE.sequence
                            [ BE.signedInt32 LE <| String.length expected
                            , BE.string "JSON"
                            , BE.string expected
                            ]
                            |> BE.encode
                            |> BD.decode jsonDecoder
                in
                Expect.equal (Just expected) actual
        , test "Jsonじゃない" <|
            \_ ->
                let
                    expected =
                        "{ \"hoge\": 1, \"\": false }"

                    actual =
                        BE.sequence
                            [ BE.signedInt32 LE <| String.length expected
                            , BE.string "BIN\u{0000}"
                            , BE.string expected
                            ]
                            |> BE.encode
                            |> BD.decode jsonDecoder
                in
                Expect.equal Nothing actual
        , test "任意のバイト数を捨てる" <|
            \_ ->
                let
                    bin =
                        BE.sequence
                            [ BE.string "I'm "
                            , BE.signedInt8 0
                            , BE.signedInt8 0
                            , BE.signedInt8 0
                            , BE.string "human"
                            ]
                            |> BE.encode

                    actual =
                        "I'm human"
                in
                BD.decode
                    (BD.string 4
                        |> BD.andThen (VRM.throw 3)
                        |> BD.andThen (\x -> BD.string 5 |> BD.map (\y -> x ++ y))
                    )
                    bin
                    |> Expect.equal (Just actual)
        ]
