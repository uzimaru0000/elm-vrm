module DataDecode exposing (suite)

import Color
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as JD
import Math.Vector3 exposing (vec3)
import Math.Vector4 exposing (vec4)
import Test exposing (..)
import VRM.Data exposing (..)


suite : Test
suite =
    describe "JSONをDecode出来る"
        [ [ ( "5120", Byte )
          , ( "5121", UnsignedByte )
          , ( "5122", Short )
          , ( "5123", UnsignedShort )
          , ( "5124", Int )
          , ( "5125", UnsignedInt )
          , ( "5126", Float )
          ]
            |> List.map
                (\( f, s ) ->
                    test f <|
                        \_ ->
                            JD.decodeString componentTypeDecoder f
                                |> Result.toMaybe
                                |> Expect.equal (Just s)
                )
            |> concat
        , test "sceneDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "nodes": [
                                0,
                                13,
                                142
                            ]
                        }
                        """

                    expected =
                        { nodes = [ 0, 13, 142 ] }
                in
                decoderTest sceneDecoder json expected
        , test "textureDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "sampler": 0,
                            "source": 0
                        }
                        """

                    expected =
                        { sampler = 0, source = 0 }
                in
                decoderTest textureDecoder json expected
        , test "skinDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "inverseBindMatrices": 200,
                            "joints": [
                                48,
                                49,
                                115,
                                50
                            ],
                            "skeleton": 14
                        }
                        """

                    expected =
                        { inverseBindMatrices = Just 200
                        , joints =
                            [ 48
                            , 49
                            , 115
                            , 50
                            ]
                        , skeleton = Just 14
                        }
                in
                decoderTest skinDecoder json expected
        , test "node" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "name": "mesh",
                            "children": [
                                1,
                                2,
                                3,
                                4,
                                5,
                                6,
                                7,
                                8,
                                9,
                                10,
                                11,
                                12
                            ],
                            "translation": [
                                0,
                                0,
                                0
                            ],
                            "rotation": [
                                0,
                                0,
                                0,
                                1
                            ],
                            "scale": [
                                1,
                                1,
                                1
                            ],
                            "extras": {}
                        }
                        """

                    expected =
                        { camera = Nothing
                        , children =
                            [ 1
                            , 2
                            , 3
                            , 4
                            , 5
                            , 6
                            , 7
                            , 8
                            , 9
                            , 10
                            , 11
                            , 12
                            ]
                        , skin = Nothing
                        , matrix = Nothing
                        , mesh = Nothing
                        , rotation = Just <| vec4 0 0 0 1
                        , scale = Just <| vec3 1 1 1
                        , translation = Just <| vec3 0 0 0
                        , weights = Nothing
                        }
                in
                decoderTest nodeDecoder json expected
        , test "samplerDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "magFilter": 9729,
                            "minFilter": 9729,
                            "wrapS": 10497,
                            "wrapT": 10497
                        }
                        """

                    expected =
                        { magFilter = Linear
                        , minFilter = Linear
                        , wrapS = Repeat
                        , wrapT = Repeat
                        }
                in
                decoderTest samplerDecoder json expected
        , test "primitiveDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "mode": 4,
                            "indices": 5,
                            "attributes": {
                                "POSITION": 0,
                                "NORMAL": 1,
                                "TEXCOORD_0": 2,
                                "JOINTS_0": 4,
                                "WEIGHTS_0": 3
                            },
                            "material": 0
                        }
                        """

                    expected =
                        { attributes =
                            Dict.fromList
                                [ ( "POSITION", 0 )
                                , ( "NORMAL", 1 )
                                , ( "TEXCOORD_0", 2 )
                                , ( "JOINTS_0", 4 )
                                , ( "WEIGHTS_0", 3 )
                                ]
                        , indices = Just 5
                        , material = Just 0
                        , mode = Triangles
                        , targets = Nothing
                        }
                in
                decoderTest primitiveDecoder json expected
        , test "meshDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "name": "body_top.baked",
                            "primitives": [
                                {
                                    "mode": 4,
                                    "indices": 5,
                                    "attributes": {
                                        "POSITION": 0,
                                        "NORMAL": 1,
                                        "TEXCOORD_0": 2,
                                        "JOINTS_0": 4,
                                        "WEIGHTS_0": 3
                                    },
                                    "material": 0
                                }
                            ]
                        }
                        """

                    expected =
                        { primitives =
                            [ { attributes =
                                    Dict.fromList
                                        [ ( "POSITION", 0 )
                                        , ( "NORMAL", 1 )
                                        , ( "TEXCOORD_0", 2 )
                                        , ( "JOINTS_0", 4 )
                                        , ( "WEIGHTS_0", 3 )
                                        ]
                              , indices = Just 5
                              , material = Just 0
                              , mode = Triangles
                              , targets = Nothing
                              }
                            ]
                        , weights = Nothing
                        }
                in
                decoderTest meshDecoder json expected
        , test "textureInfoDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "index": 3,
                            "texCoord": 0
                        }
                        """

                    expected =
                        { index = 3
                        , texCoord = 0
                        }
                in
                decoderTest textureInfoDecoder json expected
        , test "occulusionTextureInfoDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "index": 3,
                            "strength": 1
                        }
                        """

                    expected =
                        { index = 3
                        , texCoord = 0
                        , strength = 1
                        }
                in
                decoderTest occulutionTextureInfoDecoder json expected
        , test "normalTextureInfoDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "index": 3,
                            "texCoord": 2,
                            "scale": 0
                        }
                        """

                    expected =
                        { index = 3
                        , texCoord = 2
                        , scale = 0
                        }
                in
                decoderTest normalTextureInfoDecoder json expected
        , test "pbrMetallicRoughnessDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "baseColorTexture": {
                                "index": 3,
                                "texCoord": 0
                            },
                            "baseColorFactor": [
                                1,
                                1,
                                1,
                                1
                            ],
                            "metallicFactor": 0,
                            "roughnessFactor": 0.9
                        }
                        """

                    expected =
                        { baseColorTexture =
                            { index = 3
                            , texCoord = 0
                            }
                        , baseColorFactor = Color.rgba 1 1 1 1
                        , metallicFactor = 0.0
                        , roughnessFactor = 0.9
                        , metallicRoughnessTexture = Nothing
                        }
                in
                decoderTest pbrMetallicRoughnessDecoder json expected
        , test "materialDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "name": "Alicia_body",
                            "pbrMetallicRoughness": {
                                "baseColorTexture": {
                                    "index": 0,
                                    "texCoord": 0
                                },
                                "baseColorFactor": [
                                    1,
                                    1,
                                    1,
                                    1
                                ],
                                "metallicFactor": 0,
                                "roughnessFactor": 0.9
                            },
                            "emissiveFactor": [
                                0,
                                0,
                                0
                            ],
                            "alphaMode": "OPAQUE",
                            "alphaCutoff": 0.5,
                            "doubleSided": false,
                            "extensions": {
                                "KHR_materials_unlit": {}
                            }
                        }
                        """

                    expected =
                        { pbrMetallicRoughness =
                            Just
                                { baseColorTexture =
                                    { index = 0
                                    , texCoord = 0
                                    }
                                , baseColorFactor = Color.rgba 1 1 1 1
                                , metallicFactor = 0.0
                                , roughnessFactor = 0.9
                                , metallicRoughnessTexture = Nothing
                                }
                        , normalTexture = Nothing
                        , occlusionTexture = Nothing
                        , emissiveTexture = Nothing
                        , emissiveFactor = vec3 0 0 0
                        , alphaMode = Opaque
                        , alphaCutoff = 0.5
                        , doubleSided = False
                        }
                in
                decoderTest materialDecoder json expected
        , test "imageDecoder with uri" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "name": "Alicia_body",
                            "uri": "AliciaSolid.vrm_img0.png"
                        }
                        """

                    expected =
                        UriImage
                            { uri = "AliciaSolid.vrm_img0.png"
                            , mimeType = Nothing
                            }
                in
                decoderTest imageDecoder json expected
        , test "imageDecoder with bufferView" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "name": "Alicia_body",
                            "bufferView": 0,
                            "mimeType": "image/jpeg"
                        }
                        """

                    expected =
                        BufferImage
                            { bufferView = 0
                            , mimeType = Just Jpeg
                            }
                in
                decoderTest imageDecoder json expected
        , test "bufferViewDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "buffer": 0,
                            "byteOffset": 0,
                            "byteLength": 57648,
                            "target": 34962
                        }
                        """

                    expected =
                        { buffer = 0
                        , byteOffset = 0
                        , byteLength = 57648
                        , byteStride = Nothing
                        , target = Just ArrayBuffer
                        }
                in
                decoderTest bufferViewDecoder json expected
        , test "bufferDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "uri": "AliciaSolid.vrm_data.bin",
                            "byteLength": 3945328
                        }
                        """

                    expected =
                        { uri = Just "AliciaSolid.vrm_data.bin"
                        , byteLength = 3945328
                        }
                in
                decoderTest bufferDecoder json expected
        , test "assetDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "generator": "UniGLTF-1.28",
                            "version": "2.0"
                        }
                        """

                    expected =
                        { copyright = Nothing
                        , generator = Just "UniGLTF-1.28"
                        , version = "2.0"
                        , minVersion = Nothing
                        }
                in
                decoderTest assetDecoder json expected
        , test "sparseValueDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "bufferView": 0,
                            "byteOffset": 0
                        }
                        """

                    expected =
                        { bufferView = 0
                        , byteOffset = 0
                        }
                in
                decoderTest sparseValueDecoder json expected
        , test "sparseIndexDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "bufferView": 0,
                            "byteOffset": 0,
                            "componentType": 5121
                        }
                        """

                    expected =
                        { bufferView = 0
                        , byteOffset = 0
                        , componentType = UnsignedByte
                        }
                in
                decoderTest sparseIndexDecoder json expected
        , test "sparseDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "count": 0,
                            "values": {
                                "bufferView": 0,
                                "byteOffset": 0
                            },
                            "indices": {
                                "bufferView": 0,
                                "byteOffset": 0,
                                "componentType": 5121
                            }
                        }
                        """

                    expected =
                        { count = 0
                        , values =
                            { bufferView = 0
                            , byteOffset = 0
                            }
                        , indices =
                            { bufferView = 0
                            , byteOffset = 0
                            , componentType = UnsignedByte
                            }
                        }
                in
                decoderTest sparseDecoder json expected
        , test "accessorDecoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "bufferView": 0,
                            "byteOffset": 0,
                            "type": "VEC3",
                            "componentType": 5126,
                            "count": 4804,
                            "max": [
                                0.614650965,
                                1.31239367,
                                0.150282308
                            ],
                            "min": [
                                -0.614748538,
                                0.9991788,
                                -0.0840013
                            ],
                            "normalized": false
                        }
                        """

                    expected =
                        { bufferView = Just 0
                        , byteOffset = 0
                        , componentType = Float
                        , normalized = False
                        , count = 4804
                        , type_ = Vec3
                        , max =
                            Just
                                [ 0.614650965
                                , 1.31239367
                                , 0.150282308
                                ]
                        , min =
                            Just
                                [ -0.614748538
                                , 0.9991788
                                , -0.0840013
                                ]
                        , sparse = Nothing
                        }
                in
                decoderTest accessorDecoder json expected
        , test "decoder" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "scenes" : [
                                {
                                    "nodes" : [ 0 ]
                                }
                            ],
                            "nodes" : [
                                {
                                    "mesh" : 0
                                }
                            ],
                            "meshes" : [
                                {
                                "primitives" : [ {
                                    "attributes" : {
                                        "POSITION" : 1
                                    },
                                    "indices" : 0
                                } ]
                                }
                            ],
                            "buffers" : [
                                {
                                "uri" : "data:application/octet-stream;base64,AAABAAIAAAAAAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAACAPwAAAAA=",
                                "byteLength" : 44
                                }
                            ],
                            "bufferViews" : [
                                {
                                "buffer" : 0,
                                "byteOffset" : 0,
                                "byteLength" : 6,
                                "target" : 34963
                                },
                                {
                                "buffer" : 0,
                                "byteOffset" : 8,
                                "byteLength" : 36,
                                "target" : 34962
                                }
                            ],
                            "accessors" : [
                                {
                                "bufferView" : 0,
                                "byteOffset" : 0,
                                "componentType" : 5123,
                                "count" : 3,
                                "type" : "SCALAR",
                                "max" : [ 2 ],
                                "min" : [ 0 ]
                                },
                                {
                                "bufferView" : 1,
                                "byteOffset" : 0,
                                "componentType" : 5126,
                                "count" : 3,
                                "type" : "VEC3",
                                "max" : [ 1.0, 1.0, 0.0 ],
                                "min" : [ 0.0, 0.0, 0.0 ]
                                }
                            ],
                            "asset" : {
                                "version" : "2.0"
                            }
                        }
                        """

                    expected : Data
                    expected =
                        { scene = Nothing
                        , scenes =
                            [ { nodes = [ 0 ] }
                            ]
                        , extensionsUsed = []
                        , extensionsRequired = []
                        , accessors =
                            [ { bufferView = Just 0
                              , byteOffset = 0
                              , componentType = UnsignedShort
                              , normalized = False
                              , count = 3
                              , type_ = Scalar
                              , max = Just [ 2 ]
                              , min = Just [ 0 ]
                              , sparse = Nothing
                              }
                            , { bufferView = Just 1
                              , byteOffset = 0
                              , componentType = Float
                              , normalized = False
                              , count = 3
                              , type_ = Vec3
                              , max = Just [ 1.0, 1.0, 0.0 ]
                              , min = Just [ 0.0, 0.0, 0.0 ]
                              , sparse = Nothing
                              }
                            ]
                        , asset =
                            { copyright = Nothing
                            , generator = Nothing
                            , version = "2.0"
                            , minVersion = Nothing
                            }
                        , buffers =
                            [ { uri = Just "data:application/octet-stream;base64,AAABAAIAAAAAAAAAAAAAAAAAAAAAAIA/AAAAAAAAAAAAAAAAAACAPwAAAAA="
                              , byteLength = 44
                              }
                            ]
                        , bufferViews =
                            [ { buffer = 0
                              , byteOffset = 0
                              , byteLength = 6
                              , byteStride = Nothing
                              , target = Just ElementArrayBuffer
                              }
                            , { buffer = 0
                              , byteOffset = 8
                              , byteLength = 36
                              , byteStride = Nothing
                              , target = Just ArrayBuffer
                              }
                            ]
                        , images =
                            []
                        , materials =
                            []
                        , meshes =
                            [ { primitives =
                                    [ { attributes = Dict.fromList [ ( "POSITION", 1 ) ]
                                      , indices = Just 0
                                      , material = Nothing
                                      , mode = Triangles
                                      , targets = Nothing
                                      }
                                    ]
                              , weights = Nothing
                              }
                            ]
                        , nodes =
                            [ { mesh = Just 0
                              , camera = Nothing
                              , children = []
                              , skin = Nothing
                              , matrix = Nothing
                              , rotation = Nothing
                              , scale = Nothing
                              , translation = Nothing
                              , weights = Nothing
                              }
                            ]
                        , samplers =
                            []
                        , skins =
                            []
                        , textures =
                            []
                        }
                in
                decoderTest decoder json expected
        ]


decoderTest : JD.Decoder a -> String -> a -> Expectation
decoderTest f json expected =
    JD.decodeString f json
        |> Result.toMaybe
        |> Expect.equal (Just expected)
