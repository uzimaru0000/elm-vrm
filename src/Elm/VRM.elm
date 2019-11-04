module VRM exposing (jsonDecoder)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BD
import Json.Decode as JD
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import VRM.Data as VRM
import VRM.Decode as VRM


type alias VRM =
    { mesh : List Float
    , texture : List Float
    }


type Accessor
    = Scalar Float
    | Vec2 Vec2
    | Vec3 Vec3
    | Vec4 Vec4
    | Mat4 Mat4


jsonDecoder : BD.Decoder String
jsonDecoder =
    VRM.chunkDecoder
        |> BD.andThen
            (\( t, n ) ->
                if t == VRM.Json then
                    BD.string n

                else
                    BD.fail
            )


bufferDatas : VRM.Data -> List ( VRM.Accessor, VRM.BufferView )
bufferDatas data =
    List.map2 Tuple.pair data.accessors data.bufferViews


componentDecoder : VRM.ComponentType -> BD.Decoder Float
componentDecoder componentType =
    case componentType of
        VRM.Byte ->
            BD.signedInt8
                |> BD.map toFloat

        VRM.UnsignedByte ->
            BD.unsignedInt8
                |> BD.map toFloat

        VRM.Short ->
            BD.signedInt16 LE
                |> BD.map toFloat

        VRM.UnsignedShort ->
            BD.unsignedInt16 LE
                |> BD.map toFloat

        VRM.Int ->
            BD.signedInt32 LE
                |> BD.map toFloat

        VRM.UnsignedInt ->
            BD.unsignedInt32 LE
                |> BD.map toFloat

        VRM.Float ->
            BD.float32 LE


accessorTypeDecoder : BD.Decoder Float -> VRM.AccessorType -> BD.Decoder Accessor
accessorTypeDecoder d type_ =
    case type_ of
        VRM.Vec2 ->
            VRM.vec2 d
                |> BD.map Vec2

        VRM.Vec3 ->
            VRM.vec3 d
                |> BD.map Vec3

        VRM.Vec4 ->
            VRM.vec4 d
                |> BD.map Vec4

        VRM.Mat4 ->
            VRM.mat4 d
                |> BD.map Mat4

        _ ->
            d
                |> BD.map Scalar


accessorDecoder : ( VRM.Accessor, VRM.BufferView ) -> BD.Decoder (List Accessor)
accessorDecoder ( accessor, bufferView ) =
    let
        d =
            componentDecoder accessor.componentType

        ad =
            accessorTypeDecoder d accessor.type_
    in
    VRM.list accessor.count ad



-- accessorsDecoder : VRM.Data -> BD.Decoder (List Accessor)
-- accessorsDecoder data =
--     let
--         buffers = bufferDatas data
--     in
-- decoder : BD.Decoder VRM
-- decoder =
--     VD.readHeader
--         |> BD.andThen (\_ -> jsonDecoder)
--         |> BD.andThen
--             (\json ->
--                 case JD.decodeString VRM.decoder json of
--                     Ok data ->
--                         BD.succeed data
--                     Err _ ->
--                         BD.fail
--             )
