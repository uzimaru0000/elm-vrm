module VRM.Decode exposing
    ( Chunk(..)
    , chunkDecoder
    , list
    , readHeader
    , throw
    , vec2
    , vec3
    , vec4
    , mat4
    )

import Bytes exposing (Endianness(..))
import Bytes.Decode as BD exposing (Step(..))
import Json.Decode as JD
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)


type Chunk
    = Json
    | Bin


list : Int -> BD.Decoder a -> BD.Decoder (List a)
list count decoder =
    BD.loop ( count, [] ) (listStep decoder)


listStep : BD.Decoder a -> ( Int, List a ) -> BD.Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        BD.succeed (Done xs)

    else
        BD.map (\x -> Loop ( n - 1, xs ++ [ x ] )) decoder


throw : Int -> a -> BD.Decoder a
throw count currentData =
    BD.loop ( count, [] ) (listStep BD.unsignedInt8)
        |> BD.andThen (\_ -> BD.succeed currentData)


vec2 : BD.Decoder Float -> BD.Decoder Vec2
vec2 decoder =
    list 2 decoder
        |> BD.andThen
            (\xs ->
                case xs of
                    [ x, y ] ->
                        BD.succeed <| Vec2.vec2 x y

                    _ ->
                        BD.fail
            )


vec3 : BD.Decoder Float -> BD.Decoder Vec3
vec3 decoder =
    list 3 decoder
        |> BD.andThen
            (\xs ->
                case xs of
                    [ x, y, z ] ->
                        BD.succeed <| Vec3.vec3 x y z

                    _ ->
                        BD.fail
            )


vec4 : BD.Decoder Float -> BD.Decoder Vec4
vec4 decoder =
    list 4 decoder
        |> BD.andThen
            (\xs ->
                case xs of
                    [ x, y, z, w ] ->
                        BD.succeed <| Vec4.vec4 x y z w

                    _ ->
                        BD.fail
            )


mat4 : BD.Decoder Float -> BD.Decoder Mat4
mat4 decoder =
    list 16 decoder
        |> BD.andThen
            (\xs ->
                case xs of
                    [ m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 ] ->
                        Mat4.fromRecord
                            { m11 = m11
                            , m21 = m21
                            , m31 = m31
                            , m41 = m41
                            , m12 = m12
                            , m22 = m22
                            , m32 = m32
                            , m42 = m42
                            , m13 = m13
                            , m23 = m23
                            , m33 = m33
                            , m43 = m43
                            , m14 = m14
                            , m24 = m24
                            , m34 = m34
                            , m44 = m44
                            }
                            |> BD.succeed

                    _ ->
                        BD.fail
            )


readHeader : BD.Decoder Int
readHeader =
    BD.string 4
        |> BD.andThen
            (\x ->
                if x == "glTF" then
                    BD.succeed ()

                else
                    BD.fail
            )
        |> BD.andThen
            (\_ -> BD.signedInt32 LE)
        |> BD.andThen
            (\_ -> BD.signedInt32 LE)


chunkDecoder : BD.Decoder ( Chunk, Int )
chunkDecoder =
    BD.signedInt32 LE
        |> BD.andThen
            (\x -> BD.string 4 |> BD.map (Tuple.pair x))
        |> BD.andThen
            (\( n, t ) ->
                if t == "JSON" then
                    BD.succeed ( Json, n )

                else if t == "BIN\u{0000}" then
                    BD.succeed ( Bin, n )

                else
                    BD.fail
            )
