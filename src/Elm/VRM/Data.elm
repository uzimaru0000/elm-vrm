module VRM.Data exposing
    ( Accessor
    , AccessorType(..)
    , AlphaMode(..)
    , Asset
    , Buffer
    , BufferView
    , ComponentType(..)
    , Data
    , Filter(..)
    , Image(..)
    , Material
    , Mesh
    , MimeType(..)
    , Node
    , NormalTextureInfo
    , OcculutionTextureInfo
    , PBRMetallicRoughness
    , Primitive
    , RenderType(..)
    , Sampler
    , Scene
    , Skin
    , Sparse
    , SparseIndex
    , SparseValue
    , Target(..)
    , Texture
    , TextureInfo
    , WrapMode(..)
    , accessorDecoder
    , accessorType
    , accessorTypeDecoder
    , alphaMode
    , alphaModeDecoder
    , assetDecoder
    , bufferDecoder
    , bufferViewDecoder
    , colorDecoder
    , componentType
    , componentTypeDecoder
    , decoder
    , filter
    , filterDecoder
    , imageDecoder
    , mat4Decoder
    , materialDecoder
    , meshDecoder
    , mimeType
    , mimeTypeDecoder
    , nodeDecoder
    , normalTextureInfoDecoder
    , occulutionTextureInfoDecoder
    , pbrMetallicRoughnessDecoder
    , primitiveDecoder
    , renderType
    , renderTypeDecoder
    , samplerDecoder
    , sceneDecoder
    , skinDecoder
    , sparseDecoder
    , sparseIndexDecoder
    , sparseValueDecoder
    , target
    , targetDecoder
    , textureDecoder
    , textureInfoDecoder
    , vec3Decoder
    , vec4Decoder
    , wrapMode
    , wrapModeDecoder
    )

import Color exposing (Color)
import Dict exposing (Dict)
import Json.Decode as JD exposing (..)
import Json.Decode.Pipeline as JD
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)


type ComponentType
    = Byte
    | UnsignedByte
    | Short
    | UnsignedShort
    | Int
    | UnsignedInt
    | Float


type AccessorType
    = Scalar
    | Vec2
    | Vec3
    | Vec4
    | Mat2
    | Mat3
    | Mat4


type Target
    = ArrayBuffer
    | ElementArrayBuffer


type Filter
    = Nearest
    | Linear
    | NearestMipmapNearest
    | LinearMipmapNearest
    | NearestMipmapLinear
    | LinearMipmapLinear


type WrapMode
    = ClampToEdge
    | MirroredRepeat
    | Repeat


type MimeType
    = Jpeg
    | Png


type AlphaMode
    = Opaque
    | Mask
    | Blend


type RenderType
    = Points
    | Lines
    | LineLoop
    | LineStrip
    | Triangles
    | TriangleStrip
    | TriangleFan


type alias Data =
    { scene : Maybe Int
    , scenes : List Scene
    , extensionsUsed : List String
    , extensionsRequired : List String
    , accessors : List Accessor
    , asset : Asset
    , buffers : List Buffer
    , bufferViews : List BufferView
    , images : List Image
    , materials : List Material
    , meshes : List Mesh
    , nodes : List Node
    , samplers : List Sampler
    , skins : List Skin
    , textures : List Texture
    }


type alias Accessor =
    { bufferView : Maybe Int
    , byteOffset : Int
    , componentType : ComponentType
    , normalized : Bool
    , count : Int
    , type_ : AccessorType
    , max : Maybe (List Float)
    , min : Maybe (List Float)
    , sparse : Maybe Sparse
    }


type alias Sparse =
    { count : Int
    , indices : SparseIndex
    , values : SparseValue
    }


type alias SparseIndex =
    { bufferView : Int
    , byteOffset : Int
    , componentType : ComponentType
    }


type alias SparseValue =
    { bufferView : Int
    , byteOffset : Int
    }


type alias Asset =
    { copyright : Maybe String
    , generator : Maybe String
    , version : String
    , minVersion : Maybe String
    }


type alias Buffer =
    { uri : Maybe String
    , byteLength : Int
    }


type alias BufferView =
    { buffer : Int
    , byteOffset : Int
    , byteLength : Int
    , byteStride : Maybe Int
    , target : Maybe Target
    }


type Image
    = UriImage
        { uri : String
        , mimeType : Maybe MimeType
        }
    | BufferImage
        { bufferView : Int
        , mimeType : Maybe MimeType
        }


type alias Material =
    { pbrMetallicRoughness : Maybe PBRMetallicRoughness
    , normalTexture : Maybe NormalTextureInfo
    , occlusionTexture : Maybe OcculutionTextureInfo
    , emissiveTexture : Maybe TextureInfo
    , emissiveFactor : Vec3
    , alphaMode : AlphaMode
    , alphaCutoff : Float
    , doubleSided : Bool
    }


type alias PBRMetallicRoughness =
    { baseColorFactor : Color
    , baseColorTexture : TextureInfo
    , metallicFactor : Float
    , roughnessFactor : Float
    , metallicRoughnessTexture : Maybe TextureInfo
    }


type alias NormalTextureInfo =
    { index : Int
    , texCoord : Int
    , scale : Int
    }


type alias OcculutionTextureInfo =
    { index : Int
    , texCoord : Int
    , strength : Int
    }


type alias TextureInfo =
    { index : Int
    , texCoord : Int
    }


type alias Mesh =
    { primitives : List Primitive
    , weights : Maybe (List Int)
    }


type alias Primitive =
    { attributes : Dict String Int
    , indices : Maybe Int
    , material : Maybe Int
    , mode : RenderType
    , targets : Maybe (List (Dict String Int))
    }


type alias Node =
    { camera : Maybe Int
    , children : List Int
    , skin : Maybe Int
    , matrix : Maybe Mat4
    , mesh : Maybe Int
    , rotation : Maybe Vec4
    , scale : Maybe Vec3
    , translation : Maybe Vec3
    , weights : Maybe (List Int)
    }


type alias Sampler =
    { magFilter : Filter
    , minFilter : Filter
    , wrapS : WrapMode
    , wrapT : WrapMode
    }


type alias Skin =
    { inverseBindMatrices : Maybe Int
    , skeleton : Maybe Int
    , joints : List Int
    }


type alias Texture =
    { sampler : Int
    , source : Int
    }


type alias Scene =
    { nodes : List Int
    }


mappedDecoder : Decoder a -> (a -> Maybe b) -> String -> Decoder b
mappedDecoder trigger f failMsg =
    trigger
        |> JD.map f
        |> JD.andThen
            (\x ->
                case x of
                    Just v ->
                        succeed v

                    Nothing ->
                        fail failMsg
            )


componentType : Int -> Maybe ComponentType
componentType x =
    case x of
        5120 ->
            Just Byte

        5121 ->
            Just UnsignedByte

        5122 ->
            Just Short

        5123 ->
            Just UnsignedShort

        5125 ->
            Just UnsignedInt

        5126 ->
            Just Float

        _ ->
            Just Int


accessorType : String -> Maybe AccessorType
accessorType x =
    case x of
        "SCALAR" ->
            Just Scalar

        "VEC2" ->
            Just Vec2

        "VEC3" ->
            Just Vec3

        "VEC4" ->
            Just Vec4

        "MAT2" ->
            Just Mat2

        "MAT3" ->
            Just Mat3

        "MAT4" ->
            Just Mat4

        _ ->
            Nothing


target : Int -> Maybe Target
target x =
    case x of
        34962 ->
            Just ArrayBuffer

        34963 ->
            Just ElementArrayBuffer

        _ ->
            Nothing


filter : Int -> Maybe Filter
filter x =
    case x of
        9728 ->
            Just Nearest

        9729 ->
            Just Linear

        9984 ->
            Just NearestMipmapNearest

        9985 ->
            Just LinearMipmapNearest

        9986 ->
            Just NearestMipmapLinear

        9987 ->
            Just LinearMipmapLinear

        _ ->
            Nothing


wrapMode : Int -> Maybe WrapMode
wrapMode x =
    case x of
        33071 ->
            Just ClampToEdge

        33648 ->
            Just MirroredRepeat

        10497 ->
            Just Repeat

        _ ->
            Nothing


mimeType : String -> Maybe MimeType
mimeType x =
    case x of
        "image/jpeg" ->
            Just Jpeg

        "image/png" ->
            Just Png

        _ ->
            Nothing


alphaMode : String -> Maybe AlphaMode
alphaMode x =
    case x of
        "OPAQUE" ->
            Just Opaque

        "MASK" ->
            Just Mask

        "BLEND" ->
            Just Blend

        _ ->
            Nothing


renderType : Int -> Maybe RenderType
renderType x =
    case x of
        0 ->
            Just Points

        1 ->
            Just Lines

        2 ->
            Just LineLoop

        3 ->
            Just LineStrip

        4 ->
            Just Triangles

        5 ->
            Just TriangleStrip

        6 ->
            Just TriangleFan

        _ ->
            Nothing


componentTypeDecoder : Decoder ComponentType
componentTypeDecoder =
    mappedDecoder int componentType "invalid componentType"


accessorTypeDecoder : Decoder AccessorType
accessorTypeDecoder =
    mappedDecoder string accessorType "invalid accessorType"


targetDecoder : Decoder Target
targetDecoder =
    mappedDecoder int target "invalid target"


filterDecoder : Decoder Filter
filterDecoder =
    mappedDecoder int filter "invalid filter"


wrapModeDecoder : Decoder WrapMode
wrapModeDecoder =
    mappedDecoder int wrapMode "invalid wrapMode"


mimeTypeDecoder : Decoder MimeType
mimeTypeDecoder =
    mappedDecoder string mimeType "invalid mimeType"


alphaModeDecoder : Decoder AlphaMode
alphaModeDecoder =
    mappedDecoder string alphaMode "invalid alphaMode"


renderTypeDecoder : Decoder RenderType
renderTypeDecoder =
    mappedDecoder int renderType "invalid renderType"


decoder : Decoder Data
decoder =
    succeed Data
        |> JD.optional "scene" (maybe int) Nothing
        |> JD.optional "scenes" (list sceneDecoder) []
        |> JD.optional "extensionsUsed" (list string) []
        |> JD.optional "extensionsRequired" (list string) []
        |> JD.optional "accessors" (list accessorDecoder) []
        |> JD.required "asset" assetDecoder
        |> JD.optional "buffers" (list bufferDecoder) []
        |> JD.optional "bufferViews" (list bufferViewDecoder) []
        |> JD.optional "images" (list imageDecoder) []
        |> JD.optional "materials" (list materialDecoder) []
        |> JD.optional "meshes" (list meshDecoder) []
        |> JD.optional "nodes" (list nodeDecoder) []
        |> JD.optional "samplers" (list samplerDecoder) []
        |> JD.optional "skins" (list skinDecoder) []
        |> JD.optional "textures" (list textureDecoder) []


accessorDecoder : Decoder Accessor
accessorDecoder =
    succeed Accessor
        |> JD.optional "bufferView" (maybe int) Nothing
        |> JD.optional "byteOffset" int 0
        |> JD.required "componentType" componentTypeDecoder
        |> JD.optional "normalized" bool False
        |> JD.required "count" int
        |> JD.required "type" accessorTypeDecoder
        |> JD.optional "max" (maybe <| list float) Nothing
        |> JD.optional "min" (maybe <| list float) Nothing
        |> JD.optional "sparse" (maybe sparseDecoder) Nothing


sparseDecoder : Decoder Sparse
sparseDecoder =
    succeed Sparse
        |> JD.required "count" int
        |> JD.required "indices" sparseIndexDecoder
        |> JD.required "values" sparseValueDecoder


sparseIndexDecoder : Decoder SparseIndex
sparseIndexDecoder =
    succeed SparseIndex
        |> JD.required "bufferView" int
        |> JD.optional "byteOffset" int 0
        |> JD.required "componentType" componentTypeDecoder


sparseValueDecoder : Decoder SparseValue
sparseValueDecoder =
    succeed SparseValue
        |> JD.required "bufferView" int
        |> JD.optional "byteOffset" int 0


assetDecoder : Decoder Asset
assetDecoder =
    succeed Asset
        |> JD.optional "copyright" (maybe string) Nothing
        |> JD.optional "generator" (maybe string) Nothing
        |> JD.required "version" string
        |> JD.optional "minVersion" (maybe string) Nothing


bufferDecoder : Decoder Buffer
bufferDecoder =
    succeed Buffer
        |> JD.optional "uri" (maybe string) Nothing
        |> JD.required "byteLength" int


bufferViewDecoder : Decoder BufferView
bufferViewDecoder =
    succeed BufferView
        |> JD.required "buffer" int
        |> JD.optional "byteOffset" int 0
        |> JD.required "byteLength" int
        |> JD.optional "byteStride" (maybe int) Nothing
        |> JD.optional "target" (maybe targetDecoder) Nothing


imageDecoder : Decoder Image
imageDecoder =
    let
        toDecoder : Maybe String -> Maybe MimeType -> Maybe Int -> Decoder Image
        toDecoder maybeUri mime maybeBufferView =
            case ( maybeUri, maybeBufferView ) of
                ( Just uri, Nothing ) ->
                    succeed <| UriImage { uri = uri, mimeType = mime }

                ( Nothing, Just bufferView ) ->
                    succeed <| BufferImage { bufferView = bufferView, mimeType = mime }

                _ ->
                    fail "invalid image"
    in
    succeed toDecoder
        |> JD.optional "uri" (maybe string) Nothing
        |> JD.optional "mimeType" (maybe mimeTypeDecoder) Nothing
        |> JD.optional "bufferView" (maybe int) Nothing
        |> JD.resolve


materialDecoder : Decoder Material
materialDecoder =
    succeed Material
        |> JD.optional "pbrMetallicRoughness" (maybe pbrMetallicRoughnessDecoder) Nothing
        |> JD.optional "normalTexture" (maybe normalTextureInfoDecoder) Nothing
        |> JD.optional "occlusionTexture" (maybe occulutionTextureInfoDecoder) Nothing
        |> JD.optional "emissiveTexture" (maybe textureInfoDecoder) Nothing
        |> JD.optional "emissiveFactor" vec3Decoder (vec3 0 0 0)
        |> JD.optional "alphaMode" alphaModeDecoder Opaque
        |> JD.required "alphaCutoff" float
        |> JD.optional "doubleSided" bool False


pbrMetallicRoughnessDecoder : Decoder PBRMetallicRoughness
pbrMetallicRoughnessDecoder =
    succeed PBRMetallicRoughness
        |> JD.optional "baseColorFactor" colorDecoder (Color.rgba 1.0 1.0 1.0 1.0)
        |> JD.required "baseColorTexture" textureInfoDecoder
        |> JD.required "metallicFactor" float
        |> JD.required "roughnessFactor" float
        |> JD.optional "metallicRoughnessTexture" (maybe textureInfoDecoder) Nothing


normalTextureInfoDecoder : Decoder NormalTextureInfo
normalTextureInfoDecoder =
    succeed NormalTextureInfo
        |> JD.required "index" int
        |> JD.optional "texCoord" int 0
        |> JD.required "scale" int


occulutionTextureInfoDecoder : Decoder OcculutionTextureInfo
occulutionTextureInfoDecoder =
    succeed OcculutionTextureInfo
        |> JD.required "index" int
        |> JD.optional "texCoord" int 0
        |> JD.required "strength" int


textureInfoDecoder : Decoder TextureInfo
textureInfoDecoder =
    succeed TextureInfo
        |> JD.required "index" int
        |> JD.optional "texCoord" int 0


meshDecoder : Decoder Mesh
meshDecoder =
    succeed Mesh
        |> JD.required "primitives" (list primitiveDecoder)
        |> JD.optional "weights" (maybe <| list int) Nothing


primitiveDecoder : Decoder Primitive
primitiveDecoder =
    succeed Primitive
        |> JD.required "attributes" (dict int)
        |> JD.optional "indices" (maybe int) Nothing
        |> JD.optional "material" (maybe int) Nothing
        |> JD.optional "mode" renderTypeDecoder Triangles
        |> JD.optional "targets" (maybe <| list <| dict int) Nothing


samplerDecoder : Decoder Sampler
samplerDecoder =
    succeed Sampler
        |> JD.required "magFilter" filterDecoder
        |> JD.required "minFilter" filterDecoder
        |> JD.required "wrapS" wrapModeDecoder
        |> JD.required "wrapT" wrapModeDecoder


nodeDecoder : Decoder Node
nodeDecoder =
    succeed Node
        |> JD.optional "camera" (maybe int) Nothing
        |> JD.optional "children" (list int) []
        |> JD.optional "skin" (maybe int) Nothing
        |> JD.optional "matrix" (maybe mat4Decoder) Nothing
        |> JD.optional "mesh" (maybe int) Nothing
        |> JD.optional "rotation" (maybe vec4Decoder) Nothing
        |> JD.optional "scale" (maybe vec3Decoder) Nothing
        |> JD.optional "translation" (maybe vec3Decoder) Nothing
        |> JD.optional "weights" (maybe <| list int) Nothing


skinDecoder : Decoder Skin
skinDecoder =
    JD.map3 Skin
        (maybe <| field "inverseBindMatrices" int)
        (maybe <| field "skeleton" int)
        (field "joints" <| list int)


textureDecoder : Decoder Texture
textureDecoder =
    JD.map2 Texture
        (field "sampler" int)
        (field "source" int)


sceneDecoder : Decoder Scene
sceneDecoder =
    JD.map Scene
        (field "nodes" <| list int)


mat4Decoder : Decoder Mat4
mat4Decoder =
    list float
        |> JD.andThen
            (\x ->
                case x of
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
                            |> succeed

                    _ ->
                        fail "invalid Mat4"
            )


vec4Decoder : Decoder Vec4
vec4Decoder =
    list float
        |> JD.andThen
            (\xs ->
                case xs of
                    [ x, y, z, w ] ->
                        succeed <| vec4 x y z w

                    _ ->
                        fail "invalid vec4"
            )


vec3Decoder : Decoder Vec3
vec3Decoder =
    list float
        |> JD.andThen
            (\xs ->
                case xs of
                    [ x, y, z ] ->
                        succeed <| vec3 x y z

                    _ ->
                        fail "invalid vec3"
            )


colorDecoder : Decoder Color
colorDecoder =
    list float
        |> JD.andThen
            (\xs ->
                case xs of
                    [ r, g, b, a ] ->
                        succeed <| Color.rgba r g b a

                    _ ->
                        fail "invalid color"
            )

