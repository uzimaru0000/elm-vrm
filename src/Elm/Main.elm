module Main exposing (Model, Msg(..), main, update, view)

import Browser
import Bytes.Decode as B
import Html exposing (Html)
import Http exposing (..)
import Json.Decode as JD
import VRM
import VRM.Data exposing (Data)
import VRM.Decode


type alias Model =
    { data : Maybe Data
    }


type Msg
    = NoOp
    | GetVRM (Result String Data)


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { data = Nothing }
                , Http.get
                    { url = "./vrm/AliciaSolid.vrm"
                    , expect = vrmExpect GetVRM
                    }
                )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


view : Model -> Html Msg
view model =
    Html.text "hello"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetVRM (Ok data) ->
            ( { data = Just data }, Cmd.none )

        _ ->
            ( model, Cmd.none )


vrmExpect : (Result String Data -> Msg) -> Http.Expect Msg
vrmExpect msg =
    expectBytesResponse msg
        (\res ->
            case res of
                GoodStatus_ _ body ->
                    case
                        B.decode
                            (VRM.Decode.readHeader
                                |> B.andThen
                                    (\_ -> VRM.jsonDecoder)
                            )
                            body
                    of
                        Just json ->
                            JD.decodeString VRM.Data.decoder json
                                |> Result.mapError (always "decode failed")

                        Nothing ->
                            Err "bytes decode error"

                _ ->
                    Err "http error"
        )
