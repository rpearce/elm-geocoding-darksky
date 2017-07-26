module Main exposing (..)

import Http
import Html exposing (Html, div, form, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode exposing (float, list, string, Decoder)
import Json.Decode.Pipeline exposing (decode, required)


-- Models


type alias Model =
    { address : String
    , coords : Coords
    }


type alias Coords =
    ( Float, Float )


type alias GeoModel =
    { status : String
    , results : List GeoResult
    }


type alias GeoResult =
    { geometry : GeoGeometry }


type alias GeoGeometry =
    { location : GeoLocation }


type alias GeoLocation =
    { lat : Float
    , lng : Float
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { address = ""
    , coords = ( 0, 0 )
    }


initialGeoResult : GeoResult
initialGeoResult =
    { geometry =
        { location =
            { lat = 0
            , lng = 0
            }
        }
    }


geocodingUrl : String -> String
geocodingUrl address =
    "http://localhost:5050/geocode/" ++ address



-- Commands


sendAddress : String -> Cmd Msg
sendAddress address =
    Http.get (geocodingUrl address) decodeGeo
        |> Http.send ReceiveGeocoding



-- Decoders


decodeGeo : Decoder GeoModel
decodeGeo =
    decode GeoModel
        |> required "status" string
        |> required "results" (list decodeGeoResult)


decodeGeoResult : Decoder GeoResult
decodeGeoResult =
    decode GeoResult
        |> required "geometry" decodeGeoGeometry


decodeGeoGeometry : Decoder GeoGeometry
decodeGeoGeometry =
    decode GeoGeometry
        |> required "location" decodeGeoLocation


decodeGeoLocation : Decoder GeoLocation
decodeGeoLocation =
    decode GeoLocation
        |> required "lat" float
        |> required "lng" float



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Msg


type Msg
    = UpdateAddress String
    | SendAddress
    | ReceiveGeocoding (Result Http.Error GeoModel)
    | NoOp



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAddress text ->
            ( { model | address = text }
            , Cmd.none
            )

        SendAddress ->
            ( model, sendAddress model.address )

        ReceiveGeocoding (Ok { results, status }) ->
            let
                result =
                    case status of
                        "OK" ->
                            results
                                |> List.head
                                |> Maybe.withDefault initialGeoResult

                        _ ->
                            initialGeoResult

                location =
                    result.geometry.location

                newModel =
                    { model | coords = ( location.lat, location.lng ) }
            in
                ( newModel, Cmd.none )

        ReceiveGeocoding (Err _) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ form [ onSubmit SendAddress ]
            [ input
                [ type_ "text"
                , placeholder "City"
                , value model.address
                , onInput UpdateAddress
                ]
                []
            ]
        , p [] [ text ("Coords: " ++ (toString model.coords)) ]
        ]



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
