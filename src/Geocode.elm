module Geocode
    exposing
        ( Model
        , Msg
        , initialModel
        , fetch
        , update
        , view
        )

import Http
import Html exposing (Html, p, text)
import Json.Decode exposing (float, list, string, Decoder)
import Json.Decode.Pipeline exposing (decode, required)


--import Weather
-- Models


type alias Model =
    { coords : ( Float, Float )
    }


type alias Geo =
    { status : String
    , results : List ResultItem
    }


type alias ResultItem =
    { geometry : Geometry }


type alias Geometry =
    { location : Location }


type alias Location =
    { lat : Float
    , lng : Float
    }


initialModel : Model
initialModel =
    { coords = ( 0, 0 )
    }


initialResultItem : ResultItem
initialResultItem =
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


fetch : String -> Cmd Msg
fetch address =
    Http.get (geocodingUrl address) decodeGeo
        |> Http.send ReceiveGeocoding



-- Decoders


decodeGeo : Decoder Geo
decodeGeo =
    decode Geo
        |> required "status" string
        |> required "results" (list decodeResultItem)


decodeResultItem : Decoder ResultItem
decodeResultItem =
    decode ResultItem
        |> required "geometry" decodeGeometry


decodeGeometry : Decoder Geometry
decodeGeometry =
    decode Geometry
        |> required "location" decodeLocation


decodeLocation : Decoder Location
decodeLocation =
    decode Location
        |> required "lat" float
        |> required "lng" float



-- Msg


type Msg
    = ReceiveGeocoding (Result Http.Error Geo)



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReceiveGeocoding (Ok resp) ->
            let
                result =
                    determineResult resp
            in
                { model
                    | coords =
                        ( result.geometry.location.lat
                        , result.geometry.location.lng
                        )
                }

        ReceiveGeocoding (Err _) ->
            model


determineResult : Geo -> ResultItem
determineResult { results, status } =
    case status of
        "OK" ->
            results
                |> List.head
                |> Maybe.withDefault initialResultItem

        _ ->
            initialResultItem



-- View


view : Model -> Html Msg
view model =
    p [] [ text ("Coords: " ++ toString model.coords) ]
