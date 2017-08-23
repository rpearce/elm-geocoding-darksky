module Weather
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


-- Models


type alias Model =
    { currently : Currently
    }


type alias Currently =
    { icon : String
    , summary : String
    , temperature : Float
    }


initialModel : Model
initialModel =
    { currently = initialCurrently
    }


initialCurrently : Currently
initialCurrently =
    { icon = "–"
    , summary = "–"
    , temperature = 0
    }


weatherUrl : ( Float, Float ) -> String
weatherUrl ( lat, lng ) =
    "http://localhost:5051/forecast/"
        ++ (toString lat)
        ++ ","
        ++ (toString lng)



-- Commands


fetch : ( Float, Float ) -> Cmd Msg
fetch coords =
    Http.get (weatherUrl coords) decodeWeather
        |> Http.send ReceiveWeather



-- Decoders


decodeWeather : Decoder Model
decodeWeather =
    decode Model
        |> required "currently" decodeCurrently


decodeCurrently : Decoder Currently
decodeCurrently =
    decode Currently
        |> required "icon" string
        |> required "summary" string
        |> required "temperature" float



-- Msg


type Msg
    = ReceiveWeather (Result Http.Error Model)



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReceiveWeather (Ok resp) ->
            { model | currently = resp.currently }

        ReceiveWeather (Err _) ->
            model



-- View


view : Model -> Html Msg
view model =
    p [] [ text ("Weather: " ++ (toString <| round <| model.currently.temperature)) ]
