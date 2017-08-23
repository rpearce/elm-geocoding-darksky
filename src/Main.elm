module Main exposing (..)

import Html exposing (Html, div, form, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Geocode
import Weather


-- Models


type alias Model =
    { address : String
    , geocode : Geocode.Model
    , weather : Weather.Model
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { address = ""
    , geocode = Geocode.initialModel
    , weather = Weather.initialModel
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Msg


type Msg
    = UpdateAddress String
    | SendAddress
    | GeocodeMsg Geocode.Msg
    | WeatherMsg Weather.Msg
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
            ( { model
                | geocode = initialModel.geocode
                , weather = initialModel.weather
              }
            , Cmd.map GeocodeMsg (Geocode.fetch model.address)
            )

        GeocodeMsg gmsg ->
            let
                newModel =
                    { model
                        | geocode = Geocode.update gmsg model.geocode
                    }
            in
                ( newModel
                , Cmd.map WeatherMsg (Weather.fetch newModel.geocode.coords)
                )

        WeatherMsg wmsg ->
            ( { model
                | weather = Weather.update wmsg model.weather
              }
            , Cmd.none
            )

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
        , Html.map GeocodeMsg (Geocode.view model.geocode)
        , Html.map WeatherMsg (Weather.view model.weather)
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
