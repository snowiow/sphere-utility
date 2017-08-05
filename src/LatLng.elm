module LatLng exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type_, value, class, id, for)
import HtmlEvents exposing (..)


type alias LatLng =
    { lat : Float
    , lng : Float
    , err : String
    }


initLatLng : LatLng
initLatLng =
    { lat = 0
    , lng = 0
    , err = ""
    }


parseLat : String -> Result String Float
parseLat str =
    parseVal str 90 "latitude"


parseLng : String -> Result String Float
parseLng str =
    parseVal str 180 "longitude"


parseVal : String -> Float -> String -> Result String Float
parseVal str border name =
    case String.toFloat str of
        Ok float ->
            if float >= -border && float <= border then
                Ok float
            else
                Err
                    ("Illegal value for "
                        ++ name
                        ++ " given (only between -"
                        ++ (toString border)
                        ++ " to "
                        ++ (toString border)
                        ++ " allowed)"
                    )

        Err msg ->
            Err msg


degToRad : Float -> Float
degToRad deg =
    let
        pi180 =
            pi / 180
    in
        deg * pi180


radToDeg : Float -> Float
radToDeg rad =
    rad * 180 / pi


ieeeReminder : Float -> Float -> Float
ieeeReminder f1 f2 =
    let
        div =
            f1 / f2 |> round |> toFloat
    in
        f1 - (div * f2)


normalize : LatLng -> LatLng
normalize latlng =
    let
        pi2 =
            pi / 2

        minPiLat =
            min pi2 (degToRad latlng.lat)
    in
        { lat = max -pi2 minPiLat
        , lng = ieeeReminder (degToRad latlng.lng) (2 * pi)
        , err = ""
        }



-- UPDATE


type Msg
    = InputLat String
    | InputLng String


update : Msg -> LatLng -> LatLng
update msg latlng =
    case msg of
        InputLat val ->
            case parseLat val of
                Ok lat ->
                    { latlng | lat = lat }

                Err msg ->
                    { latlng | err = msg }

        InputLng val ->
            case parseLng val of
                Ok lng ->
                    { latlng | lng = lng }

                Err msg ->
                    { latlng | err = msg }


view : LatLng -> Html Msg
view latlng =
    div [ class "col-5" ]
        [ h5 [ class "error" ] [ text latlng.err ]
        , div [ class "form-group row" ]
            [ label [ for "lat" ] [ text "Latitude" ]
            , input
                [ type_ "text"
                , onBlurValue InputLat
                , value (toString latlng.lat)
                , class "form-control"
                , id "lat"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "lng" ] [ text "Longitude" ]
            , input
                [ type_ "text"
                , onBlurValue InputLng
                , value (toString latlng.lng)
                , class "form-control"
                , id "lng"
                ]
                []
            ]
        ]
