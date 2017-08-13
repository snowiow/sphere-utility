module LatLng exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type_, value, class, id, for)
import FloatInput exposing (FloatInput)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input


type alias Model =
    { lat : FloatInput
    , lng : FloatInput
    }


type alias LatLng =
    { lat : Float
    , lng : Float
    }


init : Model
init =
    { lat = FloatInput.init
    , lng = FloatInput.init
    }


initLatLng : LatLng
initLatLng =
    { lat = 0
    , lng = 0
    }


modelToLatLng : Model -> LatLng
modelToLatLng model =
    { lat =
        case model.lat.number of
            Just number ->
                number

            Nothing ->
                0
    , lng =
        case model.lng.number of
            Just number ->
                number

            Nothing ->
                0
    }


latLngToModel : LatLng -> Model
latLngToModel latlng =
    { lat =
        { number = Just latlng.lat
        , input = toString latlng.lat
        , err = ""
        }
    , lng =
        { number = Just latlng.lng
        , input = toString latlng.lng
        , err = ""
        }
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
        }



-- UPDATE


type Msg
    = InputLat String
    | InputLng String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputLat val ->
            { model | lat = FloatInput.parse val }

        InputLng val ->
            { model | lng = FloatInput.parse val }


view : Model -> Html Msg
view model =
    Grid.row []
        [ Grid.col [ Col.xs5 ]
            [ Form.form []
                [ Form.group
                    (if String.isEmpty model.lat.err then
                        []
                     else
                        [ Form.groupDanger ]
                    )
                    [ Form.label [ for "lat" ] [ text "Latitude" ]
                    , Input.text
                        [ Input.onInput InputLat
                        , Input.value model.lat.input
                        , Input.id "lat"
                        ]
                    , Form.validationText [] [ text model.lat.err ]
                    ]
                , Form.group
                    (if String.isEmpty model.lng.err then
                        []
                     else
                        [ Form.groupDanger ]
                    )
                    [ Form.label [ for "lng" ] [ text "Longitude" ]
                    , Input.text
                        [ Input.onInput InputLng
                        , Input.value model.lng.input
                        , Input.id "lng"
                        ]
                    , Form.validationText [] [ text model.lng.err ]
                    ]
                ]
            ]
        ]
