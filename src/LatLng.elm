module LatLng exposing (..)


type alias LatLng =
    { lat : Float
    , lng : Float
    }


initLatLng : LatLng
initLatLng =
    { lat = 0
    , lng = 0
    }


setLat : Float -> LatLng -> LatLng
setLat lat model =
    { model
        | lat = lat
    }


setLng : Float -> LatLng -> LatLng
setLng lng model =
    { model
        | lng = lng
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
