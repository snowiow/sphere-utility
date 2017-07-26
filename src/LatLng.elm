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


degToRad : Float -> Float
degToRad deg =
    let
        pi180 =
            pi / 180
    in
        deg * pi180


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
