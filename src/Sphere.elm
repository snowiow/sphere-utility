module Sphere exposing (..)

import Point exposing (..)
import LatLng exposing (..)


latlngToPoint : LatLng -> Point
latlngToPoint latlng =
    let
        normalized =
            normalize latlng

        cosphi =
            cos normalized.lat
    in
        { x = cos normalized.lng |> (*) cosphi
        , y = sin normalized.lng |> (*) cosphi
        , z = normalized.lat |> sin
        }


latlngToPointOnModels : LatLng.Model -> Point.Model
latlngToPointOnModels =
    LatLng.modelToLatLng >> latlngToPoint >> Point.pointToModel


pointModelToLatLng : Point.Model -> LatLng.LatLng
pointModelToLatLng =
    Point.modelToPoint >> pointToLatLng


pointToLatLngOnModels : Point.Model -> LatLng.Model
pointToLatLngOnModels =
    pointModelToLatLng >> LatLng.latLngToModel


pointToLatLng : Point -> LatLng
pointToLatLng point =
    let
        qX =
            point.x * point.x

        qY =
            point.y * point.y
    in
        { lat = qX + qY |> sqrt |> atan2 point.z |> radToDeg
        , lng = atan2 point.y point.x |> radToDeg
        }
