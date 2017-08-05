module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Sphere exposing (..)
import Point exposing (..)
import LatLng exposing (..)


-- MODEL


type alias Model =
    { point : Point
    , latlng : LatLng
    , d1 : Point
    , d2 : Point
    , dist : Float
    }


initModel : Model
initModel =
    { latlng = initLatLng
    , point = initPoint
    , d1 = initPoint
    , d2 = initPoint
    , dist = 0
    }



-- UPDATE


type Msg
    = ConvertLatLng
    | ConvertPoint
    | CalculateDistance
    | CopyD1
    | CopyD2
    | LatLngMsg LatLng.Msg
    | PMsg Point.Msg
    | D1Msg Point.Msg
    | D2Msg Point.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        ConvertLatLng ->
            { model | point = latlngToPoint model.latlng }

        ConvertPoint ->
            { model | latlng = pointToLatlng model.point }

        LatLngMsg subMsg ->
            { model | latlng = LatLng.update subMsg model.latlng }

        PMsg subMsg ->
            { model | point = Point.update subMsg model.point }

        D1Msg subMsg ->
            { model | d1 = Point.update subMsg model.d1 }

        D2Msg subMsg ->
            { model | d2 = Point.update subMsg model.d2 }

        CalculateDistance ->
            { model | dist = Point.dist model.d1 model.d2 }

        CopyD1 ->
            { model | d1 = model.point }

        CopyD2 ->
            { model | d2 = model.point }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ conversionView model
        , distanceView model
        ]


distanceView : Model -> Html Msg
distanceView model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-2" ] []
            , div [ class "col-8" ]
                [ h2 [ class "title" ] [ text "Calculate distance between two points" ] ]
            , div [ class "col-2" ] []
            ]
        , div [ class "row" ]
            [ div [ class "col-5" ]
                [ distancePoint1View model.d1 ]
            , div [ class "col-5" ]
                [ distancePoint2View model.d2 ]
            , div [ class "col-1" ]
                [ button
                    [ type_ "button"
                    , onClick CalculateDistance
                    , class "btn btn-primary"
                    ]
                    [ text "Calculate Distance" ]
                , label [] [ toString model.dist |> (++) "Distance: " |> text ]
                ]
            ]
        ]


distancePoint1View : Point -> Html Msg
distancePoint1View point =
    div []
        [ h5 [] [ text "Point D1" ]
        , Html.map D1Msg (Point.view point)
        ]


distancePoint2View : Point -> Html Msg
distancePoint2View point =
    div []
        [ h5 [] [ text "Point D2" ]
        , Html.map D2Msg (Point.view point)
        ]


conversionView : Model -> Html Msg
conversionView model =
    div []
        [ div [ class "row" ]
            [ div [ class "col-2" ] []
            , div [ class "col-8" ]
                [ h2 [ class "title" ] [ text "Convert Point to LatLng/ LatLng to Point" ] ]
            , div [ class "col-2" ] []
            ]
        , div [ class "row" ]
            [ latlngView model
            , div [ class "col-2" ]
                []
            , pointView model
            ]
        ]


latlngView : Model -> Html Msg
latlngView model =
    div [ class "col-5" ]
        [ h5 [] [ text "Latitude/Longitude in Degrees" ]
        , Html.map LatLngMsg (LatLng.view model.latlng)
        , div [ class "form-group row" ]
            [ div [ class "col" ]
                [ button
                    [ type_ "button"
                    , onClick ConvertLatLng
                    , class "btn btn-primary"
                    ]
                    [ text "Convert to Point" ]
                ]
            ]
        ]


pointView : Model -> Html Msg
pointView model =
    div [ class "col-5" ]
        [ h5 [] [ text "Point" ]
        , Html.map PMsg (Point.view model.point)
        , div [ class "form-group row" ]
            [ div [ class "col" ]
                [ button
                    [ type_ "button"
                    , onClick ConvertPoint
                    , class "btn btn-primary"
                    ]
                    [ text "Convert to Latitude/Longitude" ]
                ]
            , div [ class "col" ]
                [ button
                    [ type_ "button"
                    , onClick CopyD1
                    , class "btn btn-primary margin"
                    ]
                    [ text "Copy point into D1" ]
                , button
                    [ type_ "button"
                    , onClick CopyD2
                    , class "btn btn-primary margin"
                    ]
                    [ text "Copy Point into D2" ]
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
