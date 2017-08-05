module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Sphere exposing (..)
import HtmlEvents exposing (..)
import Point exposing (..)
import LatLng exposing (..)


-- MODEL


type alias Model =
    { point : Point
    , latlng : LatLng
    , d1 : Point
    , d2 : Point
    , dist : Float
    , err : String
    }


type alias FormInput =
    { id : String
    , text : String
    , msg : String -> Msg
    , val : Float
    }


initModel : Model
initModel =
    { latlng = initLatLng
    , point = initPoint
    , d1 = initPoint
    , d2 = initPoint
    , dist = 0
    , err = ""
    }



-- UPDATE


type Msg
    = ConvertLatLng
    | ConvertPoint
    | InputLat String
    | InputLng String
    | CalculateDistance
    | CopyD1
    | CopyD2
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

        InputLat val ->
            case parseLat val of
                Ok lat ->
                    { model | latlng = setLat lat model.latlng }

                Err msg ->
                    { model | err = msg }

        InputLng val ->
            case parseLng val of
                Ok lng ->
                    { model | latlng = setLng lng model.latlng }

                Err msg ->
                    { model | err = msg }

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
                [ h2 [ class "title" ] [ text "Convert Point to LatLng/ LatLng to Point" ]
                , h5 [ class "error" ] [ text model.err ]
                ]
            , div [ class "col-2" ] []
            ]
        , div [ class "row" ]
            [ latlngView model
            , div [ class "col-2" ]
                []
            , pointView model
            ]
        ]


formGroup : FormInput -> Html Msg
formGroup formInput =
    div [ class "form-group row" ]
        [ label [ for formInput.id ] [ text formInput.text ]
        , input
            [ type_ "text"
            , onBlurValue formInput.msg
            , value (toString formInput.val)
            , class "form-control"
            , id "lat"
            ]
            []
        ]


latlngView : Model -> Html Msg
latlngView model =
    div [ class "col-5" ]
        [ h5 [] [ text "Latitude/Longitude in Degrees" ]
        , formGroup (FormInput "lat" "Latitude" InputLat model.latlng.lat)
        , formGroup (FormInput "lng" "Longitude" InputLng model.latlng.lng)
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
