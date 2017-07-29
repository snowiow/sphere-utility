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
    { point = initPoint
    , latlng = initLatLng
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
    | PMsg PointMsg
    | D1Msg PointMsg
    | D2Msg PointMsg


update : Msg -> Model -> Model
update msg model =
    case msg of
        ConvertLatLng ->
            { model
                | point = latlngToPoint model.latlng
            }

        ConvertPoint ->
            { model
                | latlng = pointToLatlng model.point
            }

        InputLat val ->
            case parseLat val of
                Ok lat ->
                    { model
                        | latlng = setLat lat model.latlng
                    }

                Err msg ->
                    { model
                        | err = msg
                    }

        InputLng val ->
            case parseLng val of
                Ok lng ->
                    { model
                        | latlng = setLng lng model.latlng
                    }

                Err msg ->
                    { model
                        | err = msg
                    }

        PMsg subMsg ->
            updateP subMsg model

        D1Msg subMsg ->
            updateD1 subMsg model

        D2Msg subMsg ->
            updateD2 subMsg model

        CalculateDistance ->
            { model
                | dist = Point.dist model.d1 model.d2
            }

        CopyD1 ->
            { model
                | d1 = model.point
            }

        CopyD2 ->
            { model
                | d2 = model.point
            }


updateP : PointMsg -> Model -> Model
updateP msg model =
    case msg of
        InputX val ->
            case String.toFloat val of
                Ok x ->
                    { model
                        | point = setX x model.point
                    }

                Err msg ->
                    { model
                        | err = msg
                    }

        InputY val ->
            case String.toFloat val of
                Ok y ->
                    { model
                        | point = setY y model.point
                    }

                Err msg ->
                    { model
                        | err = msg
                    }

        InputZ val ->
            case String.toFloat val of
                Ok z ->
                    { model
                        | point = setZ z model.point
                    }

                Err msg ->
                    { model
                        | err = msg
                    }


updateD1 : PointMsg -> Model -> Model
updateD1 msg model =
    case msg of
        InputX val ->
            case String.toFloat val of
                Ok x ->
                    { model
                        | d1 = setX x model.d1
                    }

                Err msg ->
                    { model
                        | err = msg
                    }

        InputY val ->
            case String.toFloat val of
                Ok y ->
                    { model
                        | d1 = setY y model.d1
                    }

                Err msg ->
                    { model
                        | err = msg
                    }

        InputZ val ->
            case String.toFloat val of
                Ok z ->
                    { model
                        | d1 = setZ z model.d1
                    }

                Err msg ->
                    { model
                        | err = msg
                    }


updateD2 : PointMsg -> Model -> Model
updateD2 msg model =
    case msg of
        InputX val ->
            case String.toFloat val of
                Ok x ->
                    { model
                        | d2 = setX x model.d2
                    }

                Err msg ->
                    { model
                        | err = msg
                    }

        InputY val ->
            case String.toFloat val of
                Ok y ->
                    { model
                        | d2 = setY y model.d2
                    }

                Err msg ->
                    { model
                        | err = msg
                    }

        InputZ val ->
            case String.toFloat val of
                Ok z ->
                    { model
                        | d2 = setZ z model.d2
                    }

                Err msg ->
                    { model
                        | err = msg
                    }



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
    div [ class "col-5" ]
        [ h5 [] [ text "Point D1" ]
        , div [ class "form-group row" ]
            [ label [ for "d1x" ] [ text "X" ]
            , input
                [ type_ "text"
                , onBlurValue (D1Msg << InputX)
                , value (toString point.x)
                , class "form-control"
                , id "d1x"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "d1y" ] [ text "Y" ]
            , input
                [ type_ "text"
                , onBlurValue (D1Msg << InputY)
                , value (toString point.y)
                , class "form-control"
                , id "d1y"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "d1z" ] [ text "Z" ]
            , input
                [ type_ "text"
                , onBlurValue (D1Msg << InputZ)
                , value (toString point.z)
                , class "form-control"
                , id "d1z"
                ]
                []
            ]
        ]


distancePoint2View : Point -> Html Msg
distancePoint2View point =
    div [ class "col-5" ]
        [ h5 [] [ text "Point D2" ]
        , div [ class "form-group row" ]
            [ label [ for "d2x" ] [ text "X" ]
            , input
                [ type_ "text"
                , onBlurValue (D2Msg << InputX)
                , value (toString point.x)
                , class "form-control"
                , id "d2x"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "d2y" ] [ text "Y" ]
            , input
                [ type_ "text"
                , onBlurValue (D2Msg << InputY)
                , value (toString point.y)
                , class "form-control"
                , id "d2y"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "d2z" ] [ text "Z" ]
            , input
                [ type_ "text"
                , onBlurValue (D2Msg << InputZ)
                , value (toString point.z)
                , class "form-control"
                , id "d2z"
                ]
                []
            ]
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
        [ h5 [] [ text "3D Point" ]
        , div [ class "form-group row" ]
            [ label [ for "px" ] [ text "X" ]
            , input
                [ type_ "text"
                , onBlurValue (PMsg << InputX)
                , value (toString model.point.x)
                , class "form-control"
                , id "px"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "py" ] [ text "Y" ]
            , input
                [ type_ "text"
                , onBlurValue (PMsg << InputY)
                , value (toString model.point.y)
                , class "form-control"
                , id "py"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "pz" ] [ text "Z" ]
            , input
                [ type_ "text"
                , onBlurValue (PMsg << InputZ)
                , value (toString model.point.z)
                , class "form-control"
                , id "pz"
                ]
                []
            ]
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
