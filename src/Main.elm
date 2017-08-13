module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Sphere exposing (..)
import Point exposing (..)
import LatLng exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col


-- MODEL


type alias Model =
    { point : Point.Model
    , latlng : LatLng.Model
    , d1 : Point.Model
    , d2 : Point.Model
    , dist : Float
    }


initModel : Model
initModel =
    { latlng = LatLng.init
    , point = Point.initModel
    , d1 = Point.initModel
    , d2 = Point.initModel
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
            { model | point = latlngToPointOnModels model.latlng }

        ConvertPoint ->
            { model | latlng = pointToLatLngOnModels model.point }

        LatLngMsg subMsg ->
            { model | latlng = LatLng.update subMsg model.latlng }

        PMsg subMsg ->
            { model | point = Point.update subMsg model.point }

        D1Msg subMsg ->
            { model | d1 = Point.update subMsg model.d1 }

        D2Msg subMsg ->
            { model | d2 = Point.update subMsg model.d2 }

        CalculateDistance ->
            { model | dist = Point.distOnModels model.d1 model.d2 }

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
        [ Grid.row []
            [ Grid.col [ Col.xs2 ] []
            , Grid.col [ Col.xs8 ]
                [ h2 [ class "title" ] [ text "Calculate distance between two points" ] ]
            , Grid.col [ Col.xs2 ] []
            ]
        , Grid.row []
            [ Grid.col [ Col.xs5 ]
                [ distancePoint1View model.d1 ]
            , Grid.col [ Col.xs5 ]
                [ distancePoint2View model.d2 ]
            , Grid.col [ Col.xs2 ]
                [ button
                    [ type_ "button"
                    , onClick CalculateDistance
                    , class "btn btn-primary"
                    ]
                    [ text "Calculate Distance" ]
                , label [] [ toString model.dist |> (++) "Distance: " |> text ]
                ]
            ]
        , Grid.row [] []
        ]


distancePoint1View : Point.Model -> Html Msg
distancePoint1View point =
    div []
        [ h5 [] [ text "Point D1" ]
        , Html.map D1Msg (Point.view point)
        ]


distancePoint2View : Point.Model -> Html Msg
distancePoint2View point =
    div []
        [ h5 [] [ text "Point D2" ]
        , Html.map D2Msg (Point.view point)
        ]


conversionView : Model -> Html Msg
conversionView model =
    div []
        [ Grid.row []
            [ Grid.col [ Col.xs2 ] []
            , Grid.col [ Col.xs8 ]
                [ h2 [ class "title" ] [ text "Convert Point to LatLng/ LatLng to Point" ] ]
            , Grid.col [ Col.xs2 ] []
            ]
        , Grid.row []
            [ latlngView model
            , Grid.col [ Col.xs2 ]
                []
            , pointView model
            ]
        ]


latlngView : Model -> Grid.Column Msg
latlngView model =
    Grid.col [ Col.xs5 ]
        [ h5 [] [ text "Latitude/Longitude in Degrees" ]
        , Html.map LatLngMsg (LatLng.view model.latlng)
        , Grid.row []
            [ Grid.col []
                [ button
                    [ type_ "button"
                    , onClick ConvertLatLng
                    , class "btn btn-primary"
                    ]
                    [ text "Convert to Point" ]
                ]
            ]
        ]


pointView : Model -> Grid.Column Msg
pointView model =
    Grid.col [ Col.xs5 ]
        [ h5 [] [ text "Point" ]
        , Html.map PMsg (Point.view model.point)
        , Grid.row []
            [ Grid.col []
                [ button
                    [ type_ "button"
                    , onClick ConvertPoint
                    , class "btn btn-primary"
                    ]
                    [ text "Convert to Latitude/Longitude" ]
                ]
            , Grid.col []
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
