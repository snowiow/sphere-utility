module DistancePage exposing (Model, init, Msg, update, view)

import Point exposing (..)
import LatLng exposing (..)
import Sphere exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup


type alias Model =
    { point : Point.Model
    , latlng : LatLng.Model
    , d1 : Point.Model
    , d2 : Point.Model
    , dist : Float
    }


init : Model
init =
    { latlng = LatLng.init
    , point = Point.init
    , d1 = Point.init
    , d2 = Point.init
    , dist = 0
    }



-- Update


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



-- View


view : Model -> List (Html Msg)
view model =
    [ conversionView model
    , br [] []
    , distanceView model
    ]


conversionView : Model -> Html Msg
conversionView model =
    div []
        [ Grid.row []
            [ Grid.col [ Col.xs8 ]
                [ h2 [ class "title" ] [ text "Convert Point to LatLng/ LatLng to Point" ] ]
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
                [ Button.button
                    [ Button.primary
                    , Button.onClick ConvertLatLng
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
                [ ButtonGroup.buttonGroup [ ButtonGroup.vertical ]
                    [ ButtonGroup.button
                        [ Button.primary
                        , Button.onClick ConvertPoint
                        ]
                        [ text "Convert to Latitude/Longitude" ]
                    , ButtonGroup.button
                        [ Button.primary
                        , Button.onClick CopyD1
                        ]
                        [ text "Copy point into D1" ]
                    , ButtonGroup.button
                        [ Button.primary
                        , Button.onClick CopyD2
                        ]
                        [ text "Copy Point into D2" ]
                    ]
                ]
            ]
        ]


distanceView : Model -> Html Msg
distanceView model =
    div []
        [ Grid.row []
            [ Grid.col []
                [ h2 [ class "title" ] [ text "Calculate distance between two points" ] ]
            ]
        , Grid.row []
            [ Grid.col [ Col.xs5 ]
                [ distancePoint1View model.d1 ]
            , Grid.col [ Col.xs5 ]
                [ distancePoint2View model.d2 ]
            , Grid.col [ Col.xs1 ]
                [ Button.button
                    [ Button.primary
                    , Button.onClick CalculateDistance
                    ]
                    [ text "Calculate Distance" ]
                , label [] [ toString model.dist |> (++) "Distance: " |> text ]
                ]
            ]
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
