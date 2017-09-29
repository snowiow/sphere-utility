port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid.Col as Col
import UrlParser exposing ((</>))
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Navigation exposing (Location)
import DistancePage
import LatLng


port setPoint : LatLng.LatLng -> Cmd msg



-- MODEL


type alias Model =
    { navState : Navbar.State
    , page : Page
    , distancePage : DistancePage.Model
    , point : LatLng.Model
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate location
                { navState = navState
                , page = Distance
                , distancePage = DistancePage.init
                , point = LatLng.init
                }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )



-- UPDATE


type Msg
    = NavMsg Navbar.State
    | UrlChange Location
    | DistancePage DistancePage.Msg
    | LatLngMsg LatLng.Msg
    | ShowPoint


type Page
    = Distance
    | Maps
    | NotFound


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavMsg state ->
            ( { model | navState = state }, Cmd.none )

        UrlChange location ->
            urlUpdate location model

        DistancePage subMsg ->
            ( { model
                | distancePage =
                    DistancePage.update subMsg model.distancePage
              }
            , Cmd.none
            )

        LatLngMsg subMsg ->
            ( { model | point = LatLng.update subMsg model.point }, Cmd.none )

        ShowPoint ->
            ( model, setPoint (LatLng.modelToLatLng model.point) )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    case decode location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Location -> Maybe Page
decode location =
    UrlParser.parseHash routeParser location


routeParser : UrlParser.Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Distance (UrlParser.top)
        , UrlParser.map Maps (UrlParser.s "maps")
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ navView model
        , mainContent model
        ]


navView : Model -> Html Msg
navView model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand
            [ href "#" ]
            [ img
                [ src "assets/favicon.ico"
                , class "d-inline-block align-top"
                , style [ ( "width", "30px" ) ]
                ]
                []
            , text "Sphere Utilities"
            ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#" ] [ text "Distance" ]
            , Navbar.itemLink [ href "#maps" ] [ text "Maps" ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Distance ->
                List.map (Html.map DistancePage) (DistancePage.view model.distancePage)

            Maps ->
                viewMaps model

            NotFound ->
                pageNotFound


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry this page couldn't be found"
    ]


viewMaps : Model -> List (Html Msg)
viewMaps model =
    [ div []
        [ Grid.row []
            [ Grid.col [ Col.xs8 ]
                [ div [ id "map" ] []
                ]
            , Grid.col [ Col.xs4 ]
                [ latLngViewMaps model ]
            ]
        ]
    ]


latLngViewMaps : Model -> Html Msg
latLngViewMaps model =
    div []
        [ Grid.row []
            [ Grid.col [ Col.xs5 ]
                [ h5 [] [ text "Add Latitude/Longitude Value" ]
                , Html.map LatLngMsg (LatLng.view model.point)
                , Grid.row []
                    [ Grid.col []
                        [ Button.button
                            [ Button.primary
                            , Button.onClick ShowPoint
                            ]
                            [ text "Show Point" ]
                        ]
                    ]
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
