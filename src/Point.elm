module Point exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import HtmlEvents exposing (..)


-- MODEL


type alias Point =
    { x : Float
    , y : Float
    , z : Float
    , err : String
    }


initPoint : Point
initPoint =
    { x = 0
    , y = 0
    , z = 0
    , err = ""
    }


kRadiusOfEarthInMeters : Float
kRadiusOfEarthInMeters =
    6378.1 * 1000


dist : Point -> Point -> Float
dist pA pB =
    let
        x =
            crossProd pA pB |> length

        y =
            dotProd pA pB
    in
        atan2 x y |> (*) kRadiusOfEarthInMeters


crossProd : Point -> Point -> Point
crossProd pA pB =
    { x = pA.y * pB.z - pA.z * pB.y
    , y = pA.z * pB.x - pA.x * pB.z
    , z = pA.x * pB.y - pA.y * pB.x
    , err = ""
    }


dotProd : Point -> Point -> Float
dotProd pA pB =
    pA.x * pB.x + pA.y * pB.y + pA.z * pB.z


length : Point -> Float
length point =
    let
        qX =
            point.x * point.x

        qY =
            point.y * point.y

        qZ =
            point.z * point.z
    in
        sqrt (qX + qY + qZ)



-- UPDATE


type Msg
    = InputX String
    | InputY String
    | InputZ String


update : Msg -> Point -> Point
update msg point =
    case msg of
        InputX val ->
            case String.toFloat val of
                Ok x ->
                    { point | x = x }

                Err msg ->
                    { point | err = msg }

        InputY val ->
            case String.toFloat val of
                Ok y ->
                    { point | y = y }

                Err msg ->
                    { point | err = msg }

        InputZ val ->
            case String.toFloat val of
                Ok z ->
                    { point | z = z }

                Err msg ->
                    { point | err = msg }


view : Point -> Html Msg
view point =
    div [ class "col-5" ]
        [ div [ class "form-group row" ]
            [ label [ for "x" ] [ text "X" ]
            , input
                [ type_ "text"
                , onBlurValue InputX
                , value (toString point.x)
                , class "form-control"
                , id "x"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "y" ] [ text "Y" ]
            , input
                [ type_ "text"
                , onBlurValue InputY
                , value (toString point.y)
                , class "form-control"
                , id "y"
                ]
                []
            ]
        , div [ class "form-group row" ]
            [ label [ for "z" ] [ text "Z" ]
            , input
                [ type_ "text"
                , onBlurValue InputZ
                , value (toString point.z)
                , class "form-control"
                , id "z"
                ]
                []
            ]
        ]
