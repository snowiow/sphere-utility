module Point exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, for, pattern)
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import FloatInput exposing (FloatInput)


-- MODEL


type alias Model =
    { x : FloatInput
    , y : FloatInput
    , z : FloatInput
    }


type alias Point =
    { x : Float
    , y : Float
    , z : Float
    }


init : Model
init =
    { x = FloatInput.init
    , y = FloatInput.init
    , z = FloatInput.init
    }


initPoint : Point
initPoint =
    { x = 0.0
    , y = 0.0
    , z = 0.0
    }


modelToPoint : Model -> Point
modelToPoint model =
    { x =
        case model.x.number of
            Just number ->
                number

            Nothing ->
                0
    , y =
        case model.y.number of
            Just number ->
                number

            Nothing ->
                0
    , z =
        case model.z.number of
            Just number ->
                number

            Nothing ->
                0
    }


pointToModel : Point -> Model
pointToModel point =
    { x =
        { number = Just point.x
        , input = toString point.x
        , err = ""
        }
    , y =
        { number = Just point.y
        , input = toString point.y
        , err = ""
        }
    , z =
        { number = Just point.z
        , input = toString point.z
        , err = ""
        }
    }


kRadiusOfEarthInMeters : Float
kRadiusOfEarthInMeters =
    6378.1 * 1000


distOnModels : Model -> Model -> Float
distOnModels mA mB =
    dist (modelToPoint mA) (modelToPoint mB)


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputX val ->
            { model | x = FloatInput.parse val }

        InputY val ->
            { model | y = FloatInput.parse val }

        InputZ val ->
            { model | z = FloatInput.parse val }


view : Model -> Html Msg
view model =
    Grid.row []
        [ Grid.col []
            [ Form.form []
                [ Form.group
                    (if String.isEmpty model.x.err then
                        []
                     else
                        [ Form.groupDanger ]
                    )
                    [ Form.label [ for "x" ] [ text "X" ]
                    , Input.text
                        [ Input.onInput InputX
                        , Input.value model.x.input
                        , Input.id "x"
                        ]
                    , Form.validationText [] [ text model.x.err ]
                    ]
                , Form.group
                    (if String.isEmpty model.y.err then
                        []
                     else
                        [ Form.groupDanger ]
                    )
                    [ Form.label [ for "y" ] [ text "Y" ]
                    , Input.text
                        [ Input.onInput InputY
                        , Input.value model.y.input
                        , Input.id "y"
                        ]
                    , Form.validationText [] [ text model.y.err ]
                    ]
                , Form.group
                    (if String.isEmpty model.z.err then
                        []
                     else
                        [ Form.groupDanger ]
                    )
                    [ Form.label [ for "z" ] [ text "Z" ]
                    , Input.text
                        [ Input.onInput InputZ
                        , Input.value model.z.input
                        , Input.id "z"
                        ]
                    , Form.validationText [] [ text model.z.err ]
                    ]
                ]
            ]
        ]
