module Point exposing (..)

-- MODEL


type alias Point =
    { x : Float
    , y : Float
    , z : Float
    }


initPoint : Point
initPoint =
    { x = 0
    , y = 0
    , z = 0
    }


kRadiusOfEarthInMeters : Float
kRadiusOfEarthInMeters =
    6378.1 * 1000


setX : Float -> Point -> Point
setX x model =
    { model
        | x = x
    }


setY : Float -> Point -> Point
setY y model =
    { model
        | y = y
    }


setZ : Float -> Point -> Point
setZ z model =
    { model
        | z = z
    }


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


type PointMsg
    = InputX String
    | InputY String
    | InputZ String
