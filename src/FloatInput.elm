module FloatInput exposing (..)


type alias FloatInput =
    { number : Maybe Float
    , input : String
    , err : String
    }


init : FloatInput
init =
    { number = Nothing
    , input = ""
    , err = ""
    }


{-| parses a string into an new FloatInput
-}
parse : String -> FloatInput
parse input =
    case String.toFloat input of
        Ok number ->
            { number = (Just number)
            , input = input
            , err = ""
            }

        Err msg ->
            { number = Nothing
            , input = input
            , err = msg
            }


{-| parses a string into a more restricted FloatInput which applies to a Latitude
-}
parseLat : String -> FloatInput
parseLat input =
    case (parseVal input 90 "latitude") of
        Ok lat ->
            { number = (Just lat)
            , input = input
            , err = ""
            }

        Err msg ->
            { number = Nothing
            , input = input
            , err = msg
            }


{-| parses a string into a more restricted FloatInput which applies to a Longitude
-}
parseLng : String -> FloatInput
parseLng input =
    case (parseVal input 180 "longitude") of
        Ok lng ->
            { number = (Just lng)
            , input = input
            , err = ""
            }

        Err msg ->
            { number = Nothing
            , input = input
            , err = msg
            }


parseVal : String -> Float -> String -> Result String Float
parseVal str border name =
    case String.toFloat str of
        Ok float ->
            if float >= -border && float <= border then
                Ok float
            else
                Err
                    ("Illegal value for "
                        ++ name
                        ++ " given (only between -"
                        ++ (toString border)
                        ++ " to "
                        ++ (toString border)
                        ++ " allowed)"
                    )

        Err msg ->
            Err msg
