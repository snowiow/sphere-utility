module FloatInput exposing (FloatInput, init, parse)


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
