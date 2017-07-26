module HtmlEvents exposing (onBlurValue)

import Json.Decode as Json
import Html.Events exposing (..)
import Html exposing (..)


onBlurValue : (String -> msg) -> Attribute msg
onBlurValue tagger =
    on "blur" (Json.map tagger targetValue)
