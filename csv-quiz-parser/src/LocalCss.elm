module LocalCss exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)

backgroundDiv : List (Attribute msg) -> List (Html msg) -> Html msg
backgroundDiv =
  styled Html.Styled.div
    [ 
    ]

menuDiv : List (Attribute msg) -> List (Html msg) -> Html msg
menuDiv =
  styled Html.Styled.div
    [ backgroundColor (hex "#eee")
    , marginLeft (rem 5), marginRight (rem 5)
    , color (hex "#999")
    , displayFlex
    ]

menuGapDiv : List (Attribute msg) -> List (Html msg) -> Html msg
menuGapDiv =
  styled Html.Styled.div
    [ flexGrow (num 1)
    ]

answerDiv : List (Attribute msg) -> List (Html msg) -> Html msg
answerDiv =
  styled Html.Styled.div
    [ backgroundColor (hex "#fff")
    , marginLeft (rem 5), marginRight (rem 5)
    , Css.maxHeight (vh 100)
    , overflow scroll
    ]

csvDiv : List (Attribute msg) -> List (Html msg) -> Html msg
csvDiv =
  styled Html.Styled.div
    [ backgroundColor (hex "#ddd")
    , marginLeft (rem 5), marginRight (rem 5)
    , Css.maxHeight (vh 30)
    , overflow scroll
    ]

loadButton : List (Attribute msg) -> List (Html msg) -> Html msg
loadButton =
  styled Html.Styled.button
    [
    ]

navButton : List (Attribute msg) -> List (Html msg) -> Html msg
navButton =
  styled Html.Styled.button
    [
    ]
