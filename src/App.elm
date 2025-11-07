module App exposing (main)

import Browser
import Array exposing (Array)
import Html exposing (Html, a, div, footer, h1, p, span, text, textarea)
import Html.Attributes exposing (class, href, placeholder, target, value)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { text1 : String
    , text2 : String
    , result : Int
    }


init : Model
init =
    Model "" "" 0

leven : String -> String -> Int
leven s1 s2 =
    let
        a1 = Array.fromList (String.toList s1)
        a2 = Array.fromList (String.toList s2)
        m = Array.length a1
        n = Array.length a2
    in
    if m == 0 then
        n
    else if n == 0 then
        m
    else
        let
            process c1 i prev =
                let
                    indices = List.range 0 (n - 1)

                    calc j ( curr, left, diag ) =
                        let
                            c2 = Maybe.withDefault ' ' (Array.get j a2)
                            top = Maybe.withDefault 0 (Array.get (j + 1) prev)
                            val = if c1 == c2 then diag else 1 + min left (min top diag)
                        in
                        ( Array.set (j + 1) val curr, val, top )

                    ( row, _, _ ) =
                        List.foldl calc ( Array.set 0 (i + 1) (Array.repeat (n + 1) 0), i + 1, i ) indices
                in
                row

            ( _, result ) =
                Array.foldl (\c ( i, prev ) -> ( i + 1, process c i prev )) ( 0, Array.initialize (n + 1) identity ) a1
        in
        Maybe.withDefault 0 (Array.get n result)

type Msg
    = Text1 String
    | Text2 String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Text1 s ->
            { model | text1 = s, result = leven s model.text2 }

        Text2 s ->
            { model | text2 = s, result = leven model.text1 s }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Levenshtein Distance" ]
        , p [ class "subtitle" ] [ text "Calculate the edit distance between two strings" ]
        , div [ class "element" ]
            [ div [ class "box1" ] [ textarea [ value model.text1, onInput Text1, placeholder "Enter first text..." ] [] ]
            , div [ class "box2" ] [ textarea [ value model.text2, onInput Text2, placeholder "Enter second text..." ] [] ]
            ]
        , div [ class "result-container" ]
            [ div [ class "result-label" ] [ text "Edit Distance" ]
            , div [ class "result-value" ] [ text (String.fromInt model.result) ]
            ]
        , footer [ class "footer" ]
            [ text "made by "
            , a [ href "https://github.com/takumi34", target "_blank", class "github-link" ] [ text "takumi34" ]
            ]
        ]