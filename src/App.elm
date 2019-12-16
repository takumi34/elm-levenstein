module App exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (..)
import List exposing (..)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { text1 : String
  , text2 : String
  , result : Int
  }


init : Model
init =
  Model "" "" 0

-- Levenstein
leven: String -> String -> Int
leven s1 s2 =
    if String.length s1 == 0 then
            String.length s2
    else if String.length s2 == 0 then
            String.length s1
    else 
          if right 1 s1 == right 1 s2 then
            leven (dropRight 1 s1) (dropRight 1 s2) -- 右端の文字が同じなら切り捨て
          else
            Maybe.withDefault 0
            (List.minimum [leven (dropRight 1 s1) s2,
                  leven s1 (dropRight 1 s2),
                  leven (dropRight 1 s1) (dropRight 1 s2)]) + 1

-- UPDATE


type Msg
  = Text1 String
  | Text2 String

update msg model =
  case msg of
    Text1 text1 ->
      { model | text1 = text1, result = leven model.text1 model.text2  }
    Text2 text2 ->
      { model | text2 = text2}


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ value model.text1, onInput Text1 ] []
    , input [ value model.text2, onInput Text2 ] []
    , div [] [ text (String.fromInt (leven model.text1 model.text2))]
    ]