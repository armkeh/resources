module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html.Styled exposing (Html, button, p, pre, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (style)
import Html.Styled.Events exposing (onClick)
import Task

import List exposing (filter)

import Dropdown as Dropdown

import Csv.Decode as Decode exposing (Decoder)
import Decoder exposing (..)

import LocalCss exposing (..)

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view >> toUnstyled
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { rawContents : Maybe String
  , answers : Answers
  , questions : List Dropdown.Item
  , text : String
  }

type alias Answers = (List Answer, List Answer)

emptyAnswers : Answers
emptyAnswers = ([] , [])

initAnswers : List Answer -> Answers
initAnswers l = ([] , l)

head : Answers -> Maybe Answer
head ans = case ans of
  (_ , x :: _) -> Just x
  (_ , []) -> Nothing

next : Answers -> Maybe Answers
next ans = case ans of
  (ys , x :: xs) -> Just (x :: ys, xs)
  (_ , []) -> Nothing

prev : Answers -> Maybe Answers
prev ans = case ans of
  (x :: xs , ys) -> Just (xs, x :: ys)
  ([] , _) -> Nothing

init : () -> (Model, Cmd Msg)
init _ =
  ( { rawContents = Nothing
    , answers = emptyAnswers
    , questions = []
    , text = "Please load a CSV file."
    }
  , Cmd.none )

view : Model -> Html Msg
view model =
  backgroundDiv []
    [ menuDiv []
        [ loadButton [ onClick CsvRequested ] [ text "Load quiz CSV" ]
        , loadButton [ onClick CsvUnloaded ] [ text "Unload CSV" ]
        , menuGapDiv [] []
        -- , 
        , menuGapDiv [] []
        , navButton [ onClick PrevAnswer ] [ text "Previous submission" ]
        , navButton [ onClick NextAnswer ] [ text "Next submission" ]
        ]
    , answerDiv [] [ pre [] [ text model.text ] ]
    , (case model.rawContents of
         Nothing -> p [] []  
         Just content ->
           csvDiv [] [ pre [] [ text content ] ]
      )
    ]

type Msg
  = CsvRequested
  | CsvSelected File
  | CsvLoaded String
  | CsvUnloaded
  | PrevAnswer
  | NextAnswer

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CsvRequested ->
      ( model
      , Select.file ["text/csv"] CsvSelected
      )

    CsvSelected file ->
      ( model
      , Task.perform CsvLoaded (File.toString file)
      )

    CsvLoaded contents ->
      let decoded = Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder contents in
      ( { model | rawContents = Just contents
                , answers = case decoded of
                              Ok list -> initAnswers (filter (\x -> .question_num x == Just 4 || .question_num x == Just 5 || .question_num x == Just 6) list)
                              Err _ -> emptyAnswers
                --, questions = ...
                , text = case decoded of
                           Ok list -> case head (initAnswers (filter (\x -> .question_num x == Just 4 || .question_num x == Just 5 || .question_num x == Just 6) list)) of
                                        Nothing -> "No questions found."
                                        Just x -> .firstname x ++ ", " ++ .lastname x ++ "\n\n"
                                               ++ (case .q_text x of
                                                     Nothing -> "This question has no question text."
                                                     Just t -> t)
                                               ++ "\n\n\n"
                                               ++ case .answer_match x of
                                                    Nothing -> "This question has no answer."
                                                    Just t -> rebreak t
                           Err err -> "Encountered an error when parsing the CSV: " ++ Decode.errorToString err
        }
      , Cmd.none
      )
      
    CsvUnloaded ->
      ( { model | rawContents = Nothing
                , answers = emptyAnswers
                , text = "Please load another CSV file." }
      , Cmd.none
      )

    PrevAnswer ->
      case prev (.answers model) of
        Nothing -> ( { model | text = "There is no previous submission!" }
                , Cmd.none
                )
        Just ans -> ( { model | answers = ans
                              , text = case head ans of
                                         Nothing -> "There should be a previous submission, but I lost it. This should never occur, you have somehow found a bad state."
                                         Just x -> .firstname x ++ ", " ++ .lastname x ++ "\n\n"
                                                ++ (case .q_text x of
                                                      Nothing -> "This question has no question text."
                                                      Just t -> t)
                                                ++ "\n\n\n"
                                                ++ case .answer_match x of
                                                     Nothing -> "This question has no answer."
                                                     Just t -> rebreak t
                      }
                    , Cmd.none
                    )

    NextAnswer ->
      case next (.answers model) of
        Nothing -> ( { model | text = "There is no next submission!" }
                , Cmd.none
                )
        Just ans -> ( { model | answers = ans
                              , text = case head ans of
                                         Nothing -> "There should be a next submission, but I lost it. This should never occur, you have somehow found a bad state."
                                         Just x -> .firstname x ++ ", " ++ .lastname x ++ "\n\n"
                                                ++ (case .q_text x of
                                                      Nothing -> "This question has no question text."
                                                      Just t -> t)
                                                ++ "\n\n\n"
                                                ++ case .answer_match x of
                                                     Nothing -> "This question has no answer."
                                                     Just t -> rebreak t
                      }
                    , Cmd.none
                    )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
