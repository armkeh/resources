module Decoder exposing (..)

import List exposing (map, filter)
import List.Extra exposing (unique)

import Regex as Regex

import Dropdown as Dropdown

import Csv.Decode as Decode exposing (Decoder)

type alias Answer =
  { student_num : Int
  , macid : String
  , firstname : String
  , lastname : String
  , attempt_num : Int
  , start_time : String
  , end_time : String
  , section_num : Maybe Int
  , question_num : Maybe Int
  , question_type : Maybe String
  , question_title : Maybe String
  , q_text : Maybe String
  , is_bonus : Maybe String
  , difficulty : Maybe Int
  , answer : Maybe String
  , answer_match : Maybe String
  , score : Maybe String
  , out_of : Maybe Int
  }

decoder : Decoder Answer
decoder =
  Decode.into Answer
    |> Decode.pipeline (Decode.field "Org Defined ID" (Decode.int))
    |> Decode.pipeline (Decode.field "Username"       (Decode.string))
    |> Decode.pipeline (Decode.field "FirstName"      (Decode.string))
    |> Decode.pipeline (Decode.field "LastName"       (Decode.string))
    |> Decode.pipeline (Decode.field "Attempt #"      (Decode.int))
    |> Decode.pipeline (Decode.field "Attempt Start"  (Decode.string))
    |> Decode.pipeline (Decode.field "Attempt End"    (Decode.string))
    |> Decode.pipeline (Decode.field "Section #"      (Decode.blank Decode.int))
    |> Decode.pipeline (Decode.field "Q #"            (Decode.blank Decode.int))
    |> Decode.pipeline (Decode.field "Q Type"         (Decode.blank Decode.string))
    |> Decode.pipeline (Decode.field "Q Title"        (Decode.blank Decode.string))
    |> Decode.pipeline (Decode.field "Q Text"         (Decode.blank Decode.string))
    |> Decode.pipeline (Decode.field "Bonus?"         (Decode.blank Decode.string))
    |> Decode.pipeline (Decode.field "Difficulty"     (Decode.blank Decode.int))
    |> Decode.pipeline (Decode.field "Answer"         (Decode.blank Decode.string))
    |> Decode.pipeline (Decode.field "Answer Match"   (Decode.blank Decode.string))
    |> Decode.pipeline (Decode.field "Score"          (Decode.blank Decode.string))
    |> Decode.pipeline (Decode.field "Out Of"         (Decode.blank Decode.int))

type alias QuestionID = (Int, Int)

qidToString : QuestionID -> String
qidToString (s, q) = "Section " ++ String.fromInt s ++ ", Question" ++ String.fromInt q

qid : (Maybe Int, Maybe Int) -> QuestionID
qid x = case x of
  (Nothing, Nothing) -> (-1, -1)
  (Nothing, Just j) -> (-1, j)
  (Just i, Nothing) -> (i, -1)
  (Just i, Just j) -> (i, j)

qids : List Answer -> List QuestionID
qids l = unique (map (\x -> qid (.section_num x, .question_num x)) l)

hasQid : QuestionID -> Answer -> Bool
hasQid (s, q) a = (if s == -1 then .section_num a == Nothing else .section_num a == Just s)
               && (if q == -1 then .question_num a == Nothing else .question_num a == Just q)

getQuestion : QuestionID -> List Answer -> List Answer
getQuestion q l = filter (hasQid q) l

rebreak : String -> String
rebreak s =
  let maybeRegex = Regex.fromString "\\s\\s+" in
  let regex = Maybe.withDefault Regex.never maybeRegex in
  Regex.replace regex (.match >> (\x -> "\n" ++ x)) s
