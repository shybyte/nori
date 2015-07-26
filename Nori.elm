module Nori where

import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Debug
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)
import Time

------------- MODEL ------------

type alias Model = {
  text: String,
  currentWord: Maybe SingingWord,
  phonemes: List (List String),
  phonemesToPlay: List (List String)
}

type alias SingingWord = {
  text: String,
  freq: Float
}

initialModel : Model
initialModel =
  {
    text = "I love you",
    currentWord = Nothing,
    phonemes = [],
    phonemesToPlay = []
  }



------------- ACTIONS ------------

type Action =
  NoOp |
  Tick |
  Speak |
  SetPhonemes (List (List String)) |
  SetText String

update : Action -> Model -> Model
update action model =
  case action of
    SetText text -> {model | text <- text}
    SetPhonemes phonemes -> {model | phonemes <- phonemes}
    Tick -> tick model
    Speak -> {model | phonemesToPlay <- model.phonemes}
    otherwise -> model


tick : Model -> Model
tick model =
  let
    newWord =
      case model.phonemesToPlay of
        [] -> Nothing
        list -> Just { text = nextWord list, freq = 220}
  in
    { model |
        currentWord <- newWord,
        phonemesToPlay <- Maybe.withDefault [] (List.tail model.phonemesToPlay)
    }


nextWord : List (List String) -> String
nextWord phonemes =  String.join "" (Maybe.withDefault [""] (List.head phonemes))

------------- VIEWS ------------

view : Signal.Address Action -> Model -> Html
view address model =
  let field =
        input
          [ placeholder "English Words"
          , value model.text
          , on "input" targetValue (Signal.message address << SetText)
          , class "myStyle"
          ]
          []

      resultMessage =
          div [ class "myStyle" ] [  text (flatListListToString model.phonemes)]

      speakButton =
        button [onClick address Speak] [text "Speak"]

  in
      div [] [field, resultMessage, speakButton]



------------- Wiring ------------

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

main : Signal Html
main =
  Signal.map (view actions.address) model

model : Signal Model
model =
  Signal.foldp update initialModel (Signal.map (Debug.log "Action")  actions.signal)


------------- Timer ------------

tickSignal = Time.every Time.second
port ticksTask : Signal (Task x ())
port ticksTask =
  Signal.map (\time -> Signal.send actions.address Tick) tickSignal

------------- Interaction with Nori-Voice (Javascript) ------------

port speakPort : Signal (Maybe SingingWord)
port speakPort =
    Signal.map .currentWord model
    --Signal.sampleOn (Signal.filter isSpeakAction NoOp actions.signal) (Signal.map (.phonemes >> flatListListToString) model)



------------- Http Requests ------------

port requests : Signal (Task x ())
port requests =
  Signal.map lookupPhonemes (Signal.dropRepeats (Signal.map .text model))
    |> Signal.map (\task -> Task.toResult task `andThen` sendResult)

sendResult : (Result String (List (List String))) -> Task x ()
sendResult = resultToAction >> Signal.send actions.address

resultToAction : Result.Result String (List (List String)) -> Action
resultToAction result = SetPhonemes (Maybe.withDefault [["AY"]] (Result.toMaybe result))

lookupPhonemes : String -> Task String (List (List String))
lookupPhonemes query =
  let toUrl =
        if String.length query > 0
          then succeed ("http://localhost:3000/phonemes/" ++ query)
          else fail "Give me some english words!"
  in
        toUrl `andThen` (mapError (always "Not found :(") << Http.get arpabet)


arpabet : Json.Decoder (List (List String))
arpabet = Json.list (Json.list Json.string)



------------- UTILS ------------

flatListListToString : List (List String) -> String
flatListListToString list = String.join " " (List.map (String.join "") list)