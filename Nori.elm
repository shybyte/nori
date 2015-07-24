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

------------- MODEL ------------

type alias Model = {
  text: String,
  phonemes: List (List String)
}

initialModel : Model
initialModel =
  {
    text = "I love you",
    phonemes = [["AY"],["L", "AH", "V"], ["Y", "UW"]]
  }



------------- ACTIONS ------------

type Action =
  NoOp |
  Speak |
  SetPhonemes (List (List String)) |
  SetText String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Speak -> model
    SetText text -> {model | text <- text}
    SetPhonemes phonemes -> {model | phonemes <- phonemes}

isSpeakAction : Action -> Bool
isSpeakAction action =
 case action of
  Speak -> True
  _ -> False

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



------------- Interaction with Nori-Voice (Javascript) ------------

port speakPort : Signal String
port speakPort =
    Signal.sampleOn (Signal.filter isSpeakAction NoOp actions.signal) (Signal.map (.phonemes >> flatListListToString) model)



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