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
import Random
import Array

------------- MODEL ------------

type alias Model = {
  tickCount: Int,
  text: String,
  baseNote: Int,
  currentWord: Maybe SingingWord,
  midiCommand: Maybe MidiCommand,
  phonemes: List (List String),
  phonemesToPlay: List (List String),
  randomSeed: Random.Seed
}

type alias SingingWord = {
  text: String,
  freq: Float
}

type alias MidiCommand = {
  note: Int
}

initialModel : Model
initialModel =
  {
    tickCount = 0,
    text = "I love you and you love me too",
    baseNote = 50,
    currentWord = Nothing,
    midiCommand = Nothing,
    phonemes = [],
    phonemesToPlay = [],
    randomSeed = Random.initialSeed 123
  }

simpleScale = Array.fromList [0, 2, 4, 7, 9]

scalePosToMidi : Int -> Array.Array Int -> Int
scalePosToMidi pos scale =
  let
    scaleLen = Array.length scale
    octave = pos // scaleLen
    remainder = pos % scaleLen
  in
    Array.get (remainder) scale
    |> Maybe.withDefault 0
    |> (+) (octave * 12)

pattern: Array.Array Int
pattern = Array.fromList([0, 7, 12, 7])


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
    Speak -> {model | phonemesToPlay <- model.phonemes, tickCount <- 0}
    otherwise -> model


tick : Model -> Model
tick model =
  let
    (randomInt,randomSeed') =
      Random.generate (Random.int 0 5) model.randomSeed

    midiNote =
      scalePosToMidi randomInt simpleScale + 50

    baseNote' =
        if model.tickCount % 8 == 0 then
          midiNote
        else
          model.baseNote

    newWord =
      if model.tickCount % 4 == 0 then
        case model.phonemesToPlay of
          [] -> (Nothing)
          list -> Just { text = nextWord list, freq = midiToFreq(midiNote)}
      else
        Nothing

    phonemesToPlay' =
          if model.tickCount % 4 == 0 then
            Maybe.withDefault [] (List.tail model.phonemesToPlay)
          else
            model.phonemesToPlay

    newMidiCommand =
      if List.isEmpty model.phonemesToPlay then
        Nothing
      else
        Just {note = baseNote' - 12 + (Array.get (model.tickCount % 4) pattern |> Maybe.withDefault 0 )}

  in
    { model |
        tickCount <- model.tickCount + 1,
        baseNote <- baseNote',
        midiCommand <-  newMidiCommand,
        currentWord <- newWord,
        phonemesToPlay <- phonemesToPlay',
        randomSeed <- randomSeed'
    }


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

tickSignal = Time.every (250 * Time.millisecond)
port ticksTask : Signal (Task x ())
port ticksTask =
  Signal.map (\time -> Signal.send actions.address Tick) tickSignal

------------- Interaction with Nori-Voice (Javascript) ------------

port speakPort : Signal (Maybe SingingWord)
port speakPort =
    Signal.map .currentWord model
    --Signal.sampleOn (Signal.filter isSpeakAction NoOp actions.signal) (Signal.map (.phonemes >> flatListListToString) model)

port midiPort : Signal (Maybe MidiCommand)
port midiPort =
    Signal.map .midiCommand model

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


nextWord : List (List String) -> String
nextWord phonemes =
  List.head phonemes
    |> Maybe.withDefault [""]
    |> List.map makeUSoundLong
    |> String.join ""


makeUSoundLong s =
  if String.startsWith "U" s then "4" ++ s else s


midiToFreq : Int -> Float
midiToFreq midiNote = (440.0 / 32.0) * (2.0 ^ (((toFloat midiNote) - 9.0) / 12.0))