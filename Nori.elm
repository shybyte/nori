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
import Random.Array exposing (shuffle)
import Array

------------- MODEL ------------

type alias Model = {
  text: String,
  seed: String,

  tickCount: Int,
  track: Track,
  midiCommands: List MidiCommand,
  phonemes: PhonemeLyrics
}


type alias Track = List Channel

type alias Channel = {
  midiChannel: Int,
  notes: Array.Array (List Note)
}

type alias Note = {
  pitch: MidiNote,
  phonemes: PhonemeWord
}

type alias MidiNote = Int


generateTrack : PhonemeLyrics -> Int -> Track
generateTrack phonemeLyrics seed =
  let
    lineNumber = List.length phonemeLyrics
    randomSeed = Random.initialSeed seed
    intGenerator = Random.int 0 12

    (chords, seed2) = randomChords randomSeed


    pianoChannel = {
      midiChannel = 0,
      notes =
        [0..lineNumber-1]
        |> List.map
          (\ln ->
            generateLine (38 + (Array.get (ln % 4) chords |> Maybe.withDefault 0)) 16
            |> List.map simpleNote
          )
        |> List.concat
        |> Array.fromList
    }

    celloChannel = {
      midiChannel = 1,
      notes =
        [0..lineNumber-1]
        |> List.map
          (\ln ->
            generateCelloLine (38 + (Array.get (ln % 4) chords |> Maybe.withDefault 0)) 16
          )
        |> List.concat
        |> Array.fromList
    }

    voiceChannel : Channel
    voiceChannel = {
      midiChannel = 16,
      notes =
        mapRandom generateVoiceLine seed2 phonemeLyrics
        |> snd
        |> List.concat
        |> Array.fromList
    }
  in
    [pianoChannel, celloChannel, voiceChannel]

majorScaleChords = Array.fromList [0, 2, 4, 5, 7, 8]

randomChords: Random.Seed -> (Array.Array Int, Random.Seed)
randomChords seed =
  let
    (shuffledArray, seed') = shuffle seed majorScaleChords
  in
    (Array.slice 0 4 shuffledArray, seed')


mapRandom : (Random.Seed -> a -> (Random.Seed, b)) -> Random.Seed -> List a -> (Random.Seed, List b)
--mapRandom : (a -> b) -> List a -> List b
--mapRandom f randomSeed aList = List.foldr (\aEl list -> f aEl :: [] ) [] aList
mapRandom f randomSeed aList =
  let
    initState = (randomSeed, [])
    foldF el state =
      let
        (randomSeed, bList) = state
        (randomSeed', b) = f randomSeed el
      in
        (randomSeed', b :: bList)

  in
    List.foldr foldF initState aList




generateVoiceLine : Random.Seed ->  PhonemeLine -> (Random.Seed, List (List Note))
generateVoiceLine randomSeed phonemeLine =
  let
    intGenerator = Random.int 0 8
    listIntGenerator = Random.list 4 intGenerator
    (randomList, randomSeed') = Random.generate listIntGenerator randomSeed
    phonemeLineArray = Array.fromList phonemeLine
  in
    List.map2
      ( \measure randomInt ->
        [
          simpleVoiceNote (scalePosToMidi randomInt simpleScale + 50) (Array.get measure phonemeLineArray |> Maybe.withDefault []),
          [],
          [],
          []
        ]
      )
      [0..4-1]
      randomList
    |> List.concat
    |> (\result -> (randomSeed', result))

generateLine : MidiNote -> Int -> List MidiNote
generateLine baseNote length =
  List.repeat (length // Array.length pattern) (Array.toList pattern)
  |> List.concat
  |> List.map ((+) baseNote)

generateCelloLine : MidiNote -> Int -> List (List Note)
generateCelloLine baseNote length =
  [
    [[{pitch=baseNote, phonemes=[]}]],
    List.repeat (length//4-1) [],
    [[{pitch= baseNote+7, phonemes=[]}]],
    List.repeat (length//4-1) [],
    [[{pitch=baseNote+12, phonemes=[]}]],
    List.repeat (length//4-1) [],
    [[{pitch= baseNote+7, phonemes=[]}]],
    List.repeat (length//4-1) []
  ]
  |> List.concat

simpleNote midiNote = [{pitch= midiNote, phonemes=[]}]
simpleVoiceNote midiNote phonemes= [{pitch= midiNote, phonemes=phonemes}]

type alias PhonemeLyrics = List PhonemeLine
type alias PhonemeLine = List PhonemeWord
type alias PhonemeWord = List Phoneme
type alias Phoneme = String

type alias SingingWord = {
  text: String,
  freq: Float
}

type alias MidiCommand = {
  channel: Int,
  note: Int,
  phonemes: String
}

textBla = """Wise men say
fools rush in
you will stay
joy and sin
"""

initialModel : Model
initialModel =
  {
    tickCount = 0,
    text = """Wise men say
fools rush in
you will stay
joy and sin
""",
    seed = "123",
    track = [],
    midiCommands = [],
    phonemes = []
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
  SetPhonemes PhonemeLyrics |
  SetText String |
  SetSeed String

update : Action -> Model -> Model
update action model =
  case action of
    SetText text -> {model | text <- text}
    SetSeed seed -> {model | seed <- seed}
    SetPhonemes phonemes -> { model |
      phonemes <- phonemes
    }
    Tick -> tick model
    Speak -> {model |
      track <- Debug.log "Track: " (generateTrack model.phonemes (parseInt 24 model.seed)),
      tickCount <- 0
    }
    otherwise -> model


tick : Model -> Model
tick model =
  let
    newMidiCommands: List MidiCommand
    newMidiCommands =
      midiCommandsFromTrackPosition model.track model.tickCount

  in
    { model |
        tickCount <- model.tickCount + 1,
        midiCommands <-  newMidiCommands
    }


midiCommandsFromTrackPosition: Track -> Int -> List MidiCommand
midiCommandsFromTrackPosition track pos =
  List.map
    ( \channel ->
        channel.notes
        |> Array.get pos
        |> Maybe.withDefault []
        |> List.map (simpleMidiCommand channel.midiChannel)
    )
    track
  |> List.concat

simpleMidiCommand : Int -> Note -> MidiCommand
simpleMidiCommand midiChannel note =
  {
    note = note.pitch,
    channel = midiChannel,
    phonemes = String.join "" note.phonemes
  }


------------- VIEWS ------------

view : Signal.Address Action -> Model -> Html
view address model =
  let
      speakButton =
        button [onClick address Speak] [text "Speak"]

      seedField =
        input
          [ placeholder "Random Seed"
          , value model.seed
          , on "input" targetValue (Signal.message address << SetSeed)
          ]
          []

      field =
        textarea
          [ placeholder "English Words"
          , value model.text
          , on "input" targetValue (Signal.message address << SetText)
          , class "textInput"
          , rows 5
          ]
          []

      resultMessage =
          div []
            (model.phonemes |> List.map
              (\line -> div [ class "myStyle" ] [  text (flatListListToString line)]))
  in
      div [] [seedField, speakButton, field, resultMessage]



------------- Wiring ------------

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

main : Signal Html
main =
  Signal.map (view actions.address) model

model : Signal Model
model =
  Signal.foldp update initialModel  actions.signal
  -- Signal.foldp update initialModel (Signal.map (Debug.log "Action")  actions.signal)


------------- Timer ------------

tickSignal = Time.every (250 * Time.millisecond)
port ticksTask : Signal (Task x ())
port ticksTask =
  Signal.map (\time -> Signal.send actions.address Tick) tickSignal

------------- Interaction with Nori-Voice (Javascript) ------------

port midiPort : Signal (List MidiCommand)
port midiPort =
    Signal.map .midiCommands model

------------- Http Requests ------------

port requests : Signal (Task x ())
port requests =
  Signal.map lookupPhonemes (Signal.dropRepeats (Signal.map .text model))
    |> Signal.map (\task -> Task.toResult task `andThen` sendResult)

sendResult : (Result String (PhonemeLyrics)) -> Task x ()
sendResult = resultToAction >> Signal.send actions.address

resultToAction : Result.Result String (PhonemeLyrics) -> Action
resultToAction result = SetPhonemes (Maybe.withDefault [[["AY"]]] (Result.toMaybe result))

lookupPhonemes : String -> Task String (PhonemeLyrics)
lookupPhonemes query =
  let
    toUrl =
        if String.length query > 0
          then succeed ("http://localhost:3000/phonemes/" ++ Http.uriEncode query)
          else fail "Give me some english words!"
    debug = Debug.log "query" query

  in
        toUrl `andThen` (mapError (always "Not found :(") << Http.get arpabet)


arpabet : Json.Decoder (PhonemeLyrics)
arpabet = Json.list (Json.list (Json.list Json.string))



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


parseInt: Int -> String -> Int
parseInt defaultInt s=
  String.toInt s
  |> getResultValue defaultInt

getResultValue: a -> Result x a -> a
getResultValue defaultValue result=
  Result.toMaybe result
  |> Maybe.withDefault defaultValue