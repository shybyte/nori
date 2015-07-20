import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)

port setText : Signal (String)

-- VIEW

view : String -> Result String (String) -> Html
view string result =
  let field =
        input
          [ placeholder "English Words"
          , value string
          , on "input" targetValue (Signal.message query.address)
          , myStyle
          ]
          []

      resultMessage =
        case result of
          Err msg ->
              div [ myStyle ] [ text msg ]

          Ok cities ->
              div [ myStyle ] [  text cities]
  in
      div [] (field :: [resultMessage])


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

-- WIRING

main =
  Signal.map2 view query.signal results.signal


query : Signal.Mailbox String
query =
  Signal.mailbox "I love you"


results : Signal.Mailbox (Result String (String))
results =
  Signal.mailbox (Err "Is this a english word?")


port requests : Signal (Task x ())
port requests =
  Signal.map lookupZipCode query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)


lookupZipCode : String -> Task String (String)
lookupZipCode query =
  let toUrl =
        if String.length query > 0
          then succeed ("http://localhost:3000/phonemes/" ++ query)
          else fail "Give me some english words!"
  in
      toUrl `andThen` (mapError (always "Not found :(") << Http.getString)




--arpabet = Json.list Json.string
--arpabet : Json.Decoder (String)
--arpabet = Json.string