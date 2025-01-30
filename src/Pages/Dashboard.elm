module Pages.Dashboard exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id, href)
import Html.Events exposing (onClick)
import Time
import Random
import Flags exposing (Flags)
-- init
type alias Model = 
  { secondsElapsed : Int
  , isRunning : Bool
  , roll : Maybe Int
  }

init : Flags -> ( Model, Cmd Msg )
init flags = 
    ( { secondsElapsed = 0
      , isRunning = False
      , roll = Nothing
      } 
    , Random.generate
        GeneratedNumber
        d6Generator
    )

d6Generator : Random.Generator Int
d6Generator = 
    Random.int 1 6

-- update
type Msg
    = TimerWentOff Time.Posix
    | ClickedStop
    | ClickedStart
    | ClickedReset
    | GeneratedNumber Int
    | ClickedReroll

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        TimerWentOff _ ->
            ( { model | secondsElapsed = model.secondsElapsed + 1 }
            , Cmd.none
            )

        ClickedStop ->
            ( { model | isRunning = False }
            , Cmd.none
            )

        ClickedReset ->
            ( { model | secondsElapsed = 0, isRunning = False }
            , Cmd.none
            )

        ClickedStart ->
            ( { model | isRunning = True }
            , Cmd.none
            )

        GeneratedNumber rolledValue ->
            ( { model | roll = Just rolledValue }
            , Cmd.none
            )

        ClickedReroll ->
            ( model
            , Random.generate GeneratedNumber d6Generator
            )


-- view
view : Model -> Browser.Document Msg
view model =
    { title = String.fromInt model.secondsElapsed
    , body = [

    div []
    [ p [] [ text ( "Timer: " ++ String.fromInt model.secondsElapsed) ]
    , if model.isRunning then 
        button [ onClick ClickedStop ] [ text "Stop" ]
      else 
        button [ onClick ClickedStart ] [ text "Start" ]
    -- , button [ onClick Decrement ] [ text "Minus" ]
    , button [ onClick ClickedReset ] [ text "Reset" ]
    , p [] [ text (Debug.toString model.roll) ]
    , button [ onClick ClickedReroll ] [ text "Reroll" ]
    , a [ href "/x" ] [ text "Click here" ]
    ]
        ]
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isRunning then
        Time.every 1000 TimerWentOff
    else
        Sub.none


-- main =
--   Browser.document
--       { init = init
--       , update = update
--       , view = view 
--       , subscriptions = subscriptions
--       }
--
