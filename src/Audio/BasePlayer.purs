module Audio.BasePlayer
  (Melody, MidiPhrase, PlaybackState(..), State, Event (SetInstruments, SetMelody, PlayMelody), initialState, foldp, setInstruments, setMelody, view) where

import CSS.TextAlign (center, textAlign)
import Audio.SoundFont (AUDIO, Instrument, MidiNote, playNotes)
import CSS (color, fromString)
import CSS.Background (background, backgroundImages)
import CSS.Border (border, borderRadius, solid)
import CSS.Box (boxShadow)
import CSS.Color (rgb, rgba)
import CSS.Display (display, float, floatLeft, inlineBlock, position, relative)
import CSS.Geometry (width, height, padding, margin)
import CSS.Overflow (hidden, overflow)
import CSS.Size (px)
import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (null, index, length)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Prelude (class Show, class Eq, bind, const, discard, negate, not, show, pure, (==), ($), (+), (*), (&&), (||))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (div, input, progress)
import Text.Smolder.HTML.Attributes (type', disabled, max, src, value)
import Text.Smolder.Markup (Attribute, text, (#!), (!), (!?))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

-- | The Base Player.  This does the playing, but relies on a specific Player
-- | (which wraps it) to provide the Melody Source.  The idea is that we can then
-- | have different players - a MIDI player, an ABC player and an HSoM player all
-- | of which essentially 'inherit' from the base player.
-- | The soundfont instruments must be supplied if you want any sound!
-- | The melody should only be established when the Play button is first pressed.

-- | a Melody is the entity that is played by the MIDI player
type MidiPhrase = Array MidiNote
type Melody = Array MidiPhrase

-- | Player events,  Only SetMelody and PlayMelody is exposed.
data Event
  = NoOp
  | SetInstruments (Array Instrument)   -- set the available instruments
  | SetMelody Melody                    -- instantiate the melody to be played
  | StepMelody Number                   -- step to the next phrase
                                        -- not exposed but its presence allows a view update
  | PlayMelody PlaybackState            -- play | pause
  | EnablePlayButton                    -- re-enable the pause/play button (after a pause)
  | StopMelody                          -- stop and set index to zero

-- | the internal state of the player
type State =
  { instruments :: Array Instrument  -- the instrument soundfonts available
  , melody :: Melody                 -- the melody to play
  , playing :: PlaybackState         -- the state of the playback
  , phraseIndex :: Int               -- the current phrase being played
  , lastPhraseLength :: Number       -- the duration of the phrase currently playing
  }

-- | now we have tri-state logic for playback state because of the pending status
data PlaybackState =
    PLAYING           -- the melody is playing
  | PENDINGPAUSED     -- the pause button has been hit, but the phrase is stil finishing
  | PAUSED            -- the melody is not playing  (paused or stopped)

derive instance genericPlaybackState :: Generic PlaybackState _
instance showEvent :: Show PlaybackState where
  show = genericShow
instance eqEvent :: Eq PlaybackState where
  eq = genericEq

-- | the initial state of the player (with no melody to play yet)
initialState :: State
initialState =
  { instruments : []
  , melody : []
  , playing : PAUSED
  , phraseIndex : 0
  , lastPhraseLength : 0.0
  }

-- | set the instrument sound fonts to use
setInstruments :: Array Instrument -> State -> State
setInstruments instruments state =
  state { instruments = instruments }

-- | set the source of the melody directly as a Melody itself (needing no transformation)
setMelody :: Melody -> State -> State
setMelody melody state =
  state { melody = melody, phraseIndex = 0, lastPhraseLength = 0.0 }

-- | the autonomous state update
foldp :: ∀ fx. Event -> State -> EffModel State Event (au :: AUDIO | fx)
foldp NoOp state =  noEffects $ state
foldp (SetInstruments instruments) state =
  noEffects $ setInstruments instruments state
foldp (SetMelody melody) state =
  noEffects $ setMelody melody state
foldp (StepMelody delay) state =
  step state delay
foldp (PlayMelody playing) state =
  if ((playing == PLAYING) && (not (null state.melody))) then
    -- play
    step (state { playing = playing }) 0.0
  else
    -- pause
    temporarilyFreezePlayButton (state { playing = PENDINGPAUSED })
foldp (StopMelody) state =
  -- stop/reset to start
  temporarilyFreezePlayButton (state { phraseIndex = 0
                                     , playing = PENDINGPAUSED })
foldp EnablePlayButton state =
  noEffects $ state { playing = PAUSED }

-- | step through the MIDI events, one by one
step :: forall e. State -> Number -> EffModel State Event (au :: AUDIO | e)
step state sDelay =
  case locateNextPhrase state of
    Just (midiPhrase) ->
      let
        msDelay = sDelay * 1000.0
        -- set the new state
        newState =
          state { phraseIndex = state.phraseIndex + 1
                , lastPhraseLength = sDelay
                }
      in
        { state: newState
        , effects:
          [ do
              _ <- delay (Milliseconds msDelay)
              nextDelay <- liftEff (playEvent state.instruments midiPhrase)
              pure $ Just (StepMelody nextDelay)
          ]
        }
    _ ->
      noEffects state

-- | the pause button is unresponsive.  The playback only pauses when the current
-- | phrase finishes playing. So temporarily freeze the play button.
temporarilyFreezePlayButton :: forall e. State -> EffModel State Event (au :: AUDIO | e)
temporarilyFreezePlayButton state =
  let
    msDelay = state.lastPhraseLength * 1000.0
  in
    { state: state
    , effects:
      [ do
          _ <- delay (Milliseconds msDelay)
          pure $ Just EnablePlayButton
      ]
    }

-- | play a MIDI Phrase (a bunch of MIDI notes)
-- | only NoteOn events produce sound
playEvent :: forall eff. Array Instrument -> MidiPhrase -> Eff (au :: AUDIO | eff) Number
playEvent instruments midiPhrase =
  playNotes instruments midiPhrase

-- | locate the next MIDI phrase from the performance
locateNextPhrase :: State -> Maybe MidiPhrase
locateNextPhrase state =
  if (not (state.playing == PLAYING)) || (null state.melody) then
    Nothing
  else
    index state.melody (state.phraseIndex)

-- | the player widget (start, stop, pause, progress)
view :: State -> HTML Event
view state =
  player state

player :: State -> HTML Event
player state =
  let
    sliderPos = show state.phraseIndex
    -- the buttons are temporarily disabled after a pause command
    isDisabled = (state.playing == PENDINGPAUSED)

    startImg = "assets/images/play.png"
    stopImg =  "assets/images/stop.png"
    pauseImg = "assets/images/pause.png"
    -- the action reverses the PLAYING - PAUSED status
    playAction =
      if (state.playing == PLAYING) then
         PlayMelody PAUSED
      else
         PlayMelody PLAYING
    playButtonImg =
      if (state.playing == PAUSED) then
        startImg
      else
        -- the pause image is displayed both if the tune is to be PlayMelody
        -- or else if it is disabled pending a pause
        pauseImg
    capsuleMax =
      show $ length state.melody
  in
    div ! playerBlockStyle $ do
      div ! playerBaseStyle ! playerStyle $ do
        progress ! capsuleStyle ! max capsuleMax ! value sliderPos $ do
          text ""
        div ! buttonStyle $ do
          (input !? isDisabled) (disabled "disabled") ! type' "image" ! src playButtonImg
             #! onClick (const playAction)
          (input !? isDisabled) (disabled "disabled") ! type' "image" ! src stopImg
             #! onClick (const StopMelody)

centreStyle :: Attribute
centreStyle =
  style do
    textAlign center

-- | the capsule is the bit in the centre of the player widget that shows progress
-- | through the recording
capsuleStyle :: Attribute
capsuleStyle =
  style do
    border solid (px 1.0) (rgb 0 0 0)
    margin (px 8.0) (px 0.0) (px 8.0) (px 0.0) -- ??
    borderRadius (px 5.0) (px 5.0) (px 5.0) (px 5.0)
    -- backgroundColor (rgb 0 0 0)
    background (rgb 0 0 0)
    backgroundImages
      [ fromString "-webkit-gradient(linear, left top, left bottom, color-stop(1, rgba(0,0,0,0.5)), color-stop(0, #333))"
      , fromString "-webkit-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-moz-linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-ms-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "-o-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      , fromString "linear-gradient(top, rgba(0, 0, 0, 0.5) 1, #333 0)"
      ]
    boxShadow (px 0.0) (px 0.0) (px 0.0) (rgb 5 5 5)
    overflow hidden
    display inlineBlock
    width (px 220.0)
    height (px 20.0)


-- | the basic style of the outline of the player which surrounds
-- | both the buttons and the capsule
playerBlockStyle :: Attribute
playerBlockStyle =
  style do
    margin (px 10.0) (px 0.0) (px 10.0) (px 0.0)
    background (rgba 0 0 0 0.7)
    border solid (px 1.0) (rgb 0 0 0)
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)
    width (px 330.0)
    position relative  -- "relative; z-index: 2"

-- the style of the player
playerStyle :: Attribute
playerStyle =
  style do
    height (px 36.0)
    boxShadow (px (-1.0)) (px (-1.0)) (px (-1.0))  (rgb 0 0 0)
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)

-- more player style attributes
playerBaseStyle :: Attribute
playerBaseStyle =
  style do
    border solid (px 1.0) (rgb 0 0 0)
    backgroundImages
      [ fromString "-webkit-gradient(linear,left top,left bottom,from(rgba(66,66,66,1)),to(rgba(22,22,22,1)))"
      , fromString "-webkit-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-moz-linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-ms-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "-o-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      , fromString "linear-gradient(top, rgba(66, 66, 66, 1) 0%, rgba(22, 22, 22, 1) 100%)"
      ]
    boxShadow (px 0.0) (px 0.0) (px 10.0) (rgb 15 15 15) -- #fff
    borderRadius (px 10.0) (px 10.0) (px 10.0) (px 10.0)
    padding (px 15.0) (px 20.0) (px 15.0) (px 20.0)
    color (rgba 255 255 255 0.8)
    -- "text-shadow", "1px 1px 2px #000"  ???

-- player button styling
buttonStyle :: Attribute
buttonStyle =
  style do
    width (px 80.0)
    margin (px 2.0) (px 3.0) (px 0.0) (px 3.0)
    float floatLeft
    -- opacity 0.7
