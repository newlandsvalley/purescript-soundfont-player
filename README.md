purescript-soundfont-player
===========================

This is a player for melodies consisting of Midi Notes which can be played through [soundfont](https://github.com/newlandsvalley/purescript-polyphonic-soundfonts) buffers via web-audio. It is designed as a Pux module.  

It follows the standard TEA/Pux guidelines for an embedded module.  It implements a view of a player widget which operates autonomously from the calling application and responds to stop/pause/start button presses which in turn produce the equivalent stop/pause/start event messages.  This can be achieved if the calling program delegates all such player event messages to the player itself within the main event loop.  

The player requires Midi Notes to be grouped into phrases which then make up the overall _Melody_.  The player re-renders the view only after a phrase has completed playing.  This is a compromise between allowing the tune to be played at tempo and letting the stop/start buttons be reasonably responsive. It is the responsibility of the calling program to decide the phrase length and thus influence the responsiveness of the buttons.

It is possible to use TEA in such a way that it implements the moral equivalent of inheritance if you make appropriate use of message passing. It is intended that players for specific representations of music will inherit from this basic player in order to provide dedicated players (for MIDI, ABC etc) and it is these players that will be embedded into the overall application.

In many cases, an application will want to display the player whenever there is something to play, but the user may not always choose to use it. It thus makes sense for any specialised player to defer building the _Melody_ until the play button is first pressed.  This can be achieved simply by intercepting the 'Play' message before delegating to the base player. The player thus exposes __SetMelody__ (so that it can be re-initialised with a new melody) and __PlayMelody__ (so that Play messages can be detected and if necessary intercepted). 

The player uses png images for the various buttons.  These are maintained in the projects for the dedicated players rather than in this project.

to build the module
-------------------

   bower install

   pulp build


