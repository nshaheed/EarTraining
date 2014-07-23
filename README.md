EarTraining
===========

This is a rudimentary ear training app based in the HTML5 canvas, and created with blank-canvas (https://github.com/ku-fpg/blank-canvas).

It allows for melodic interval ear training with white-key notes in the range of D4-G5 with an interval span from a unison to an octave, and all chromatic intervals in between.  

Guessing the interval can be done either using the buttons below the staff or by clicking on the staff itself to guess a note.

###Libraries to Grab
EatTraining uses blank-canvas (https://hackage.haskell.org/package/blank-canvas) for the UI and euterpea (http://haskell.cs.yale.edu/euterpea/) for the musical back-end and playback.

###How to Install
Simply make in the directory, the program will build and automatically run after building.  Once the program is running, go to a web browser and type ``http://localhost:3000/`` to get to the program

### A Note About Playback 
If instant playback is an issue, simply comment out the line from eatTrainer.hs ``play z`` and uncomment the line ``writeMidi "randomTest.mid" z`` in order to output a midi file, which can then be played with external software, not ideal but at least you will get sound.
