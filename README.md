# count-down
Count Down is a simple, web based and single player, game. It is implemented
in Elm, using the language's nice WebGL library.

A screenshot of the game can be seen below.

![Game screenshot](https://raw.githubusercontent.com/psandahl/count-down/master/screenshots/count-down.png)

## How to play

The purpose is to move the marker, using the Left, Up, Right, Down keys on the
keyboard (or the W, A, S, D keys), and run over counters that show up on the
board. If a counter is ran over it is stopped, you get a point and a bonus
where your marker will move a little faster for a few seconds.

If a counter expire you will lose a life and you need to re-play the current
level. If you lose all your three lives it's game over.

Demo video.
![Demo video](https://www.youtube.com/watch?v=DhSXR7EaJ5E)

## Build the game server

The game is served by a simple web server. If you're familiar using Haskell
you just build it with 'stack build'. Start the server in the root directory
of the repository and point your web browser to port 8000 of your computer.
