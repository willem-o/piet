about
================
An interpreter for the programming language Piet (http://www.dangermouse.net/esoteric/piet.html).
Currently only runs the program "hi.png".

build instructions
==================
Run ``ghc -o piet Main.hs Types.hs``.

program manual
==============
Assuming the compiled program is called `piet`, run ``./piet <img_name> -cs <codel_size>`` on a Unix system or 
``piet.exe <img_name> -cs <codel_size>`` when you're on Windows. `img_name` and `codel_size` respectively
represent the name of the image you'd like to execute and the 'pixel size'. That is, a codel is an enlarged
pixel by a factor of `codel_size`. The reason Piet uses codels, and not just pixels, is that otherwise Piet 
programs would be rather small.

running the example
===================
The codels of `hi.png` are 16x16, so you should run
``./piet hi.png -cs 16`` or ``piet.exe hi.png -cs 16``.

`hi.png` was downloaded from http://www.bertnase.de/npiet/.

licence
=======
You may use this code for whatever purpose it may serve.