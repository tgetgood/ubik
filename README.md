# lemonade

Experiment in higher order visual programming.

## Overview

This is both a library for doing vector graphics in a data driven idiomatic
clojure fashion and a framework for making interactive graphics / animations /
UIs using that library.

These two parts will likely be separated at a future date, but since the is
mostly an experiment at present, it's far more convenient to keep them together.

## Breakdown

The core graphics primitives are in `lemonade.core`.

Renderers are in `lemonade.renderers.*`. There are currently only 2, an HTML
Canvas 2d rendering context renderer for the browser and a quil renderer for JVM
Clojure.

Demo setups of the two renderers are in `lemonade.demos.*`, and examples that
*should* work in either are in `lemonade.examples.*`

## Setup

To get an interactive development environment run:

	lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

	(js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

	lein clean

To create a production build run:

	lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.

## License

Copyright © 2017 Thomas Getgood

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
