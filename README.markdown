# Race

Executing `race x y z` is pretty much the same as executing `x & y & z & wait`,
except it prints output of each command in different color.

The idea is to use it to run multiple long-running (most likely never ending)
commands that help during development.

## Installation

Clone it from Github. Run `stack build --copy-bins`, which will build it and
copy the final executable into some place that is hopefully in your `$PATH`.
