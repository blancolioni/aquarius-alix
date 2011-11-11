#!/bin/sh
gnatmake -O3 -P projects/aquarius.gpr
cp bin/aquarius-main ~/bin/aquarius
cp --archive config/* ~/.aquarius
aquarius --version
