Version compatible with demon2k 4.3.6. To install this version LAPACK is needed in addition to the other dependencies of demon2k

Use the .default_version in the deMon directory to specify version and compiler. Make sure the correct database file is in  
deMon/4.3.6/database. Put the demon directory in the same directory as demon is located.

Example standard installation:

(standing in /home/mathias/programs/deMon.4.3.6)
* export CREX_ROOT=/home/mathias/programs/deMon.4.3.6
* deMon/bin/CREX
* choose standard installation in list

executable created in:
/home/mathias/programs/deMon.4.3.6/deMon/4.3.6/object.std/deMon.4.3.6.std

custom installation:

(standing in /home/mathias/programs/deMon.4.3.6)
* export CREX_ROOT=/home/mathias/programs/deMon.4.3.6
* deMon/bin/old.crex -c std demon/4.3.6/

executable created in:
/home/mathias/programs/deMon.4.3.6/demon/4.3.6/object.std/deMon.4.3.6.std


