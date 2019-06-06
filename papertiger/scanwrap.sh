#!/bin/bash
# scanwrap.sh
# Scan using scanimage with output file specified as first parameter

# Pass on all options to scanimage except the first
OUTPUTFILE="$1"
# get rid of first parameter, the output filename
shift
# call sane with the rest of the parameters:
scanimage "$@" > $OUTPUTFILE 
