#!/bin/bash
# DR #5757 - Moves any acarssounding processing files out of the localization
# structure and deletes the old processing directories.
#
# Only needs to be run on one of the edex-ingest machines.
# This should not be run while any edex-ingest VMs are running, or 
# simultaneously on multiple machines.
# Rerunning the script after the first run will have no effect.
#

from='/awips2/edex/data/utility/edex_static/base/acars'
to='/awips2/edex/data/processing/acars'

if [ ! -e $to/rawdata ]
then
    mkdir $to/rawdata
fi

if [ ! -e $to/acftObs ]
then
    mkdir $to/acftObs
fi


if [ -e $from/rawdata ]
then
    echo "Relocating rawdata/ files"
    for file in `ls $from/rawdata/`
    do
        cat $from/rawdata/$file >> $to/rawdata/$file
    done

    echo "Removing $from/rawdata"
    rm -r $from/rawdata
fi

if [ -e $from/acftObs/ ]
then
    echo "Relocating acftObs/ files"
    for file in `ls $from/acftObs/`
    do
        cat $from/acftObs/$file >> $to/acftObs/$file
    done

    echo "Removing $from/acftObs"
    rm -r $from/acftObs
fi

echo "Done."