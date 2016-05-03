#!/bin/csh
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
#
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
#
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
#
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
#
# A script wrapper that is meant to get inventories of gridded data
# from the A-II database.  The data is output to stdout as ASCII.
# This version can adapt to use a python stub that calls the
# data access framework.
#
# Usage:
#
#  a2invmdl.csh p srcid ctyp lval1 lval2 varAbrev
#    p - A literal p. (optional)
#    srcid - Unique alphanumeric name for gridded data source.
#    ctyp  - Level type id (optional)
#    lval1 - First level value (optional)
#    lval2 - Second level value (optional)
#    varAbrev - Variable abreviation. (optional)
#
#  Legacy usage, not supported in all cases:
#
#  a2invmdl.csh p gproc ggid ctyp lval1 lval2 varAbrev
#
#    p - A literal p. (optional)
#    gproc - GRIB process number (can be multiple comma delimited)
#    ggid  - GRIB grid number
#    ctyp  - Level type id (optional)
#    lval1 - First level value (optional)
#    lval2 - Second level value (optional)
#    varAbrev - Variable abreviation. (optional)
#
#  With the new unified GRIB decoder, instead of gproc ggid, it is best
#  to supply the srcid, which is like ETA or GFS-EPAC40; e.g. the directory
#  under /awips2/edex/data/hdf5/grid where the data is stored.
#
#  Note that now for sources with no <grid> tag in the associated <model>
#  entry, the ggid must be supplied as a quoted empty string.
#
#  With no arguments after the grid number, returns a list of variables for 
#  the data source specified by the process and grid id.  With only a variable,
#  returns information for the list of planes for that variable. With more
#  arguments, returns a list of times for that variable and plane.
#
#  Level value arguments are meaningless without the level type argument,
#  but it is meaningful to provide only a level type.
#
#  If the only argument after the process and grid is a literal at sign ('@')
#  then what is returned is a list of all times for which there is data
#  available for the given process/grid combination.
#
#  If the only argument after the process and grid is a literal plus sign (+),
#  then what will be returned will be a level inventory for all variables.
#
#  The literal p option means preserve the final version of the python
#  submitted to the UEngine instead of cleaning it up.  The path to the
#  finalized python is /tmp/a2rdmdlNNNNN.py where NNNNN is a unix process id.
#
# Gets all available raob data in the A-II database over a specified range of
# times. The data is output to stdout as ASCII.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2014-10-22      3599          nabowle        Initial modification. Changed to properly call DAF version.
#
set rmpy = yes
if ( "$1" == "p" ) then
    set rmpy = no
    shift
endif
#
# Identify directory this script is in, will be one of the directories we
# search for other files in.
#
set mydir = `dirname $0`
set d1 = `echo $mydir | cut -c1`
if ( "$mydir" == '.' ) then
     set mydir = $PWD
else if ( "$d1" != "/" ) then
     set mydir = $PWD/$mydir
endif
set mydir = `(cd $mydir ; pwd)`
if ( -x $mydir/$0 ) then
    set me = $mydir/$0
else
    set me = $0
endif
if ( ! $?FXA_HOME ) set FXA_HOME = xxxx
#
#  Primarily base stuff on source name, but try to use the old interface.
#
set sss = "$1"
shift
set ids = `echo $sss | tr ',' ' '`
echo "$ids[1]" | grep '^[0-9][0-9]*$' >& /dev/null
if ( $status == 0 || $#ids > 1 ) then
    set mroot = /awips2/edex/data/utility/edex_static/base/grib/models
    set ids = `echo $ids | tr ' ' '\n' | grep -v '^ *$' | \
               sed 's#^#<id>#g' | sed 's#$#<|#g'`
    set ids = `echo ${ids}quertyuiop | sed 's/ *//g'`
    set ggg = "$1"
    shift
    if ( "$ggg" == "" ) then
        set mmm = `find $mroot -name '*xml' ! -name '*ECMWF*' \
                  -exec cat '{}' \; | sed 's|-->|~|g' | \
                  tr '\t' ' ' | sed 's/ *//g' | sed 's|</model>|~|g' | \
                  tr '\n' ' ' | tr '~' '\n' | grep -E "$ids" | \
                  grep -v "<grid>" | sed 's/^.*<name>//g' | \
                  cut '-d<' -f1 | sort -u`
    else
        set mmm = `find $mroot -name '*xml' -exec cat '{}' \; | \
                  sed 's|-->|~|g' | \
                  tr '\t' ' ' | sed 's/ *//g' | sed 's|</model>|~|g' | \
                  tr '\n' ' ' | tr '~' '\n' | grep -E "$ids" | \
                  grep "<grid>$ggg<" | sed 's/^.*<name>//g' | \
                  cut '-d<' -f1 | sort -u`
    endif
    if ( $#mmm != 1 ) then
        echo "$mmm"
        echo "Could not look up model name based on $sss '$ggg'"
        exit 1
    endif
    set sss = $mmm
endif

#
# Locate python stub that we will modify to create the final python logic.
#
if ( -e ./a2invmdlStub.py ) then
    set stubpy = ./a2invmdlStub.py
else if ( -e $mydir/a2invmdlStub.py ) then
    set stubpy = $mydir/a2invmdlStub.py
else if ( -e $FXA_HOME/src/dm/grid/a2invmdlStub.py ) then
    set stubpy = $FXA_HOME/src/dm/grid/a2invmdlStub.py
else if ( -e $FXA_HOME/bin/a2invmdlStub.py ) then
    set stubpy = $FXA_HOME/bin/a2invmdlStub.py
else
    bash -c "echo could not find a2invmdlStub.py 1>&2"
    exit
endif
#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    if ( "$*" == "+" ) then
        /awips2/python/bin/python $stubpy --mode fieldplane --srcId $sss
    else if ( "$1" == "" ) then
        /awips2/python/bin/python $stubpy --mode field --srcId $sss
    else if ( "$1" == "@" ) then
        /awips2/python/bin/python $stubpy --mode time --srcId $sss
    else if ( "$2" == "" ) then
        /awips2/python/bin/python $stubpy --mode plane --srcId $sss --varAbrev $1
    else if ( "$3" == "" ) then
        /awips2/python/bin/python $stubpy --mode time --srcId $sss --lvlName $1 --varAbrev $2
    else if ( "$4" == "" ) then
        /awips2/python/bin/python $stubpy --mode time --srcId $sss --lvlName $1 --lvlOne $2 --varAbrev $3
    else
        /awips2/python/bin/python $stubpy --mode time --srcId $sss --lvlName $1 --lvlOne $2 --lvlTwo $3 --varAbrev $4
    endif
else
    #
    if ( "$*" == "+" ) then
        set varList = `$me $sss`
        foreach onevar ( $varList )
            echo ${onevar}:
            $me $sss $onevar | tr '\n' ' '
            echo
        end
        exit
    endif

    #
    # Set up the environment we need to run the UEngine.
    #
    if ( -e ./UEngine.cshsrc ) then
        set ueenv = ./UEngine.cshsrc
    else if ( -e $mydir/UEngine.cshsrc ) then
        set ueenv = $mydir/UEngine.cshsrc
    else if ( -e $FXA_HOME/src/dm/point/UEngine.cshsrc ) then
        set ueenv = $FXA_HOME/src/dm/point/UEngine.cshsrc
    else if ( -e $FXA_HOME/bin/UEngine.cshsrc ) then
        set ueenv = $FXA_HOME/bin/UEngine.cshsrc
    else
        bash -c "echo could not find UEngine.cshsrc 1>&2"
        exit
    endif
    source $ueenv

    #
    #  Modify the text of special tags in stub to create finalized script.
    #
    set specpy = /tmp/a2invmdl${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    set plane = no
    if ( "$1" == "" ) then
      cat $stubpy | sed "s/MMMMM/field/g" | sed "s/SSSSS/$sss/g" | \
          sed 's/^.*TTTTT.*$//g' | sed 's/^.*LLLLL.*$//g' | \
          sed 's/^.*22222.*$//g' | sed 's/^.*VVVVV.*$//g' >> $specpy
    else if ( "$1" == "@" ) then
      cat $stubpy | sed "s/MMMMM/time/g" | sed "s/SSSSS/$sss/g" | \
          sed 's/^.*TTTTT.*$//g' | sed 's/^.*LLLLL.*$//g' | \
          sed 's/^.*22222.*$//g' | sed 's/^.*VVVVV.*$//g' >> $specpy
    else if ( "$2" == "" ) then
      set plane = yes
      cat $stubpy | sed "s/MMMMM/plane/g" | sed "s/SSSSS/$sss/g" | \
          sed 's/^.*TTTTT.*$//g' | sed 's/^.*LLLLL.*$//g' | \
          sed 's/^.*22222.*$//g' | sed "s/VVVVV/$1/g" >> $specpy
    else if ( "$3" == "" ) then
      cat $stubpy | sed "s/MMMMM/time/g" | sed "s/SSSSS/$sss/g" | \
          sed "s/TTTTT/$1/g" | sed 's/^.*LLLLL.*$//g' | \
          sed 's/^.*22222.*$//g' | sed "s/VVVVV/$2/g" >> $specpy
    else if ( "$4" == "" ) then
      cat $stubpy |  sed "s/MMMMM/time/g" | sed "s/SSSSS/$sss/g" | \
          sed "s/TTTTT/$1/g" | sed "s/LLLLL/$2/g"| \
          sed 's/^.*22222.*$//g' | sed "s/VVVVV/$3/g" >> $specpy
    else
      cat $stubpy |  sed "s/MMMMM/time/g" | sed "s/SSSSS/$sss/g" | \
          sed "s/TTTTT/$1/g" | sed "s/LLLLL/$2/g" | \
          sed "s/22222/$3/g" | sed "s/VVVVV/$4/g" >> $specpy
    endif
    if ( "$plane" == "no" ) then
        cd $UE_BIN_PATH
        ( uengine -r python < $specpy ) |& grep attributes | cut '-d"' -f4
    else
        cd $UE_BIN_PATH
        ( uengine -r python < $specpy ) |& sed 's|.*</items>.*|@|g' | \
            grep -E 'attributes|@' | cut '-d"' -f4 | tr '\n' ' ' | tr '@' '\n' | \
            sed 's/ -999999.0//g' | sed 's/^ *//g' | sed 's/ *$//g'
    endif
    #if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
    #
endif

