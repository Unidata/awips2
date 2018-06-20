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
# A script wrapper that is meant to get data for a single satellite sector
# from the A-II database.  The result is output to stdout as ASCII.
# The first line returned has the dimensions of the image, the time, and the
# source satellite of the data set returned. The rest is one line per row
# of satellite data.  The data for each row undergoes second order compression
# Each pixel value of 0 or 255 is encoded as @ or #, respectively. Otherwise
# the first pixel on the row and any pixel that is more than 20 counts 
# different than the previous one is encoded as two hex digits.  Pixels the
# same as the previous are encoded as a period, pixels from 1 to 20 counts less
# than the previous are encoded as G through Z, and pixels from 1 to 20 counts
# more than the previous are encoded as g through z. There are no delimeters
# between the encoding for each pixel.
#
# This version can adapt to use a python stub that calls the
# data access framework.
#
# Usage:
#
#  a2rdsat.csh {p} {h|i} sector channel {satid} date time {slop} {partition}
#
#    p - (optional) A literal p.
#    h|i - (optional) A literal h or literal i.
#          Output pure undelimited hex or delimited integer values.
#    sector - sector id
#    channel - channel id
#    satid - (optional) satellite id
#    date - yyyy-mm-dd
#    time - hh:mm
#    slop - seconds of slop either side, defaults to 180 
#    partition - (optional) upper case letter indicating partition to get. For
#                very large images data may need to be returned in pieces.
#                Allowable partitions are A through D.
#
#   The ids can be either D-2D integer ids, or AWIPS-II ascii ids, in which
#   case they may need to be quoted on the command line.
#
#   Integer ids can be looked up in a2satInfo.txt, channel id corresponds to
#   the physicalElement, and satid corresponds to the creatingEntity.
#
#  The literal p option means preserve the final version of the python
#  submitted to the UEngine instead of cleaning it up.  The path to the
#  finalized python is /tmp/a2rdsatNNNNN.py where NNNNN is a unix process id.
#  The literal n option means the first line of output is the dimension of
#  the grid returned.
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Sep 29, 2014    3596          nabowle        Initial modification. Handle daf conversion.
#
#
#
set rmpy = yes
if ( "$1" == "p" ) then
    set rmpy = no
    shift
endif
set encoding = 2
if ( "$1" == "h" ) then
    set encoding = 1
    shift
endif
if ( "$1" == "i" ) then
    set encoding = 0
    shift
endif
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
if ( ! $?FXA_HOME ) set FXA_HOME = xxxx
#
# Locate python stub that we will modify to create the final python logic.
#
if ( -e ./a2rdsatStub.py ) then
    set stubpy = ./a2rdsatStub.py
else if ( -e $mydir/a2rdsatStub.py ) then
    set stubpy = $mydir/a2rdsatStub.py
else if ( -e $FXA_HOME/src/dm/sat/a2rdsatStub.py ) then
    set stubpy = $FXA_HOME/src/dm/sat/a2rdsatStub.py
else if ( -e $FXA_HOME/bin/a2rdsatStub.py ) then
    set stubpy = $FXA_HOME/bin/a2rdsatStub.py
else
    bash -c "echo could not find a2rdsatStub.py 1>&2"
    exit
endif


#
# Locate file containing mapping between D-2D interger ids and AWIPS-II ascii
# ids for sectors, channels, and satellites.
#
if ( -e ./a2satInfo.txt ) then
    set satInf = ./a2satInfo.txt
else if ( -e $mydir/a2satInfo.txt ) then
    set satInf = $mydir/a2satInfo.txt
else if ( -e $FXA_HOME/src/dm/sat/a2satInfo.txt ) then
    set satInf = $FXA_HOME/src/dm/sat/a2satInfo.txt
else if ( -e $FXA_HOME/data/a2satInfo.txt ) then
    set satInf = $FXA_HOME/data/a2satInfo.txt
else
    set satInf = ""
    set sss = "$1"
    set ccc = "$2"
    set eee = `echo $3 | grep -v '.*-'`
endif
#
#
if ( $satInf != "" ) then
    set sss = `grep "^ *$1|.*sectorID" $satInf | cut '-d|' -f3`
    if ( "$sss" == "" ) set sss = "$1"
    set ccc = `grep "^ *$2|.*physicalElement" $satInf | cut '-d|' -f3`
    if ( "$ccc" == "" ) set ccc = "$2"
    set eee = `echo $3 | grep -v '.*-'`
    if ( "$eee" != "" ) then
        set eee = `grep "^ *$eee|.*creatingEntity" $satInf | cut '-d|' -f3`
        if ( "$eee" == "" ) set eee = "$3"
    endif
endif

shift
shift

if ( "$eee" != "" ) shift
set slop = `echo $3 | grep '[0-9]'`
if ( "$slop" == "" ) set slop = 180

set ppp = `echo $argv[$#argv] | grep '^[A-Z]$'`
if ( "$ppp" == "" ) set ppp = 0

#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    set opts = ""
    if ( "$eee" != "" ) then
        set opts = "--entity ${eee}"
    endif

    if ( "$encoding" == "1" ) then
        set opts = "$opts --hex"
    else if ( "$encoding" == "0" ) then
        set opts = "$opts --int"
    endif 

    /awips2/python/bin/python $stubpy --sector "${sss}" --physical "${ccc}" --datetime "$1 $2" --part $ppp --slop $slop $opts
else
    #
    #  Get program that can do math with ascii time string, then use this to
    #  properly encode range of times for which we look for data.
    #
    if ( -x ./gtasUtil ) then
        set gtasUtil = ./gtasUtil
    else if ( -x $mydir/gtasUtil ) then
        set gtasUtil = $mydir/gtasUtil
    else if ( -x $FXA_HOME/src/dm/point/gtasUtil ) then
        set gtasUtil = $FXA_HOME/src/dm/point/gtasUtil
    else if ( -x $FXA_HOME/bin/gtasUtil ) then
        set gtasUtil = $FXA_HOME/bin/gtasUtil
    else
        bash -c "echo could not find gtasUtil executable 1>&2"
        exit
    endif

    #
    # Set up the environment we need to run the UEngine.
    #
    set method = "uengine"
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

    set aaa = `$gtasUtil = $1 $2 -$slop`
    set bbb = `$gtasUtil = $1 $2 $slop`

    set specpy = /tmp/a2rdsat${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    if ( "$eee" == "" ) then
      cat $stubpy | sed "s/SSSSS/$sss/g" | sed "s/CCCCC/$ccc/g" | \
          sed "s/AAAAA/$aaa/g" | sed "s/BBBBB/$bbb/g" | \
          sed 's/^.*EEEEE.*$//g'| sed "s/PPPPP/$ppp/g" | \
          sed "s/XXXXX/$encoding/g" >> $specpy
    else
      cat $stubpy | sed "s/SSSSS/$sss/g" | sed "s/CCCCC/$ccc/g" | \
          sed "s/AAAAA/$aaa/g" | sed "s/BBBBB/$bbb/g" | \
          sed "s/EEEEE/$eee/g" | sed "s/PPPPP/$ppp/g" | \
          sed "s/XXXXX/$encoding/g" >> $specpy
    endif

    cd $UE_BIN_PATH
    ( uengine -r python < $specpy ) | grep -v '<' | grep -v Response

    if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
    #
endif
