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
# A script wrapper that is meant to get inventories of radar data
# from the A-II database.  The data is output to stdout as ASCII.
# This version can adapt to use a python stub that calls the
# data access framework.
#
# Usage:
#
#  a2invrad.csh p radar date time msgcode elev 
#
#    p - A literal p.
#    radar - four character radar id
#    date - yyyy-mo-dd
#    time - hh:mm
#    msgcode - RPG message code
#    elev - tilt angle/layer value. defaults to 0.
#
#  All arguments are optional, but must retain the specified order.
#
#  The literal p option means preserve the final version of the python
#  submitted to the UEngine instead of cleaning it up.  The path to the
#  finalized python is /tmp/a2invradNNNNN.py where NNNNN is a unix process id.
#  This argument does not change the output of the script.
#
#  If no radar id is supplied, the only other useable arguments are the
#  date and time, and what is returned is a list of radars.
#
#  The date and time must be supplied together. If not supplied, then the
#  information returned covers all times, otherwise +/- one minute from
#  the supplied date and time.
#
#  If the message code is not supplied, or is a literal '+', then what is
#  listed is the available message code for the specified radar.  The plus
#  sign causes some descriptive information to be supplied with the returned
#  codes. 
#
#  If the message code is supplied and the tilt/layer is a literal '+',
#  then what is returned is a list of tilts available for the specified
#  message code.
#
#  When a message code is supplied but no tilt is supplied, this is meant
#  to return a list of times for a non-tilt based product, such as storm
#  total precip.  Otherwise if a tilt/layer is supplied, this is meant
#  to return a list of times for product that are for a specific tilt,
#  like base reflectivity.
#
#  When tilt angles are specified on the command line, or are returned
#  in response to a tilt angle argument of '+', these tilt angles are
#  primary tilt angles for a range of tilts.  When tilt angles are returned
#  with a time inventory, then these are the actual tilt angle of the
#  data received.
#
# Gets inventories of radar grid data from the A-II database.  The data is
# output to stdout as ASCII. Inventories are limited to Radial and Raster
# formats.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2014-10-28      3600          nabowle        Initial modification. Call DAF properly.
#
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
if ( ! $?FXA_HOME ) set FXA_HOME = xxxx
set fxa_home = $FXA_HOME
if ( $?STAGING ) then
    set fxa_home = $STAGING/D-2D
endif
#
# Locate python stub that we will modify to create the final python logic.
#
if ( -e ./a2invradStub.py ) then
    set stubpy = ./a2invradStub.py
else if ( -e $mydir/a2invradStub.py ) then
    set stubpy = $mydir/a2invradStub.py
else if ( -e $fxa_home/src/dm/radar/a2invradStub.py ) then
    set stubpy = $fxa_home/src/dm/radar/a2invradStub.py
else if ( -e $FXA_HOME/bin/a2invradStub.py ) then
    set stubpy = $FXA_HOME/bin/a2invradStub.py
else
    bash -c "echo could not find a2invradStub.py 1>&2"
    exit
endif

set rrr = `echo $1 | grep '[a-z][a-z][a-z][a-z]'`
if ( "$rrr" != "" ) shift
set lastcmd = cat

#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    set mydate = `echo "$1" | grep '.*-.*-'`
    set mytime = `echo "$2" | grep ':'`
    if ( "$mydate" != "" && "$mytime" != "" ) then
        shift
        shift
        set userargs = "--date ${mydate} --time ${mytime}"
    else
        set userargs = ""
    endif

    if ( "$rrr" == "" ) then
        #done
    else if ( "$1" == "" ) then
        set userargs = "$userargs --icao $rrr"
    else if ( "$1" == "+" ) then
        set userargs = "$userargs --icao $rrr"
        if ( -e ./msgCodeSeds.txt ) then
            set mctrans = $PWD/msgCodeSeds.txt
        else if ( -e $mydir/msgCodeSeds.txt ) then
            set mctrans = $mydir/msgCodeSeds.txt
        else if ( -e $fxa_home/src/dm/radar/msgCodeSeds.txt ) then
            set mctrans = $fxa_home/src/dm/radar/msgCodeSeds.txt
        else if ( -e $FXA_HOME/data/msgCodeSeds.txt ) then
            set mctrans = $FXA_HOME/data/msgCodeSeds.txt
        else
            bash -c "echo could not find msgCodeSeds.txt 1>&2"
            exit
        endif
        set lastcmd = "sed -f $mctrans"
    else if ( "$2" == "" ) then
        set userargs = "$userargs --icao $rrr --productCode $1 --angle 0.0 --outputDate"
    else if ( "$2" == "+" ) then
        set userargs = "$userargs --icao $rrr --productCode $1 --outputPrimary"
    else 
        set userargs = "$userargs --icao $rrr --productCode $1 --angle $2 --outputTrue --outputDate"
    endif
    /awips2/python/bin/python $stubpy ${userargs} | $lastcmd
else
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

    #
    # Set range of time we will request this information over, will default to
    # essentially forever.
    #
    set aaa = "1970-01-01 00:00:00.0"
    set bbb = "2038-01-01 00:00:00.0"
    set mydate = `echo "$1" | grep '.*-.*-'`
    set mytime = `echo "$2" | grep ':'`
    if ( "$mydate" != "" && "$mytime" != "" ) then
        shift
        shift
        if ( -x ./gtasUtil ) then
            set gtasUtil = ./gtasUtil
        else if ( -x $mydir/gtasUtil ) then
            set gtasUtil = $mydir/gtasUtil
        else if ( -x $fxa_home/src/dm/point/gtasUtil ) then
            set gtasUtil = $fxa_home/src/dm/point/gtasUtil
        else if ( -x $FXA_HOME/bin/gtasUtil ) then
            set gtasUtil = $FXA_HOME/bin/gtasUtil
        else
            bash -c "echo could not find gtasUtil executable 1>&2"
            exit
        endif
        set aaa = `$gtasUtil = $mydate $mytime -60`
        set bbb = `$gtasUtil = $mydate $mytime 60`
    endif

    
    #
    #  Modify the text of special tags in stub to create finalized script.
    #
    set binary = no
    set specpy = /tmp/a2invrad${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    if ( "$rrr" == "" ) then
      cat $stubpy | grep -v "KKKK" | grep -v "MMMM" | grep -v "EEEE" | \
          sed "s/AAAAA/$aaa/g" | sed "s/BBBBB/$bbb/g" | \
          grep -v 'Code")'  | grep -v 'Time")' | grep -v 'Angle")' \
          >> $specpy
    else if ( "$1" == "" ) then
      cat $stubpy | sed "s/KKKK/$rrr/g" | grep -v "MMMM" | \
          sed "s/AAAAA/$aaa/g" | sed "s/BBBBB/$bbb/g" | grep -v 'icao")' | \
          sed 's/^.*EEEE.*$//g' | grep -v 'Time")' | grep -v 'Angle")' \
          >> $specpy
    else if ( "$1" == "+" ) then
      cat $stubpy | sed "s/KKKK/$rrr/g" | grep -v "MMMM" | \
          sed "s/AAAAA/$aaa/g" | sed "s/BBBBB/$bbb/g" | grep -v 'icao")' | \
          sed 's/^.*EEEE.*$//g' | grep -v 'Time")' | grep -v 'Angle")' \
          >> $specpy
          if ( -e ./msgCodeSeds.txt ) then
              set mctrans = $PWD/msgCodeSeds.txt
          else if ( -e $mydir/msgCodeSeds.txt ) then
              set mctrans = $mydir/msgCodeSeds.txt
          else if ( -e $fxa_home/src/dm/radar/msgCodeSeds.txt ) then
              set mctrans = $fxa_home/src/dm/radar/msgCodeSeds.txt
          else if ( -e $FXA_HOME/data/msgCodeSeds.txt ) then
              set mctrans = $FXA_HOME/data/msgCodeSeds.txt
          else
              bash -c "echo could not find msgCodeSeds.txt 1>&2"
              exit
          endif
          set lastcmd = "sed -f $mctrans"
    else if ( "$2" == "" ) then
      cat $stubpy | sed "s/KKKK/$rrr/g" | sed "s/MMMM/$1/g" | \
          sed "s/AAAAA/$aaa/g" | sed "s/BBBBB/$bbb/g" | grep -v 'icao")' | \
          sed 's/EEEE/0.0/g' | grep -v 'Angle")' | grep -v 'Code")' \
          >> $specpy
    else if ( "$2" == "+" ) then
      cat $stubpy | sed "s/KKKK/$rrr/g" | sed "s/MMMM/$1/g" | \
          sed "s/AAAAA/$aaa/g" | sed "s/BBBBB/$bbb/g" | grep -v 'icao")' | \
          sed 's/^.*EEEE.*$//g'  | grep -v 'Time")' | grep -v 'Code")' | \
          sed 's/true/primary/g' >> $specpy
    else 
      cat $stubpy | sed "s/KKKK/$rrr/g" | sed "s/MMMM/$1/g" | \
          sed "s/AAAAA/$aaa/g" | sed "s/BBBBB/$bbb/g" | grep -v 'icao")' | \
          sed "s/EEEE/$2/g" | grep -v 'Code")' >> $specpy
      set binary = yes
    endif
    #
    #  Submit the temporary python script stripping xml stuff, then remove it
    #
    if ( "$binary" == "yes" ) then
        cd $UE_BIN_PATH
        ( uengine -r python < $specpy ) |& sed 's|.*</items>.*|@|g' | \
            grep -E 'attributes|@' | cut '-d"' -f4 | tr '\n' ' ' | tr '@' '\n' | \
            sed 's/00000.*$//g' | sed 's/^ *//g' | sed 's/ *$//g'
    else
        cd $UE_BIN_PATH
        ( uengine -r python < $specpy ) |& grep attributes | cut '-d"' -f4 | \
           $lastcmd
    endif

    if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
endif

