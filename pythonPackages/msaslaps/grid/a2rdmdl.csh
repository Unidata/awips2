#!/bin/csh -f
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

# Gets all available raob data in the A-II database over a specified range of
# times. The data is output to stdout as ASCII.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2014-10-16      3598          nabowle        Initial modification. Changed to handle DataAccessLayer.
#
#
# A script wrapper that is meant to get a single slab of gridded data
# from the A-II database.  The data is output to stdout as ASCII.
# This version can adapt to use a python stub that calls the
# data access framework.
#
# Usage:
#
#  a2rdmdl.csh p n x srcid ctyp lval1 lval2 varAbrev date hour fcst
#
#    p - A literal p. (optional)
#    n - A literal n. (optional)
#    x - A literal x. (optional)
#    srcid - Unique alphanumeric name for gridded data source.
#    ctyp  - Level type id (optional)
#    lval1 - First level value (optional)
#    lval2 - Second level value (optional)
#    varAbrev - Variable abreviation.
#    date  - Date of model run time as yyyy-mm-dd
#    hour  - Hour of model run time as hh
#    fcst  - Forecast time in hours
#
# Legacy usage, not supported in all cases:
#
#  a2rdmdl.csh p n x gproc ggid ctyp lval1 lval2 varAbrev date hour fcst
#
#    p - A literal p. (optional)
#    n - A literal n. (optional)
#    x - A literal x. (optional)
#    gproc - GRIB process number (can be multiple comma delimited)
#    ggid  - GRIB grid number
#    ctyp  - Level type id (optional)
#    lval1 - First level value (optional)
#    lval2 - Second level value (optional)
#    varAbrev - Variable abreviation.
#    date  - Date of model run time as yyyy-mm-dd
#    hour  - Hour of model run time as hh
#    fcst  - Forecast time in hours
#
#  With the new unified GRIB decoder, instead of gproc ggid, it is best
#  to supply the srcid, which is like ETA or GFS-EPAC40; e.g. the directory
#  under /awips2/edex/data/hdf5/grid where the data is stored.
#
#  Note that now for sources with no <grid> tag in the associated <model>
#  entry, the ggid must be supplied as a quoted empty string.
#
#  The literal p option means preserve the final version of the python
#  submitted to the UEngine instead of cleaning it up.  The path to the
#  finalized python is /tmp/a2rdmdlNNNNN.py where NNNNN is a unix process id.
#  The literal n option means the first line of output is the dimension of
#  the grid returned.  The literal x option means return the data in xdr
#  format; in this case the xdr data begins immediately after a tab-linefeed.
#
#  Process any one character options.
#
set rmpy = yes
set dimStr = dimStr
set specpyName = a2rdmdlStub
while (1)
    if ( "$1" == "p" ) then
        set rmpy = no
    else if ( "$1" == "n" ) then
        set dimStr = qwertyuiop
    else if ( "$1" == "x" ) then
        set specpyName = a2rdmdlXdr
        set dimStr = qwertyuiopx
    else
        break
    endif
    shift
end
#
# Identify directory this script is in, will be one of the directories we
# search for other files we need.
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
# Special case of topo, we will hard code it.
#
echo " $* " | grep " Topo " >& /dev/null
if ( $status == 0 ) then
    set outfile = `find . -maxdepth 1 \
         -name "$1.Topo" -o -name "$1.$2.Topo" -o -name "*,$1.$2.Topo" -o \
         -name "$1,*.$2.Topo" -o -name "*,$1,*.$2.Topo"` >& /dev/null
    if ( ( $#outfile != 1 ) && ( $mydir != $PWD ) ) then
         set outfile = `find $mydir -maxdepth 1 \
            -name "$1.Topo" -o -name "$1.$2.Topo" -o -name "*,$1.$2.Topo" -o \
            -name "$1,*.$2.Topo" -o -name "*,$1,*.$2.Topo"` >& /dev/null
    endif
    if ( ( $#outfile != 1 ) && ( -d $FXA_HOME/data ) ) then
         set outfile = `find $FXA_HOME/data -maxdepth 1 \
             -name "$1.Topo" -o -name "$1.$2.Topo" -o -name "*,$1.$2.Topo" -o \
             -name "$1,*.$2.Topo" -o -name "*,$1,*.$2.Topo"` >& /dev/null
    endif
    if ( $#outfile != 1 ) then
        bash -c "echo No flat file available with topo for $1 $2 1>&2"
        exit
    endif
    if ( "$dimStr" == "qwertyuiop" ) then
        set nnn = `cat $outfile | wc`
        set ny = $nnn[1]
        @ nx = $nnn[2] / $ny
        echo $nx $ny
    endif
    cat $outfile
    exit
endif
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

@ i = $#argv - 3
set vvvvv = $argv[$i]

#
# Locate python stub that we will modify to create the final python logic.
#
if ( -e ./${specpyName}.py ) then
    set stubpy = ./${specpyName}.py
else if ( -e $mydir/${specpyName}.py ) then
    set stubpy = $mydir/${specpyName}.py
else if ( -e $FXA_HOME/src/dm/grid/${specpyName}.py ) then
    set stubpy = $FXA_HOME/src/dm/grid/${specpyName}.py
else if ( -e $FXA_HOME/bin/${specpyName}.py ) then
    set stubpy = $FXA_HOME/bin/${specpyName}.py
else
    bash -c "echo could not find ${specpyName}.py 1>&2"
    exit
endif
#
# Determine if we are using the data access framework or the uEngine.
#
grep DataAccessLayer $stubpy >& /dev/null
if ( $status == 0 ) then
    set userArgs = "--srcId $sss --varAbrev $vvvvv"
    if ( ( "$dimStr" != "dimStr" ) && ( "$specpyName" != "a2rdmdlXdr" ) ) then
        set userArgs = "$userArgs --dimLine"
    endif

    if ( "$5" == "" ) then
        set userArgs = "$userArgs --date $2 --hour $3 --fcst $4"
    else if ( "$6" == "" ) then
        set userArgs = "$userArgs --lvlName $1 --date $3 --hour $4 --fcst $5"
    else if ( "$7" == "" ) then
        set userArgs = "$userArgs --lvlName $1 --lvlOne $2 --date $4 --hour $5 --fcst $6"
    else
        set userArgs = "$userArgs --lvlName $1 --lvlOne $2 --lvlTwo $3 --date $5 --hour $6 --fcst $7"
    endif
    python $stubpy $userArgs
else
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

    set rrrrr = ""
    set aaa = `echo $vvvvv | grep -E '^CP|^TP|^LgSP' | tr 'A-z' ' '`
    set aaa = `echo $aaa`
    #
    #  Special case of formatting the times for accumulated precip
    #
    if ( "$aaa" != "" ) then
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
        @ i++
        set t = `echo $* | cut '-d ' -f${i}-$#argv`
        @ fff = $t[3] * 3600
        set vt = `$gtasUtil = $t[1] ${t[2]}:00:00.0 $fff`
        @ aaa = $aaa * 3600
        set bt = `$gtasUtil = $vt -$aaa`
        set rrrrr = "[$bt--$vt]"
    endif

    #
    #  Modify the text of special tags in stub to create finalized script.
    #
    set specpy = /tmp/a2rdmdl${$}.py
    rm -rf $specpy >& /dev/null
    touch $specpy
    chmod 775 $specpy
    if ( "$5" == "" ) then
      cat $stubpy | grep -v $dimStr | sed "s/SSSSS/$sss/g" | \
          sed 's/^.*TTTTT.*$//g' | sed 's/^.*LLLLL.*$//g' | \
          sed 's/^.*22222.*$//g' | sed "s/VVVVV/$1/g" | sed "s/DDDDD/$2/g" | \
          sed "s/HHHHH/$3/g" | sed "s/FFFFF/$4/g" | sed "s/RRRRR/$rrrrr/g" >> \
          $specpy
    else if ( "$6" == "" ) then
      cat $stubpy | grep -v $dimStr | sed "s/SSSSS/$sss/g" | \
          sed "s/TTTTT/$1/g" | sed 's/^.*LLLLL.*$//g' | sed 's/^.*22222.*$//g' | \
          sed "s/VVVVV/$2/g" | sed "s/DDDDD/$3/g" | \
          sed "s/HHHHH/$4/g" | sed "s/FFFFF/$5/g" | sed "s/RRRRR/$rrrrr/g" >> \
          $specpy
    else if ( "$7" == "" ) then
      cat $stubpy | grep -v $dimStr | sed "s/SSSSS/$sss/g" | \
          sed "s/TTTTT/$1/g" | sed "s/LLLLL/$2/g" | sed 's/^.*22222.*$//g' | \
          sed "s/VVVVV/$3/g" | sed "s/DDDDD/$4/g" | \
          sed "s/HHHHH/$5/g" | sed "s/FFFFF/$6/g" | sed "s/RRRRR/$rrrrr/g" >> \
          $specpy
    else
      cat $stubpy | grep -v $dimStr | sed "s/SSSSS/$sss/g" | \
          sed "s/TTTTT/$1/g" | sed "s/LLLLL/$2/g" | sed "s/22222/$3/g" | \
          sed "s/VVVVV/$4/g" | sed "s/DDDDD/$5/g" | \
          sed "s/HHHHH/$6/g" | sed "s/FFFFF/$7/g" | sed "s/RRRRR/$rrrrr/g" >> \
          $specpy
    endif
    #
    #  Submit the temporary python script stripping any xml stuff, then remove it
    #
    cd $UE_BIN_PATH
    ( uengine -r python < $specpy ) | grep -v '<' | grep -v Response
    if ( "$rmpy" == "yes" ) rm -rf $specpy >& /dev/null
endif
