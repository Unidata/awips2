#!/bin/bash

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
#
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Dec 05, 2013  #2593     rjpeter     Fix getPidsOfMyRunningCaves
# Dec 05, 2013  #2590     dgilling    Modified extendLibraryPath() to export a
#                                     var if it's already been run.
# Jan 24, 2014  #2739     bsteffen    Add method to log exit status of process.
# Jan 30, 2014  #2593     bclement    extracted generic part of getPidsOfMyRunningCaves into forEachRunningCave
#                                     added methods for reading max memory from .ini files
#                                     fixes for INI files with spaces
# Feb 20, 2014  #2780     bclement    added site type ini file check
#
# Mar 13  2014  #15348    kjohnson    added function to remove logs


source /awips2/cave/iniLookup.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: unable to find and/or access /awips2/cave/iniLookup.sh."
   exit 1
fi

# This script will be sourced by cave.sh.
export CAVE_INI_ARG=

BYTES_IN_KB=1024
BYTES_IN_MB=1048576
BYTES_IN_GB=1073741824

function lookupINI()
{
   # Arguments:
   # 
   if [ "${1}" == "" ]; then
      return 1
   fi   

   position=1
   for arg in $@; do
      if [ "${arg}" == "-component" ] ||
         [ "${arg}" == "-perspective" ]; then
            # Get The Next Argument.
            position=$(( $position + 1 ))
            nextArg=${!position}

            retrieveAssociatedINI ${arg} "${nextArg}"
            RC=$?
            if [ ${RC} -eq 0 ]; then
               export CAVE_INI_ARG="--launcher.ini /awips2/cave/${ASSOCIATED_INI}"
            else
               siteTypeIni="/awips2/cave/${SITE_TYPE}.ini"
               if [[ -e ${siteTypeIni} ]] 
               then
                   export CAVE_INI_ARG="--launcher.ini ${siteTypeIni}"
               else
                   export CAVE_INI_ARG="--launcher.ini /awips2/cave/cave.ini"
               fi
            fi
            return 0
      fi
      position=$(( $position + 1 ))
   done

   return 1
}

function extendLibraryPath()
{
   # Arguments:
   #
   # ${1} == -noX {optional}

   local CAVE_LIB_DIRECTORY=
   if [ -d /awips2/cave/lib ]; then
      local CAVE_LIB_DIRECTORY="/awips2/cave/lib"
   fi
   
   if [ -d /awips2/cave/lib64 ]; then
      local CAVE_LIB_DIRECTORY="/awips2/cave/lib64"
   fi

   export LD_LIBRARY_PATH="${CAVE_LIB_DIRECTORY}/lib_illusion:$LD_LIBRARY_PATH"
   if [ "${1}" = "-noX" ]; then
      export LD_LIBRARY_PATH="${CAVE_LIB_DIRECTORY}/lib_mesa:$LD_LIBRARY_PATH"
   fi
   
   CALLED_EXTEND_LIB_PATH="true"
}

function copyVizShutdownUtilIfNecessary()
{
   local VIZ_UTILITY_SCRIPT="awips2VisualizeUtility.sh"

   # Ensure that there is a .kde directory.
   if [ ! -d ${HOME}/.kde ]; then
      return 0
   fi

   # There is a .kde directory, continue.
   if [ ! -d ${HOME}/.kde/shutdown ]; then
      mkdir ${HOME}/.kde/shutdown
   fi

   if [ -f ${HOME}/.kde/shutdown/${VIZ_UTILITY_SCRIPT} ]; then
      rm -f ${HOME}/.kde/shutdown/${VIZ_UTILITY_SCRIPT}
   fi
   # Copy the newest version of the utility to the user's shutdown directory.
   cp /awips2/cave/${VIZ_UTILITY_SCRIPT} ${HOME}/.kde/shutdown/${VIZ_UTILITY_SCRIPT}

   chmod a+x ${HOME}/.kde/shutdown/${VIZ_UTILITY_SCRIPT}  
}

# takes a function as an argument and calls the function passing in the ps string of the process
function forEachRunningCave()
{
   local user=`whoami`
   local caveProcs=`ps -ef | grep -E "(/awips2/cave|/usr/local/viz)/cave " | grep -v "grep" | grep $user`

   # preserve IFS and set it to line feed only
   local PREV_IFS=$IFS
   IFS=$'\n'

   for caveProc in $caveProcs
   do
      "$@" $caveProc
   done
   IFS=$PREV_IFS
}

# takes in ps string of cave process, stores pid in _pids and increments _numPids
function processPidOfCave()
{
    _pids[$_numPids]=`echo $1 | awk '{print $2}'`
    let "_numPids+=1"
}

# returns _numPids and array _pids containing the pids of the currently running cave sessions.
function getPidsOfMyRunningCaves()
{
   _numPids=0
   forEachRunningCave processPidOfCave
}

# takes a name of an ini file as an argument, echos the memory (in bytes) from file (or default)
function readMemFromIni()
{
    local inifile="$1"
    local mem
    local unit
    local regex='^[^#]*-Xmx([0-9]+)([bBkKmMgG])?'
    # read ini file line by line looking for Xmx arg
    while read -r line
    do
        if [[ $line =~ $regex ]]
        then
            mem=${BASH_REMATCH[1]}
            unit=${BASH_REMATCH[2]}
            break
        fi
    done < "$inifile"
    # convert to bytes
    case "$unit" in
        [kK]) 
            mem=$(($mem * $BYTES_IN_KB))
            ;;
        [mM])
            mem=$(($mem * $BYTES_IN_MB))
            ;;
        [gG])
            mem=$(($mem * $BYTES_IN_GB))
            ;;
    esac
    regex='^[0-9]+$'
    if [[ ! $mem =~ $regex ]]
    then
        # we couldn't find a valid Xmx value
        # java default is usually 1G
        mem=1073741824
    fi
    echo $mem
}

# takes in ps string of cave process, reads Xmx from ini and adds bytes to _totalRunninMem
function addMemOfCave()
{
    local inifile
    # get ini file from process string
    local regex='--launcher.ini\s(.+\.ini)'
    if [[ $1 =~ $regex ]]
    then
        inifile="${BASH_REMATCH[1]}"
    else
        inifile="/awips2/cave/cave.ini"
    fi
    let "_totalRunningMem+=$(readMemFromIni "$inifile")"
}

# finds total max memory of running caves in bytes and places it in _totalRunningMem
function getTotalMemOfRunningCaves()
{
   _totalRunningMem=0
   forEachRunningCave addMemOfCave
}

function deleteOldCaveDiskCaches()
{
   local curDir=`pwd`
   local user=`whoami`
   local caches="diskCache/GFE"
   local cacheDir="$HOME/caveData/etc/workstation"
   local host=`hostname -s`

   if [ -d "$cacheDir/$host" ]; then
      cacheDir="$cacheDir/$host"
   else
      host=${host%-} # remove the -testbed
      if [ -d "$cacheDir/$host" ]; then
         cacheDir="$cacheDir/$host"
      else
         host=`hostname`
         cacheDir="$cacheDir/$host"
      fi
   fi

   if [ -d "$cacheDir" ]; then
      # found cache dir for workstation
      cd $cacheDir

      # grab the current cave pids
      getPidsOfMyRunningCaves

      for cache in $caches; do
         if [ -d "$cache" ]; then
           cd $cache

           diskPids=`ls -d pid_* 2> /dev/null`

           for dPid in $diskPids; do
              # strip the pid_ and compare to pids of running caves
              dPidNum="${dPid#pid_}"
              found=0

              for pid in ${_pids[*]}; do
                 if [ "$pid" == "$dPidNum" ]; then
                    found=1
                    break
                 fi
              done

              if [ $found -eq 0 ]; then
                rm -rf $dPid
              fi
           done

           cd ..
         fi
      done
   fi

   cd $curDir
}

# log the exit status and time to a log file, requires 2 args pid and log file
function logExitStatus()
{
   pid=$1
   logFile=$2
   
   trap 'kill $pid' SIGHUP SIGINT SIGQUIT SIGTERM
   wait $pid
   exitCode=$?
   curTime=`date --rfc-3339=seconds`
   echo Exited at $curTime with an exit status of $exitCode >> $logFile
   
   # If a core file was generated attempt to save it to a better place
   coreFile=core.$pid
   if [ -f "$coreFile" ]; then
     basePath="/data/fxa/cave"
     hostName=`hostname -s`
     hostPath="$basePath/$hostName/"
     mkdir -p $hostPath
     if [ -d "$hostPath" ]; then
       mv $coreFile $hostPath
     fi
   fi
}

#Delete old CAVE logs DR 15348
function deleteOldCaveLogs() 
{

    local curDir=$(pwd)
    local mybox=$(hostname)

    echo -e "Cleaning consoleLogs: "
    echo -e "find $BASE_LOGDIR -type f -name "*.log" -mtime +7 -exec rm {} \;"


    find "$BASE_LOGDIR" -type f -name "*.log" -mtime +7 -exec rm {} \;

    exit 0

}

