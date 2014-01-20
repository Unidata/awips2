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
#
#


source /awips2/cave/iniLookup.sh
RC=$?
if [ ${RC} -ne 0 ]; then
   echo "ERROR: unable to find and/or access /awips2/cave/iniLookup.sh."
   exit 1
fi

# This script will be sourced by cave.sh.
export CAVE_INI_ARG=

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

            retrieveAssociatedINI ${arg} ${nextArg}
            RC=$?
            if [ ${RC} -eq 0 ]; then
               export CAVE_INI_ARG="--launcher.ini /awips2/cave/${ASSOCIATED_INI}"
            else
               export CAVE_INI_ARG="--launcher.ini /awips2/cave/cave.ini"
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

# returns _numPids and array _pids containing the pids of the currently running cave sessions.
function getPidsOfMyRunningCaves()
{
   local user=`whoami`
   local caveProcs=`ps -ef | grep -E "(/awips2/cave|/usr/local/viz)/cave " | grep -v "grep" | grep $user`

   # preserve IFS and set it to line feed only
   local PREV_IFS=$IFS
   IFS=$'\n'
   _numPids=0

   # grab the pids for future use
   for caveProc in $caveProcs
   do
      _pids[$_numPids]=`echo $caveProc | awk '{print $2}'`
      let "_numPids+=1"
   done
   IFS=$PREV_IFS
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
