#!/bin/bash
# -----------------------------------------------------------
# UNIX Shell Script File Name: process_lock.sh
# Tested Operating System(s): RHEL 3, 4, 5
# Tested Run Level(s): 3, 5
# Shell Used: BASH shell
# Original Author(s): Douglas.Gaer@noaa.gov
# File Creation Date: 09/20/2007
# Date Last Modified: 01/22/2009
#
# Version control: 1.04
#
# Support Team:
#
# Contributors:
# -----------------------------------------------------------
# ------------- Program Description and Details -------------
# -----------------------------------------------------------
#
# Complex locking protocol functions for BASH, POSIX, Bourn, KSH
#
# This script is designed to be sourced from other scripts.
# Before calling the lock file functions the caller must
# define the $LOCKfile variable that defined the lock file
# used by the caller to lock a process.
# -----------------------------------------------------------

function CreateLockFile()
{
    # Build a new lock file using "hostname:PID" format
    hostname=`hostname`
    echo -n "${hostname}:" > ${LOCKfile}
    echo -n $$ >> ${LOCKfile}
    chmod 777 ${LOCKfile}
}

function RemoveLockFile()
{
    rm -f ${LOCKfile}
}

function SimpleLockFileCheck()
{
    # Simple check and remove for lock file

    # Set the function variables
    MINold="60"
    if [ "$1" != "" ]
        then
        MINold="$1"
    fi

    echo "Checking for lock file older than $MINold minutes"
    find ${LOCKfile} -mmin +$MINold -type f -print -exec rm -f {} \; &> /dev/null
    if [ -e ${LOCKfile} ]
        then
        echo "Lock file still exists: ${LOCKfile}"
        echo "Application may still be running or has terminated before completion"
        echo "Exiting..."
        exit 1
    fi
    echo "Removing the old lock file and continuing to run"
}

function LockFileCheck()
{
    # Complex locking protocol

    # Set the function variables
    MINold="60"
    if [ "$1" != "" ]
        then
        MINold="$1"
    fi

    if [ -e ${LOCKfile} ]
        then
        echo "A lock file exists: ${LOCKfile}"

        # Read the hostname and the PID from the lock file
        LOCKhostname=`awk -F: '{ print $1 }' ${LOCKfile}`
        LOCKpid=`awk -F: '{ print $2 }' ${LOCKfile}`

        # Check our current hostname
        hostname=`hostname`
        if [ "$hostname" != "$LOCKhostname" ]
            then
            echo "Process is locked on another node $LOCKhostname"
            echo "Checking the lock file age, max is $MINold minutes"
            SimpleLockFileCheck $MINold
            return
        fi
        echo "Checking to see how long the process has been running"
        currlockfile=`find ${LOCKfile} -mmin +$MINold -type f -print`
        if [ "${currlockfile}" == "${LOCKfile}" ]
            then
            echo "PID ${LOCKpid} has been running for more than $MINold minutes"
            ispidvalid=`ps -e | grep ${LOCKpid} | grep -v grep | awk '{print $1}'`
            if [ "$ispidvalid" == "$LOCKpid" ]
                then
                echo "Attempting to kill the previous process ${LOCKpid} and any children"
                pidlist=`ps -ef | grep $LOCKpid | grep -v grep | awk '{print $2}' | sort -rn`
                for pids in $pidlist
                do
                  echo "Killing PID $pids"
                  kill $pids
                  ispidvalid=`ps -ef | grep ${pids} | grep -v grep | awk '{print $2}'`
                  if [ "$ispidvalid" == "$pids" ]
                      then
                      echo "Could not kill process $pids for process ${LOCKpid}"
                      echo "${LOCKpid}  process is still running"
                      echo "Application may be running or has terminated before completion"
                      echo "Exiting..."
                      exit 1
                  fi
                done

                # Check for PIDs that respawned after killing a child
                pidlist=`ps -ef | grep $LOCKpid | grep -v grep | awk '{print $2}' | sort -rn`
                for pids in $pidlist
                do
                  echo "Killing respawned PID $pids"
                  ispidvalid=`ps -e | grep $pids | grep -v grep | awk '{print $1}'`
                  # Make sure the PID is still active are killing child process
                  if [ "$ispidvalid" == "$pids" ]
                      then
                      kill $pids
                  fi
                  # Check to see if the PID is still running
                  ispidvalid=`ps -ef | grep ${pids} | grep -v grep | awk '{print $2}'`
                  if [ "$ispidvalid" == "$pids" ]
                      then
                      echo "Could not kill process $pids for process ${LOCKpid}"
                      echo "${LOCKpid}  process is still running"
                      echo "Application may be running or has terminated before completion"
                      echo "Exiting..."
                      exit 1
                  fi
                done
            else
                echo "PID ${LOCKpid} is not running"
                echo "Removing the old lock file and continuing to run"
                rm -f ${LOCKfile}
                return
            fi
        fi

        # Check to see if the process is still running
        ispidvalid=`ps -e | grep ${LOCKpid} | grep -v grep | awk '{print $1}'`
        echo "Checking to see if the previous process is still running"
        if [ "$ispidvalid" == "$LOCKpid" ]
            then
            echo "${LOCKpid}  process is still running"
            echo "Application may be running or has terminated before completion"
            echo "Exiting..."
            exit 1
        fi

        echo "Previous process is not running"
        echo "Removing the old lock file and continuing to run"
        rm -f ${LOCKfile}
    fi
}
# -----------------------------------------------------------
# *******************************
# ********* End of File *********
# *******************************
