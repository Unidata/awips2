#!/bin/sh
################################################################################
#                                                                              #
# Program name:  rsyncGridsToCWF.sh                                            #
# Version:  4.0                                                                #
# Language (Perl, C-shell, etc.): bash                                         #
#                                                                              #
# Authors:  Virgil Middendorf (BYZ), Steve Sigler (MSO)                        #
# Contributers: Ahmad Garabi, Ken Sargeant, Dave Pike, Dave Rosenberg,         #
#               Tim Barker, Maureen Ballard, Jay Smith, Dave Tomalak,          #
#               Evelyn Bersack, Juliya Dynina, Jianning Zeng, John McPherson   #
#                                                                              #
# Date of last revision:  08/18/17                                             #
#                                                                              #
# Script description: This script can create a netcdf file containing IFPS     #
#    grids, quality control the netcdf file, send the file to a local rsync    #
#    server (ls2/3), and then rsync the file to the remote rsync servers.      #
#    Quality Control involves iscMosaic'ing the contents of the netcdf file    #
#    into the Restore database in GFE. If a failure is detected for any of the #
#    grids, then the forecasters will get a red-banner alarm and the script    #
#    will recreate the netcdf file.                                            #
#                                                                              #
#    To upload netcdf files use the following arguments:                       #
#       ./rsyncGridsToCWF.sh wfo                                               #
#    (where wfo is the three character wfo id)                                 #
#                                                                              #
#    This script is designed to work in service backup situations. This script #
#    is launched from the Scripts... menu in GFE and it will work in both      #
#    operational and service backup situations.                                #
#                                                                              #
# Directory program runs from:  /awips2/GFESuite/bin                           #
#                                                                              #
# Needed configuration on ls2/3:  For each wfo that you run this script for,   #
#                                 you will need a /data/ldad/grid/wfo          #
#                                 directory. (where wfo is the 3 character     #
#                                 wfo id)                                      #
#                                                                              #
# Revision History:                                                            #
# 02/12/07:  Created Script to rsync grids to CRH from ls1. vtm                #
# 03/22/07:  Added rsync to gridiron and Steve's QC methodology. vtm           #
# 03/26/07:  Changed iscMosaic so output works in remote ssh/background. sjs   #
# 04/03/07:  Added code to change permissions. vtm                             #
# 04/03/07:  Added bwlimit and timeout switches to rsync call. vtm             #
# 04/03/07:  Made parmlist easier to configure? vtm                            #
# 04/05/07:  Added a check to see if netcdf file made it to the WRH farm. vtm  #
# 04/05/07:  Added red-banner alarm if netcdf did not make it to WRH. vtm      #
# 04/05/07:  Added mask setting in config section. vtm                         #
# 04/20/07:  Fixed missing mask setting for second ifpnetCDF call. vtm         #
# 04/20/07:  Added -D 0.0 option to speed up iscMosaic. vtm                    #
# 04/20/07:  Changed iscMosaic database from Restore to Test_Fcst. vtm         #
# 04/23/07:  Took out parmlist parameter from ifpnetCDF. vtm                   #
# 04/23/07:  Added use of a backup rsync server. vtm                           #
# 04/25/07:  Added red-banner notifying forecaster that ls1 is down. vtm       #
# 04/25/07:  Added red-banner notifying forecaster that netcdf files have been #
#            rsynced. vtm                                                      #
# 05/03/07:  Added functionally to allow a limited set of parms to be sent by  #
#            the primary site, while all parms sent for backup sites. vtm      #
# 05/03/07:  Added publish to official check for service backup. vtm           #
# 05/03/07:  Now rsync file to WR webfarm first. vtm                           #
# 05/07/07:  AlertViz issue fixed. Switched Reminder to Announcer. vtm         #
# 05/20/07:  Added switch to turn off sending grids to the consolidated web    #
#            farm. vtm                                                         #
# 06/04/07:  Added code to make the number of times to attempt netcdf file     #
#            creation configurable. Baseline 3 times. vtm                      #
# 06/04/07:  Script now quality controls netcdf files created in attempts #2   #
#            and beyond. If the third attempt is bad, then script quits. vtm   #
# 06/05/07:  Added code to remove the QC logfile after each attempt, otherwise #
#            the script will never send the netcdf file in the 2nd and 3rd     #
#            attempts. vtm                                                     #
# 06/05/07:  Added code to notify forecaster that grids passed QC check and    #
#            included a switch to turn this notification off. vtm              #
# 06/06/07:  Added code to remove the netcdf file if it is too small. vtm      #
# 06/06/07:  Changed the name of the netcdf files so they include the process  #
#            id in event the script is launched again before the first launch  #
#            is completed. vtm                                                 #
# 06/06/07:  Added a check to ensure rsync is not already running to ensure    #
#            multiple script launches do not conflict with each other. vtm     #
# 06/07/07:  Fixed the rsync already running check. vtm                        #
# 06/08/07:  iscMosaic error files were not being deleted fixed. vtm           #
# 06/11/07:  Corrected name of file sent to the consolidated web farm. vtm     #
# 06/14/07:  Sent "sendCWFgrids" to "no" per request. vtm                      #
# 06/19/07:  When grids are not sent to the consolidated web farm, the backed  #
#            up zip file was not deleted and filling up ls1. Fixed. vtm        #
# 06/27/07:  Illiminated a "file does not exist error message. vtm             #
# 08/13/07:  Increased iscMosaic delay to 1.0 and make configurable. vtm       #
# 12/05/07:  Added test compressed netcdf file send to WR. vtm                 #
# 02/18/08:  Switched to use ls2/ls3. Added code to turn off send to regional  #
#            web farms. vtm                                                    #
# 02/22/08:  Removed WR specific code from the script. vtm                     #
# 02/25/08:  Adjusted list of parms based on list provided to me by Mark       #
#            Mitchell. vtm                                                     #
# 02/25/08:  creationAttempts config variable wasn't used by the script. vtm   #
# 02/25/08:  Added QCnetCDF config variable to allow offices to bypass long    #
#            netcdf file QC process. vtm                                       #
# 02/25/08:  Added a ls1 section in the configuration section. vtm             #
# 05/14/08:  Added audio for AlertViz "ANNOUNCER 0 LOCAL". Any bad message  #
#            will get sound with it. vtm                                       #
# 05/14/08:  If WR grids did not update, then script will echo out a message   #
#            for a log file. vtm                                               #
# 05/15/08:  Added code to change time stamps of netcdf files on ls1. vtm      #
# 05/15/08:  Moved AWIPS file clean up to the end. Directory wasn't being      #
#            cleaned out properly. Also included the cdl files to the list. vtm#
# 05/15/08:  Added code to clean orphaned files created by this script on AWIPS#
#            and on the local rsync server. vtm                                #
# 05/16/08:  Added check to see if netcdf file made it to the consolidated web #
#            farm. If not, then notify forecast via red banner. vtm            #
# 05/16/08:  Added switch to turn off sending grids to the WR web farm. vtm    #
# 05/16/08:  Eliminated the variables for the rsync, gunzip, and chmod         #
#            commands. vtm                                                     #
# 05/16/08:  Added configuration for email notifications. vtm                  #
# 05/16/08:  Added configuration for AlertViz alert level number when grid     #
#            creation or transfer problems occur. vtm                          #
# 05/21/08:  Added the --address= switch to the rysnc commands in an attempt   #
#            resolve rsync issues after switch to NOAANet. ls cluster only. vtm#
# 05/21/08:  If rsync fails to move the file to the server, then the script    #
#            will now retry 4 more times. vtm                                  #
# 05/22/08:  Moved relivent code from the WR version into this script. vtm     #
# 05/28/08:  Fixed Bug removing CurrentFcst.?????.site.cdf file. vtm           #
# 05/28/08:  CWF netcdf file availability check can now be turned off. vtm     #
# 05/28/08:  Added flag so all banner messages can be turned off. vtm          #
# 05/28/08:  Added the CWFcheckWait configuration. vtm                         #
# 05/29/08:  Bug in backup file time stamp touch. Touched vtm.opt.cdf instead  #
#            vtm.opt.cdf.gz. vtm                                               #
# 06/11/08:  Fixed bug. If rsync fails more than 5 times, the script was       #
#            supposed to stop. Instead it kept trying. vtm                     #
# 06/19/08:  Changed the directory on AWIPS where the netcdf file is created.  #
#            Now using a drive local to machine for performance reasons.       #
#            New directory is now /awips/fxa/netcdf. This directory will be    #
#            created by the script if it doesn't exist. vtm                    #
# 06/19/08:  Changed script to remove AWIPS netcdf and log files sooner. vtm   #
# 07/10/08:  Made the /awips/fxa/netcdf configurable for by DX and LX machines.#
#            vtm                                                               #
# 07/11/08:  Pointed most all of script feedback to a log file that can be     #
#            found in the /awips/GFESuite/primary/data/logfiles/yyyymmdd/      #
#            directory called netcdf_rsync.log. (LX workstations put the file  #
#            into the /awips/GFESuite/data/logfiles/yyyymmdd directory.) vtm   #
# 07/07/11:  Put in code to limit the time period of the netcdf file. vtm      #
# 04/16/12:  Added a little error checking for work directory and replaced     #
#            the hard-coded path to /awips2/fxa/bin with $FXA_BIN.  Removed    #
#            awips1 code.                                                      #
# 11/02/12:  Restored error checking for AWIPS2.                               #
# 04/22/13:  Update the permission of the log directories.                     #
# 02/24/14:  Create the file if rsync_parms.${site} is not available,          #
#            and mkdir the site directory on the local rsync server if it      #
#            does not exist.                                                   #
# *** Version 4.0 ***                                                          #
# 08/18/17:  Moved script to new /awips2/GFESuite/rsyncGridsToCWF/bin loc. vtm #
# 08/18/17:  Changed directory structure of app in configuration section.      #
#            Replaced DXwrkDir and WRKDIR with PROGRAM_ equivalents. vtm       #
# 08/18/17:  Added code to purge log directory after 14 days. vtm              #
# 08/18/17:  Reworked rsync check. DCS 17527. vtm                              #
# 10/04/18   Changed to reference AlertViz                                     #
################################################################################
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/16/19        21058         ryu            checks exit status from ifpnetCDF, convert_netcdf.pl and scp;
#                                                 sends AlertViz message before exiting if error occurrs;
#                                                 runs ifpnetCDF once only as ifpnetCDF makes 3 attemps;
#                                                 eliminate copying the netCDF file to the optimized filename
#                                                 before calling convert_netcdf.pl as it is unnecessary;
#                                                 replaced file paths/names with variables.
#
################################################################################


# check to see if site id was passed as argument
# if not then exit from the script
if [ $# -lt 1 ] ;then
   echo Invalid number of arguments.
   echo Script stopped.
   echo ./rsyncGridsToCWF.sh wfo
   exit
else
   SITE=$(echo ${1} | tr '[a-z]' '[A-Z]')
   site=$(echo ${1} | tr '[A-Z]' '[a-z]')
fi

################################################################################
#    Configuration Section                                                     #
################################################################################

GFESUITE_BIN="/awips2/GFESuite/bin"
PROGRAM_DIR="/awips2/GFESuite/rsyncGridsToCWF"
PROGRAM_BIN="${PROGRAM_DIR}/bin"
PROGRAM_ETC="${PROGRAM_DIR}/etc"
PROGRAM_CONFIG="${PROGRAM_DIR}/config"
PROGRAM_LOG="${PROGRAM_DIR}/log"
PROGRAM_DATA="${PROGRAM_DIR}/data"


################################################################################
#    End of Configuration Section                                              #
################################################################################

# Source in rsync_parms file for site. Copy in old site or baseline version if missing.

OLD_IFPS_DATA="/awips2/GFESuite/ServiceBackup/data"

if [ ! -f ${PROGRAM_ETC}/rsync_parms.${site} ] ;then
   # rsync_parms for site does not exist. Check if exists in old directory and if so, copy over.
   if [ -f ${OLD_IFPS_DATA}/rsync_parms.${site} ] ;then
      cp ${OLD_IFPS_DATA}/rsync_parms.${site} ${PROGRAM_ETC}/rsync_parms.${site}
   # rsync_parms not in old directory so get from baseline config directory
   else
      cp ${PROGRAM_CONFIG}/rsync_parms.ccc ${PROGRAM_ETC}/rsync_parms.${site}
   fi
fi

. ${PROGRAM_ETC}/rsync_parms.${site}

################################################################################

# set current data and log file names
currdate=$(date -u +%Y%m%d)
export LOG_FILE="${PROGRAM_LOG}/${currdate}/netcdf_rsync.log"

# check to see if log directory structure exists.
if [ ! -d  ${PROGRAM_LOG} ] ;then
   mkdir -p ${PROGRAM_LOG}
   chmod 777 ${PROGRAM_LOG}
   chown awips:fxalpha ${PROGRAM_LOG}
fi
if [ ! -d  ${PROGRAM_LOG}/${currdate} ] ;then
   mkdir -p ${PROGRAM_LOG}/${currdate}
   chmod 777 ${PROGRAM_LOG}/${currdate}
   chown awips:fxalpha ${PROGRAM_LOG}/${currdate}
fi

# Purge old log files
find ${PROGRAM_LOG}/. -mtime +14 -exec rm {} -Rf \;

# Log file header
echo " " >> $LOG_FILE
echo "####################################################################################" >> $LOG_FILE
echo "# Starting Grid Rsync Script for ${wfo}....                                        #" >> $LOG_FILE
echo "####################################################################################" >> $LOG_FILE
chmod 666 $LOG_FILE

# Check to see of the ${PROGRAM_DATA} directory exists. If not, then create.
echo making sure that ${PROGRAM_DATA} exists at $(date) >> $LOG_FILE
if [ ! -d ${PROGRAM_DATA} ] ;then
   echo "  ${PROGRAM_DATA} directory not found." >> $LOG_FILE
   echo "  making ${PROGRAM_DATA} directory..." >> $LOG_FILE
   mkdir -p ${PROGRAM_DATA}
   echo "  changing permissions of ${PROGRAM_DATA} directory..." >> $LOG_FILE
   chmod 777 ${PROGRAM_DATA}
   echo "  changing ownership of ${PROGRAM_DATA} directory to awips..." >> $LOG_FILE
   chown awips:fxalpha ${PROGRAM_DATA}
else
   echo "  ${PROGRAM_DATA} directory exists!" >> $LOG_FILE
fi
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE

# Clean up files older than 60 minutes in the ${PROGRAM_DATA} directory.
echo cleaning up files older than 60 minutes in the ${PROGRAM_DATA} directory at $(date) >> $LOG_FILE
find ${PROGRAM_DATA}/. -type f -mmin +60 -exec rm {} -f \;
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE

# sending to log file parmlist and mask settings
if [ "$parmlist" != "" ] && [ "$parmlist" != "     " ]; then
  echo "Will trim elements to $parmlist" >> $LOG_FILE
else
  echo "Will send all elements" >> $LOG_FILE
fi
echo "Using grid domain $mask" >> $LOG_FILE

# Determine the ifpnetCDF start and end times.
start_time=$(date +%Y%m%d_%H00 -d "6 hours ago")
end_time=$(date +%Y%m%d_%H00 -d "192 hours")
cdfTimeRange="-s ${start_time} -e ${end_time} "

pid=$$

# In this while loop, the netcdf file will be created and quality controlled.
# The script will attempt to create the netcdf file three times before failing.
creationAttempts=1

creationAttemptCount=1
badGridFlag=1

NCFILE=${PROGRAM_DATA}/CurrentFcst.${pid}.${site}.cdf
while (( ( $creationAttemptCount <= $creationAttempts ) && ( $badGridFlag == 1 ) ))
do
   # create the netcdf file
   echo starting netcdf file creation...attempt number ${creationAttemptCount} at $(date) >> $LOG_FILE
   echo " " >> $LOG_FILE
   ${GFESUITE_BIN}/ifpnetCDF -t -g -o ${NCFILE} -h $CDSHOST -d ${SITE}_GRID__Official_00000000_0000 -m $mask $cdfTimeRange $parmlist >> $LOG_FILE 2>&1

   if [[ $? > 0 ]]; then

      echo "Creating netcdf file for ${site} failed." >> $LOG_FILE

      if [[ $creationAttemptCount == $creationAttempts ]] ||
         [[ $turnOffAllNotifications == "no" ]]; then
          ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "Creating netcdf file for ${site} failed." -s
      fi
	 
      if [[ $creationAttemptCount == $creationAttempts ]]; then
	  echo "Exiting... " >> $LOG_FILE
	  exit 1
      fi

   else

      # Check to see if netcdf file is big enough. In service backup, publish to official may have been forgotten.
      filesize=$(ls -l ${NCFILE} | awk '{print $5}')
      if (( filesize < 1000000 )) ;then
         echo $filesize >> $LOG_FILE
         ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "${SITE} netcdf file determined to be incomplete and not sent to webfarms. Did you publish to official? Is EDEX down?" -s
         rm -f ${NCFILE}
         echo netcdf file is too small. Either the Official database is empty OR EDEX is down. >> $LOG_FILE
         echo Script stopped. >> $LOG_FILE
         exit
      fi
      echo ...finished. >> $LOG_FILE
      echo " " >> $LOG_FILE

      ##############################################
      # STOP HERE RIGHT NOW
      ##############################################
      if [[ $QCnetCDF == "yes" ]] ;then
         #Check netcdf file for errors. 
         echo started netcdf file checking at $(date) >> $LOG_FILE
         ${GFESUITE_BIN}/iscMosaic -h $CDSHOST $parmlist -f ${NCFILE} -d ${SITE}_GRID_Test_Fcst_00000000_0000 -D $iscMosaicDelay 
     
         if [[ $? > 0 ]] ;then
            if [[ $creationAttemptCount == $creationAttempts ]] ;then
               ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "Errors detected in ${SITE} netcdf file again and not sent to webfarms. Send Grids Manually." -s
               echo "Errors detected in ${SITE} netcdf file again and not sent to webfarms. Script stopped." >> $LOG_FILE
               exit
            else 
               if [[ $turnOffAllNotifications == "no" ]] ;then
                  ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "Errors detected in ${SITE} netcdf file again. Regenerating netcdf file attempt # ${creationAttemptCount}." -s
               fi
               echo "Errors detected in ${SITE} netcdf file. Regenerating netcdf file." >> $LOG_FILE
            fi
            rm -f ${NCFILE}
         else
            echo The netcdf file appears to be good. >> $LOG_FILE
            badGridFlag=0
         fi
      else
         echo netcdf file checking bypassed at $(date) >> $LOG_FILE
         badGridFlag=0
      fi
   
  fi

  creationAttemptCount=$((creationAttemptCount+1))

done
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE


# create the optimized netcdf file
echo creating optimzed netcdf file at $(date) >> $LOG_FILE

OPTNCFILE=${PROGRAM_DATA}/CurrentFcst.${pid}.${site}.opt.cdf

${PROGRAM_BIN}/convert_netcdf.pl ${NCFILE} ${OPTNCFILE} >> $LOG_FILE 2>&1

if [[ $? > 0 ]]; then
   echo "...failed" >> $LOG_FILE
   echo " " >> $LOG_FILE
   ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "Unable to optimize netCDF file to send to webfarms. Will not be sent." -s
   # clean up and exit
   rm -f ${NCFILE} >> $LOG_FILE 2>&1
   rm -f ${PROGRAM_DATA}/CurrentFcst.${pid}.${site}.*.cdl >> $LOG_FILE 2>&1
   rm -f ${OPTNCFILE} >> $LOG_FILE 2>&1
   exit 1
else
   echo ...finished. >> $LOG_FILE
   echo " " >> $LOG_FILE
fi

# check space used by the process of creating the opt netcdf file in the netcdf directory
echo "space used in ${PROGRAM_DATA}:  $(cd ${PROGRAM_DATA}; du -m --max-depth=1) mb" >> $LOG_FILE
echo ...finished >> $LOG_FILE
echo " " >> $LOG_FILE
      
# cleaning up files on AWIPS created by the optimizing process.
echo cleaning up files on AWIPS created by the optimizing process at $(date) >> $LOG_FILE
rm -f ${NCFILE} >> $LOG_FILE 2>&1
rm -f ${PROGRAM_DATA}/CurrentFcst.${pid}.${site}.*.cdl >> $LOG_FILE 2>&1
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE

# zip up the optimized netcdf file
echo starting optimized netcdf file zipping at $(date) >> $LOG_FILE
gzip -9 ${OPTNCFILE} >> $LOG_FILE 2>&1
if [[ $? > 0 ]]; then
   echo "gzipping of optimized netcdf file failed." >> $LOG_FILE
   echo " " >> $LOG_FILE
   ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "Unable to gzip optimized netCDF file to send to webfarms. Will not be sent." -s
   # clean up and exit
   rm -f ${OPTNCFILE}* >> $LOG_FILE 2>&1
   exit 2
else
   echo ...finished. >> $LOG_FILE
   echo " " >> $LOG_FILE
fi

# check spaced used by the zipped opt netcdf file in the netcdf directory
echo "space used in ${PROGRAM_DATA}:  $(cd ${PROGRAM_DATA}; du -m --max-depth=1) mb" >> $LOG_FILE
echo ... finished >> $LOG_FILE
echo " " >> $LOG_FILE


# if directory to write to is not on local rysnc server, create it. DR 16464
if ! ssh ${locServer} "[ -d ${locDirectory} ]" 2> /dev/null  ;then
     ssh ${locServer} mkdir ${locDirectory}
fi

# Clean up orphaned files on the local rsync server.
echo cleaning up orphaned files on $locServer in the ${locDirectory}/${site} directory at $(date) >> $LOG_FILE
ssh $locServer "find ${locDirectory}/${site} -mmin +720 -exec rm {} -f \;" >> $LOG_FILE 2>&1
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE


SYNCFN=CurrentFcst.${pid}.${site}.opt.cdf.gz
DESTFN=CurrentFcst.${site}.cdf.gz
SYNCDIR=${locDirectory}/${site}
SYNCFILE=${SYNCDIR}/${SYNCFN}
SYNCCOPY=${SYNCDIR}/vtm.opt.cdf.gz

# move optimized netcdf file to the local rsync server.
scp_success=0
for i in 1 2 3
do
   CHK=`ssh -q -o "BatchMode yes" -o "ConnectTimeout 5" $locServer "echo success"`;
   if [ "success" = $CHK ] >/dev/null 2>&1
   then
      echo attempt $i to scp optimized netcdf file to $locServer at $(date) >> $LOG_FILE
      scp ${PROGRAM_DATA}/${SYNCFN} ${locServer}:${SYNCDIR} >> $LOG_FILE 2>&1
      if [[ $? > 0 ]]; then
	  echo ...failed. >> $LOG_FILE
      else
	  echo ...finished. >> $LOG_FILE
	  echo " " >> $LOG_FILE
	  scp_success=1
	  break
      fi
   fi

   # failed to connect - wait 5 seconds and try again
   sleep 5
done

if [[ ${scp_success} != "1" ]] ;then
   if [[ $turnOffAllNotifications == "no" ]] ;then
       ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "Failed to send optimized netcdf file to $locServer. Script stopped." -s
   fi
   # cleanup the zipped optimized file on AWIPS
   rm -f ${PROGRAM_DATA}/${SYNCFN}
   echo "Failed to send optimized netcdf file to $locServer at $(date). Script stopped." >> $LOG_FILE
   exit 3
fi

# cleaning up the zipped optimized file on AWIPS.
echo cleaning up the zipped optimized file on AWIPS at $(date) >> $LOG_FILE
rm -f ${PROGRAM_DATA}/${SYNCFN}
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE

# Notify forecaster that the quality control check passed and rsyncing will begin.
if [[ $SendQCgoodNotification == "yes" ]] ;then
   echo sending forecaster notification that QC passed at $(date) >> $LOG_FILE
   if [[ $turnOffAllNotifications == "no" ]] ;then
      ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "${SITE} netcdf file passed quality control check. Now rsyncing the file to the webfarms." -s
   fi
   echo ...finished. >> $LOG_FILE
   echo " " >> $LOG_FILE
fi

# change file permissions
echo changed permissions of the uncompressed netcdf files on $locServer at $(date) >> $LOG_FILE
ssh $locServer "/bin/chmod 2775 ${SYNCFILE}"
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE

# make backup copies of the netcdf files so manual rsync commands can be used
echo make backup of the compressed netcdf files on $locServer at $(date) >> $LOG_FILE
ssh $locServer "cp ${SYNCFILE} ${SYNCCOPY}"
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE

# change the timestamp on the files
echo updated the time stamps of the compressed and uncompressed netcdf files on $locServer at $(date) >> $LOG_FILE
ssh $locServer "touch ${SYNCFILE}"
ssh $locServer "touch ${SYNCCOPY}"
echo ...finished. >> $LOG_FILE
echo " " >> $LOG_FILE

rsyncCompleted=0
rsyncAttempt=0
while [[ $rsyncCompleted == "0" ]] && (( $rsyncAttempt <= 5 ))
do
   echo Rsync attempt ${rsyncAttempt} >> $LOG_FILE
   echo checking to see if another rsync process is running >> $LOG_FILE

   # Checking to see if another rsync process is running. If so, then wait.
   try=0
   rsync_ok="yes"
   grepCMD="/usr/bin/rsync -rDvlzt ${locRsyncSwitches}"
   while ( ssh $locServer "ps -elf | grep \"${grepCMD}\" | grep -q -v grep" )
   do
      (( try = $try + 1 ))
      if (( $try > $rsyncWait )) ;then
         echo Waited more than $rsyncWait minutes to start a rsync to the Web Farm >> $LOG_FILE
         echo but another rsync process is still running - so could not. >> $LOG_FILE
         if [[ $turnOffAllNotifications == "no" ]] ;then
            ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "${SITE} GFE netcdf file NOT sent to the Consolidated web farm. Another rsync process blocked transfer." -s
         fi
         rsync_ok="no"
         break
      fi
      echo A rsync is already active...waiting 1 minute at $(date) >> $LOG_FILE
      sleep 60
   done

   if [[ $rsync_ok == "yes" ]] ;then
      # create the rsync test data file
      RSYNC_LOG_FILE="${PROGRAM_DATA}/rsync.${pid}.log"
      echo "" > $RSYNC_LOG_FILE
      
      # launch the rsync process that sends the netcdf file to the consolidated web farm at $(date)
      (( rsyncAttempt = $rsyncAttempt + 1 ))

      echo " " >> $LOG_FILE
      echo starting netcdf file rsync to consolidated webserver at $(date) >> $LOG_FILE
      echo " " >> $LOG_FILE

      rsyncCMD="/usr/bin/rsync -rDvlzt ${locRsyncSwitches} --stats --progress ${SYNCFILE} ${remServer1}::${remDirectory1}/${DESTFN}"
      ssh $locServer "$rsyncCMD" >> $LOG_FILE 2> $RSYNC_LOG_FILE

      echo $rsyncCMD >> $LOG_FILE
      echo "...finished." >> $LOG_FILE
      echo " " >> $LOG_FILE

      if [[ $checkCWFavailability == "yes" ]] ;then

         # Grep rsync test file for a host of errors
         error1=$(grep "connection unexpectedly closed" $RSYNC_LOG_FILE)
         error2=$(grep "failed to connect" $RSYNC_LOG_FILE)
         error3=$(grep "Connection closed by remote host" $RSYNC_LOG_FILE)
         error4=$(grep "Name or service not known" $RSYNC_LOG_FILE)
         error5=$(grep "Temporary failure in name resolution" $RSYNC_LOG_FILE)
         rsyncFlag="Fail"
         if [[ $error1 == "" && $error2 == "" && $error3 == "" && $error4 == "" && $error5 == "" ]] ;then
            rsyncFlag="Pass"
         fi

         # Check to see if netcdf file was posted at the Consolidated Web Farm. If not, then send red banner message.
         # If it did then send a notification to the forecasters that the send was successful.
         if [[ $rsyncFlag == "Fail" ]] ;then
            if [[ $turnOffAllNotifications == "no" ]] ;then
               ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "Rysnc of grids to ${remServer1} has failed. Attempting rsync again. ${error1} ${error2} ${error3} ${error4}" -s
            fi
            echo Detected that grids did NOT make it to the Consolidated web farm at $(date) >> $LOG_FILE
            echo "Rysnc of grids to ${remServer1} has failed. Attempting rsync again. ${error1} ${error2} ${error3} ${error4}" >> $LOG_FILE
            if [[ $sendEmailNotification == "yes" ]] ;then
               echo "X-Priority: 3" > ${PROGRAM_DATA}/email.txt
               echo "Subject: CWF Grid Rsync Report" >> ${PROGRAM_DATA}/email.txt
               echo "Rysnc of grids to ${remServer1} has failed. Attempting rsync again. ${error1} ${error2} ${error3} ${error4}" >> ${PROGRAM_DATA}/email.txt
               scp ${PROGRAM_DATA}/email.txt ${locServer}:${locDirectory}/email.txt
               if [[ $emailAddress1 != "" ]] ;then
                  ssh $locServer "/usr/sbin/sendmail -FAWIPS -fldad ${emailAddress1} < ${locDirectory}/email.txt"
               fi
               if [[ $emailAddress2 != "" ]] ;then
                  ssh $locServer "/usr/sbin/sendmail -FAWIPS -fldad ${emailAddress2} < ${locDirectory}/email.txt"
               fi
               if [[ $emailAddress3 != "" ]] ;then
                  ssh $locServer "/usr/sbin/sendmail -FAWIPS -fldad ${emailAddress3} < ${locDirectory}/email.txt"
               fi
            fi
         else
            rsyncCompleted=1
	 fi
      else
         rsyncCompleted=1
      fi

      rm -f $RSYNC_LOG_FILE
   fi
done
echo "...finished." >> $LOG_FILE
echo " " >> $LOG_FILE
      
if [[ ( $rsyncCompleted = 1) ]] ;then
    echo Detected that grids DID make it to the consolidated web farm at $(date) >> $LOG_FILE
    if [[ $sendCWFnotification == "yes" ]] ;then
        if [[ $turnOffAllNotifications == "no" ]] ;then
            ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "${SITE} GFE netcdf file sent to the consolidated web farm." -s
        fi
    fi
else
    echo Detected that grids did NOT make it to the Consolidated web farm at $(date) >> $LOG_FILE
    if [[ $turnOffAllNotifications == "no" ]] ;then
        ${GFESUITE_BIN}/sendGfeMessage -h ${CDSHOST} -c NDFD -m "${SITE} GFE netcdf file was NOT sent to the Consolidated Web Farm, because rsync connection is broken." -s
    fi
fi


# removing the zipped netcdf file
echo removing the zipped netcdf files on $locServer at $(date) >> $LOG_FILE
ssh $locServer "rm -f ${SYNCFILE}"
echo "...finished." >> $LOG_FILE
echo " " >> $LOG_FILE

echo script finished at $(date) >> $LOG_FILE
echo " " >> $LOG_FILE
   
exit

