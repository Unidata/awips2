#!/bin/sh 
#
##############################################################################
#
# This is the script to execute the public climate summaries.
#
#
# Modification History:
# ---------------------
# 12/05/2002  OB2  Bob Morris         - Removed CLIMATE_BIN_DIR definition, it
#                                       is set in readenv.sh.  General cleanup.
#                                     - Replace -a's in tests.
#                                     - Replace /adapt_apps/ with /climate/
#                                     - Set env vars. IFPS_SITE_OFFICE_NAME
#                                       and IFPS_SITE_TIMEZONE for format_cli.
#                                     - Rewrote the wait_times logic.
# 01/27/2003  OB2  Bob Morris         - Fix path to catRecordClimateData.sh
#                                     - Made killpid work in both BASH, HP
# 02/03/2003  OB2  Bob Morris         - Modified recordClimate remsh/rsh logic,
#                                       and pass log file as argument.
# 02/11/2003  OB2  Bob Morris         - Modified ifps env vars set logic, strip
#                                       quotes from IFPS_SITE_OFFICE_NAME
# 02/21/2003  OB2  Bob Morris         - Modified/added chmod logic for tmp files
# 03/28/2003  OB2  Bob Morris         - Fix env var ID for IFPS_SITE_OFFICE_NAME
#                                       and modified recordClimate logic/messages
# 04/03/2003  OB2  Bob Morris         - Set format_flag to 1 if manual run to
#                                       allow format_climate to put up Edit
#                                       Climate Product notification.
# 11/14/2003  OB4  Manan Dalal        - Removed all r-commands such as rsh.
# 07/20/2004  OB5  Manan Dalal        - Removed execution of catRecordClimateData.sh
#                                       Pipe files for RecordClimate are removed
#                                       and format_climate will call recordClimate
#                                       instead now.
##############################################################################

. $(dirname $0)/set_climate_env.sh

cd $(dirname $0)

# Set the following so that `ps -eo pid,args ...` in killpid works in HP-UX.
export UNIX95=1

#
# This function will kill the process id of the "Climate is Running" gui
# (wait.tcl).  It is used throughout the scripts.
#
function killpid {
pid_func=`ps -eo pid,args | grep -E 'wish[ ].*wait.tcl' | awk '{print $1}'`
#echo "pid_func in killpid is $pid_func"
if test "$pid_func" != ""
then
   kill $pid_func 2> /dev/null  1> /dev/null
fi
}

#
# Set the file path environment variables
#

if [ "$CLIMATE_DIR" = "" ]
then
  echo "Fatal error in climate.sh: CLIMATE_DIR not defined in environment!  Exiting."
  exit 1
fi

CLIMATE_DATA_DIR=${CLIMATE_DIR}/data; export CLIMATE_DATA_DIR
CLIMATE_TMP_DIR=${CLIMATE_DIR}/tmp; export CLIMATE_TMP_DIR

if [ ! -d ${CLIMATE_DATA_DIR} ]
then
   echo "Directory ${CLIMATE_DATA_DIR} does not exist." 
   echo "Check installation and setup of Climate routines."
   echo "Halting climate.sh script execution .... "
   exit 1
fi

if [ ! -d ${CLIMATE_TMP_DIR} ]
then
   echo "Directory ${CLIMATE_TMP_DIR} does not exist." 
   echo "Check installation and setup of Climate routines."
   echo "Halting climate.sh script execution .... "
   exit 1
fi

#
# This starts the running clock GUI (wait.tcl) if it's a manual run. 
# If it's automatic run, it will start in display climate.
#

if test "$1" != "auto"
then
  ./wait.tcl &
fi

#
# Set wait.tcl's process id to a variable 'pid'.
#
pid=`ps -ef| grep wait| grep bin| awk '{print $2}'`

#
# Change to the temporary climate directory.
#
cd ${CLIMATE_TMP_DIR}

LOGFILE=${CLIMATE_TMP_DIR}/tmp.txt

# if old temporary script log file is there, delete it
if [ -f $LOGFILE ]
then
  rm -f $LOGFILE
fi

# Remove all other CLIMATE_TMP_DIR files.
rm -f ${CLIMATE_TMP_DIR}/*

#
# create temporary new log file for climate script/exe output
#
echo "Log file for climate.sh $1 $2 run on = `date` " > $LOGFILE
echo "" >> $LOGFILE
chmod 777 $LOGFILE

#
# Check that environment variable for path to climate executables is set
#
if [ "$CLIMATE_BIN_DIR" = "" ]
then
  echo "In climate.sh: CLIMATE_BIN_DIR not defined in environment!  Exiting." >> $LOGFILE
  if test "$1" != "auto"
  then
    $CLIMATE_BIN_DIR/error.tcl &
    killpid
  fi
  exit 1
else
  echo "In climate.sh: CLIMATE_BIN_DIR = $CLIMATE_BIN_DIR " >> $LOGFILE
fi

#
# Create new period.txt temp file holding AM or PM for the monitor controller.
#
echo "$1 $2" > period.txt
chmod 777 period.txt

#
# Test to see if the the hmMonitor is running (?)
#

#
# Set the waiting times for create_climate and format_climate.  The waiting
# times determine the amount of time the notification server will wait before
# sending back a message that the user didn't acknowledge the alert.
#
fileok="true"
if [ -s ${CLIMATE_DATA_DIR}/wait_times.txt ]
then
   if [ `wc -w <${CLIMATE_DATA_DIR}/wait_times.txt` -eq 2 ]
   then
      echo "Reading from ${CLIMATE_DATA_DIR}/wait_times.txt"  >> $LOGFILE
      read wait1 wait2 < ${CLIMATE_DATA_DIR}/wait_times.txt
      echo "wait1, wait2 = ${wait1}, ${wait2} after file read." >> $LOGFILE
      if [ ${wait1} -ge 0 -a ${wait2} -ge 0 ]
      then
         export DATA_WAIT_TIME=$wait1
         export PRODUCT_WAIT_TIME=$wait2
         echo "data wait = $DATA_WAIT_TIME"  >> $LOGFILE
         echo "product wait = $PRODUCT_WAIT_TIME" >> $LOGFILE
      else
         echo "Warning: Improper entries in wait_times.txt file." >> $LOGFILE
         echo "Must contain exactly two positive integer values." >> $LOGFILE
         fileok="false"
      fi
   else
      echo "Warning: Improper number of entries in wait_times.txt file." >> $LOGFILE
      echo "Must contain exactly two positive integer values." >> $LOGFILE
      fileok="false"
   fi
else 
   echo "Problem: ${CLIMATE_DATA_DIR}/wait_times.txt file not found." >> $LOGFILE
   fileok="false"
fi

if [ "$fileok" = "false" ]
then
   echo "Reverting to default times of 20 and 10 minutes:" >> $LOGFILE
   DATA_WAIT_TIME=20
   export DATA_WAIT_TIME
   PRODUCT_WAIT_TIME=10
   export PRODUCT_WAIT_TIME
   echo "data wait = $DATA_WAIT_TIME" >> $LOGFILE
   echo "product wait = $PRODUCT_WAIT_TIME" >> $LOGFILE
fi

#
# Copy the global file if it exists.  Notify the user and bail out if it doesn't.
#

if [ -s ${CLIMATE_DATA_DIR}/global_day ] 
then
   echo "Copying ${CLIMATE_DATA_DIR}/global_day" >> $LOGFILE
   cp ${CLIMATE_DATA_DIR}/global_day ${CLIMATE_TMP_DIR}
else
   echo "Missing or empty ${CLIMATE_DATA_DIR}/global_day file." >> $LOGFILE
   echo "Halting script execution ..... " >> $LOGFILE
   if test "$1" != "auto"
   then
     $CLIMATE_BIN_DIR/error.tcl &
     killpid
   fi
   exit 1
fi

#
# Check and run create_climate executable
#
echo "Starting create_climate ....... " >> $LOGFILE

# Make sure that the create_climate executable exists ....

if [ ! -s ${CLIMATE_BIN_DIR}/create_climate ]
then
   echo "${CLIMATE_BIN_DIR}/create_climate executable does not exist!" >> $LOGFILE
   echo "Halting climate.sh script execution .... " >> $LOGFILE
   if test "$1" != "auto"
   then
      $CLIMATE_BIN_DIR/error.tcl &
      killpid
   fi
   exit 1
else if [ ! -x ${CLIMATE_BIN_DIR}/create_climate ]
   then 
      echo "${CLIMATE_BIN_DIR}/create_climate does not have execute permission!" >> $LOGFILE
      echo "Halting climate.sh script execution .... " >> $LOGFILE
      if test "$1" != "auto"
      then
         $CLIMATE_BIN_DIR/error.tcl &
         killpid
      fi
      exit 1
     fi
fi

${CLIMATE_BIN_DIR}/create_climate $1 $2 >> $LOGFILE 2>> $LOGFILE

# Test the return status of this routine

create_status=$?

case $create_status in
     0)
           # Successful run of create_climate, and either data wait is 0,
           # or user responded to Edit Climate Data notification.
           # Must make a decision what to do here - continue or exit.
           #
           # If the data wait time is 0, we want to continue in this
           # script with display_climate, but without bringing
           # up its GUI.  Otherwise, this script should exit,
           # since control has been given to the script
           # display.sh based on user response to notification.
           #
           echo "create_climate returned a value of 0." >> $LOGFILE

	   if test "$1" != "auto" 
	   then
	      killpid
	   fi

           if [ ${DATA_WAIT_TIME} -eq 0 ]
           then
              # Treat this situation the same as a notification timeout.
              # Must reset create_status to non-zero for use as arg in call
              # to display_climate, to prevent displaying the editor.
              #
              echo "DATA_WAIT_TIME is 0 -- continuing in climate.sh w/o data editor." >> $LOGFILE
              create_status=1
	   else
	      ##############################################################
	      # Control will go over to display.sh, climate.sh exiting.
	      #
	      # But first set write permissions on all files in tmp directory
	      # except RecCli pipes.
	      ##############################################################

	      ls * | grep -v .dat |
	        while read fn
	        do
	           chmod a+w $fn
	        done

	      exit 0
           fi
           ;;
     1)
           # Successful run where wait > 0, but notification timed out without
           # user response.  Must continue in climate.sh w/o data editor.
           #
           echo "create_climate returned a value of 1."  >> $LOGFILE 
           echo "Continuing in climate.sh w/o displaying data editor." >> $LOGFILE
	   if test "$1" != "auto"
	   then
	      killpid
	   fi
           ;;
     3)
           # Some kind of error occurred in create_climate.
           echo "create_climate returned a value of 3." >> $LOGFILE
           echo "Halting the execution of climate.sh script." >> $LOGFILE
	   if test "$1" != "auto"
	   then
              $CLIMATE_BIN_DIR/error.tcl &
	      killpid
	   fi
	   exit 1
           ;;
     4)
           echo "create_climate returned a value of 4." >> $LOGFILE
           echo "User pressed Cancel during manual run." >> $LOGFILE
           echo "Halting the execution of climate.sh script." >> $LOGFILE
	   if test "$1" != "auto"
	   then
	      killpid
	   fi
	   exit 1
           ;;
     *)
           echo "create_climate returned an unexpected value of $create_status."
           echo "Halting the execution of climate.sh script." >> $LOGFILE
	   if test "$1" != "auto"
	   then
              $CLIMATE_BIN_DIR/error.tcl &
	      killpid
	   fi
	   exit 1
           ;;
esac


# If here, then create_status is not 0, so run display_climate without the GUI.
# Otherwise, this script should have exited above, and all the following
# would have been done by display.sh script called from hmMonitor.tcl.

echo "Starting display_climate in climate.sh $1 $2 ....... " >> $LOGFILE

# Make sure that the display_climate executable exists and can be run.

if [ ! -s ${CLIMATE_BIN_DIR}/display_climate ]
then
   echo "${CLIMATE_BIN_DIR}/display_climate executable does not exist!" >> $LOGFILE
   echo "Halting climate.sh script execution .... " >> $LOGFILE
   if test "$1" != "auto" 
   then
      $CLIMATE_BIN_DIR/error.tcl &
      killpid
   fi
   exit 1
else
   if [ ! -x ${CLIMATE_BIN_DIR}/display_climate ]
   then 
      echo "${CLIMATE_BIN_DIR}/display_climate does not have execute permission!" >> $LOGFILE
      echo "Halting climate.sh script execution .... " >> $LOGFILE
      if test "$1" != "auto" 
      then
         $CLIMATE_BIN_DIR/error.tcl &
         killpid
      fi 
      exit 1
   fi
fi

${CLIMATE_BIN_DIR}/display_climate $1 $2 $create_status >> $LOGFILE 2>>$LOGFILE

# Make sure that display_climate did its thing correctly

if [ $? -ne 0 ]
then
   echo "display_climate returned an error in climate.sh.  Stopping execution. " >> $LOGFILE
   if test "$1" != "auto" 
   then
      $CLIMATE_BIN_DIR/error.tcl &
      killpid
   fi
   exit 1
fi

echo "Returned from display_climate in climate.sh $1 $2, continuing..." >> $LOGFILE
echo "" >> $LOGFILE

# Copy the files generated by set_up_climate

cp -i ${CLIMATE_DATA_DIR}/*_$2_* ${CLIMATE_TMP_DIR}

if [ $? -ne 0 ]
then
   echo "The copy of the files generated by set_up_climate failed in climate.sh" >> $LOGFILE
   if test "$1" != "auto" 
   then
      $CLIMATE_BIN_DIR/error.tcl &
      killpid      
   fi
   
   exit 1
fi

#
# Check to see if the new ifps-ccc.env site file exists and if so, extract the
# necessary environment variables and set them for climate.  If unavailable,
# notify user and log errors.
#
echo "Getting WFO Name and local Time Zone from IFPS file." >> $LOGFILE
ifpsfile="/awips/adapt/ifps/localbin/ifps-${ICWF_SITE}.env"
ourcopy="${CLIMATE_TMP_DIR}/ifps-${ICWF_SITE}.env"
haveifps="true"

if [ -s $ifpsfile ]
then
   export IFPS_SITE_OFFICE_NAME
   export IFPS_SITE_TIMEZONE
   cp $ifpsfile $ourcopy
   IFPS_SITE_OFFICE_NAME=`grep "IFPS_SITE_OFFICE_NAME=" $ourcopy | cut -f2 -d= | sed -e "s/\"//g" | sed -e "s/'//g"`
   echo "IFPS_SITE_OFFICE_NAME = $IFPS_SITE_OFFICE_NAME" >> $LOGFILE
   IFPS_SITE_TIMEZONE=`grep "IFPS_SITE_TIMEZONE=" $ourcopy | cut -f2 -d=`
   echo "IFPS_SITE_TIMEZONE = $IFPS_SITE_TIMEZONE" >> $LOGFILE
   echo "" >> $LOGFILE

   if [ "$IFPS_SITE_OFFICE_NAME" = "" -o "$IFPS_SITE_TIMEZONE" = "" ]
   then
      echo "WARNING!  IFPS_SITE_OFFICE_NAME and/or IFPS_SITE_TIMEZONE not defined in file:" >> $LOGFILE
      echo "  ${ifpsfile}" >> $LOGFILE
      echo "NWWS Climate products will have wrong time zone and/or WFO name!" >> $LOGFILE
      echo "" >> $LOGFILE
      haveifps="false"
   fi
else
   haveifps="false"
   echo "" >> $LOGFILE
   echo "IFPS environment file $ifpsfile not found." >> $LOGFILE
   echo "NWWS Climate products will have wrong time zone and/or WFO name!" >> $LOGFILE
   echo "" >> $LOGFILE
fi

# Make sure the format climate executable exists and has execute permission.

if [ ! -s ${CLIMATE_BIN_DIR}/format_climate ]
then
   echo "${CLIMATE_BIN_DIR}/format_climate executable does not exist!" >> $LOGFILE
   echo "Halting script execution .... " >> $LOGFILE
   if test "$1" != "auto"
   then
      $CLIMATE_BIN_DIR/error.tcl &
      killpid
   fi
   exit 1
else if [ ! -x ${CLIMATE_BIN_DIR}/format_climate ]
     then
        echo "${CLIMATE_BIN_DIR}/format_climate does not have execute permission!" >> $LOGFILE
        echo "Halting script execution .... " >> $LOGFILE
        if test "$1" != "auto"
        then
           $CLIMATE_BIN_DIR/error.tcl &
	   killpid
	fi
        exit 1
     fi
fi

# Run format climate

echo "" >> $LOGFILE

if test "$1" != "auto"
then
   format_flag=1
   echo "Manual run, allowing Edit Product Data notification in format_climate," >> $LOGFILE
   echo "contingent on PRODUCT_WAIT_TIME being non-zero." >> $LOGFILE
else
   format_flag=0
   echo "Auto run, disallowing Edit Product Data notification in format_climate" >> $LOGFILE
   echo "since no DISPLAY available to climate processes running on DS." >> $LOGFILE
fi

echo "format_flag is $format_flag in climate.sh" >> $LOGFILE
echo "Starting format_climate in climate.sh $1 $2 ....... " >> $LOGFILE
echo "" >> $LOGFILE

${CLIMATE_BIN_DIR}/format_climate $1 $2 $format_flag 0 >> $LOGFILE 2>&1

format_status=$?

echo "" >> $LOGFILE
echo "Returned from format_climate in climate.sh" >> $LOGFILE
echo "" >> $LOGFILE

# Set write permissions on all files in tmp directory

ls * | grep -v .dat |
  while read fn
  do
     chmod a+w $fn
  done

echo "Climate has completed its run.  Reporting status:" >> $LOGFILE
echo "" >> $LOGFILE

# Check to make sure that format_climate ran to completion
#
case $format_status in
     0)
           echo "format_climate returned a value of 0, for success." >> $LOGFILE
	   echo "RecordClimate also returned a value of 0, for success." >> $LOGFILE

	   # Delete the two temporary .dat files used for RecordClimate
	   rm -f ${CLIMATE_TMP_DIR}/*.dat

           if [ "$haveifps" = "false" ]
           then
              echo "However, IFPS environment file problems were encountered, see preceding messages." >> $LOGFILE
              echo "Climate exiting with a report of an error due to this situation." >> $LOGFILE
           fi

           if test "$1" != "auto" 
           then
              if [ "$haveifps" = "false" ]
              then
                 $CLIMATE_BIN_DIR/error.tcl &
              else
	         success.tcl &
              fi
              killpid
           fi
	   ;;
     1)
           echo "format_climate returned a value of 1.  This means the program" >> $LOGFILE
	   echo "encountered an invalid input argument, was aborted by user, or" >> $LOGFILE
	   echo "had problems invoking a non-climate process." >> $LOGFILE
	   echo " - Check the format_climate log file for details." >> $LOGFILE

           if [ "$haveifps" = "false" ]
           then
              echo "Also, IFPS environment file problems were encountered, see preceding messages." >> $LOGFILE
              echo "Climate exiting with a report of an error due to this situation." >> $LOGFILE
           fi

	   if test "$1" != "auto" 
	   then
	      $CLIMATE_BIN_DIR/error.tcl &
              killpid
	   fi
	   ;;
     3)
           echo "format_climate returned a value of 3, fatal database (dis)connect error." >> $LOGFILE
           echo "Halting the execution of climate.sh script." >> $LOGFILE
	   if test "$1" != "auto" 
	   then
	      $CLIMATE_BIN_DIR/error.tcl &
              killpid
	   fi
	   exit 1
           ;;
     5)
           echo "format_climate ran successfully, but RecordClimate returned errors within format_climate.">>$LOGFILE
	   echo "Please see logfile for recordClimate under ${LOG_FILE}">>$LOGFILE

	   rm -f ${CLIMATE_TMP_DIR}/*.dat

	   if test "$1" != "auto"
	   then
	      $CLIMATE_BIN_DIR/error.tcl &
	      killpid
           fi
	   ;;
     *)
           echo "format_climate returned an unexpected value of $create_status." >> $LOGFILE
           echo "Halting the execution of climate.sh script." >> $LOGFILE
	   if test "$1" != "auto" 
	   then
	      $CLIMATE_BIN_DIR/error.tcl &
              killpid
	   fi
	   exit 1
           ;;
esac

exit 0
