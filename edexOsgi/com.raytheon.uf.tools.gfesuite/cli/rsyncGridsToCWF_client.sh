#!/bin/sh
################################################################################
#
# Program name:  rsyncGridsToCWF_client.sh
#
# Executes rsynceGridsToCWF.sh locally or remotely as needed
#
# Author:       Juliya Dynina
#
# Revisions:
# Date            Ticket#       Engineer       Description
# ------------    ----------    -----------    -------------------------------
# 04/25/2012                    jdynina        Created Script
# 01/13/2015       #4013        randerso       Changed to work on any EDEX 
#                                              cluster server
################################################################################
if [ $# -lt 1 ] ;then
   echo Invalid number of arguments.
   echo Script stopped.
   echo ./rsyncGridsToCWF_client.sh wfo
   exit
fi

# ssh to ec which will actually go to one of the servers in the EDEX cluster
ssh ec "/awips2/GFESuite/bin/rsyncGridsToCWF.sh ${1}"
