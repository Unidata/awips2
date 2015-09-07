#!/bin/sh
################################################################################
#                                                                              #
# Program name:  rsyncGridsToCWF_client.sh                                     #
#                                                                              #
# Executes rsynceGridsToCWF.sh locally or remotely as needed                   #
#                                                                              #
# Author:       Juliya Dynina                                                  #
#                                                                              #
# Revisions:                                                                   #
# 04/25/12:  Created Script                                                    #
################################################################################
if [ $# -lt 1 ] ;then
   echo Invalid number of arguments.
   echo Script stopped.
   echo ./rsyncGridsToCWF_client.sh wfo
   exit
fi

host_name=`hostname`
host_name=`echo $host_name | cut -c1-3`

/awips2/GFESuite/bin/rsyncGridsToCWF.sh ${1}
