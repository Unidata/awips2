#!/bin/sh
################################################################################
#
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
##############################################################################
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
# 07/15/2015       #4013        randerso       Changed to use a thrift request that can
#                                              be handled on any EDEX cluster server to 
#                                              run rsyncGridsToCWF.sh
#
################################################################################

# this allows you to run this script from outside of ./bin
path_to_script=`readlink -f $0`
RUN_FROM_DIR=`dirname $path_to_script`

BASE_AWIPS_DIR=`dirname $RUN_FROM_DIR`

# get the base environment
source ${RUN_FROM_DIR}/setup.env

# setup the environment needed to run the the Python
export LD_LIBRARY_PATH=${BASE_AWIPS_DIR}/src/lib:${PYTHON_INSTALL}/lib
export PYTHONPATH=${RUN_FROM_DIR}/src:$PYTHONPATH

# execute the rsyncGridsToCWF Python module
_PYTHON="${PYTHON_INSTALL}/bin/python"
_MODULE="${RUN_FROM_DIR}/src/rsyncGridsToCWF/rsyncGridsToCWF.py"

# quoting of '$@' is used to prevent command line interpretation 
$_PYTHON $_MODULE "$@"

