#!/bin/sh

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


# Handy Test Case Utility
#
# Provides methods for performing testing on console based programs and scripts
#
# SOFTWARE HISTORY
# Date            Ticket#        Engineer    Description
# ------------    ----------     ----------- --------------------------
# Jun 26, 2008    1180           jelkins     Initial creation
#
# @author: jelkins
# @version: 1.0

COUNT=0
PASS=0

# Perform a test
#
# Execute TESTCMD with the given arguments and check for the expected output.
# Make sure to set TESTCMD with the command to execute during the test
#
# @param arg1: name of the test
# @param arg2: expected string someplace within the output of the command
# @param arg3: arguments to pass to configureTextProducts
testCase() {
        name=$1; shift
        expected=$1; shift
        
        args=$@
        
        COUNT=`expr $COUNT + 1`

        echo
        echo " === Test: $name"
        
        commandline="$TESTCMD $args"
        
        out=`$commandline 2>&1`
        
        test=`echo $out | grep "$expected"`
        
        if [ "$test" != "" ]; then
                echo " === Passed"
                PASS=`expr $PASS + 1`
        else
                echo $commandline
                echo $out
                echo " === Failed"
        fi
}

# Output the test results
testResults() {
        echo ""
        echo " === Results ================ "
        echo "     $PASS / $COUNT"
}

