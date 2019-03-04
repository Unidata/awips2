#!/bin/sh

##
##

# Test configureTextProducts.py command line interface
#
# This script tests the functionality of the configureTextProducts.py interface
#
# SOFTWARE HISTORY
# Date            Ticket#        Engineer    Description
# ------------    ----------     ----------- --------------------------
# Jun 26, 2008    1180           jelkins     Initial creation
#
# @author: jelkins
# @version: 1.0

# ---- Import ----------------------------------------------------------------
SCRIPT_PATH=`dirname "$0"`
. $SCRIPT_PATH/testUtil.sh

TESTCMD="./$SCRIPT_PATH/../configureTextProducts.py"

testCase "No arguments" "incorrect number of arguments"

testCase "One argument" "incorrect number of arguments" OAX      

testCase "Invalid Site" "invalid site" invalidSite .

testCase "Invalid A2A File" "invalid a2afile" OAX . -a invalidA2A

testCase "Invalid Template" "invalid template" OAX . --template invalidTemplate

testCase "Invalid Mode" "incorrect mode" OAX . -m invalidMode

testResults
