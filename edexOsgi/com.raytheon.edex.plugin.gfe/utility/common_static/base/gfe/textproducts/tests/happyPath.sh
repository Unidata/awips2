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

testCase "Help" "show this help message and exit" --help

testCase "Create" "110 text products created" OAX "$SCRIPT_PATH/formatted"

testCase "Delete" "110 text products deleted" OAX "$SCRIPT_PATH/formatted" -m DELETE

testCase "Delete Nothing" "0 text products deleted" OAX "$SCRIPT_PATH/formatted" -m DELETE

testCase "Info" "49 total PILs found" OAX . -m INFO

testCase "AllInfo" "6027 total PILs found" OAX . -m ALLINFO

testResults
