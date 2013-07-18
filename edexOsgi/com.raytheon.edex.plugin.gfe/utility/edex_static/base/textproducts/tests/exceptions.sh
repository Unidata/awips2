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
