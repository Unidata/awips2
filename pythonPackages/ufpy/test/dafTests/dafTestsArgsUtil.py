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

from ufpy.dataaccess import DataAccessLayer as DAL

import argparse
import os
import sys

#
# Utility methods to be used by DAF tests for parsing arguments.
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    02/05/16        4795          mapeters       Initial Creation.
#    
# 
#

def getParser():
    """
    Return an ArgumentParser for parsing the standard arguments: the host to run
    tests against and whether to display the data retrieved in the tests.
    """
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host", default="localhost",
                         help="EDEX server hostname",
                         metavar="hostname")
    parser.add_argument("-v", action="store_true", dest="verbose", default=False,
                         help="Display data output")
    return parser 

def handleArgs(args):
    """
    Handle the arguments specified in getParser().
    """
    DAL.changeEDEXHost(args.host)
    # Suppress stdout unless user-requested
    if not args.verbose:
        sys.stdout = open(os.devnull, "w")

def parseAndHandleArgs():
    """
    Parses and handles the arguments specified in getParser(). Use this
    method to handle arguments when no additional arguments need to be added.
    """
    handleArgs(getParser().parse_args())
