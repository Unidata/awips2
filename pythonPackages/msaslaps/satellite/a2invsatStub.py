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

# Gets inventories of satellite data from the A-II database.  The data is output
# to stdout as ASCII.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2014-10-23      3601          nabowle        Initial modification. Convert to DAF.
#

import argparse
import numpy
import sys

from ufpy.dataaccess import DataAccessLayer

def get_args():
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                    help="EDEX server hostname (optional)", metavar="hostname")
    parser.add_argument("--sectorID", action="store", dest="sectorID", 
                    help="The sector ID.", metavar="sectorID")
    parser.add_argument("--physicalElement", action="store", dest="physicalElement", 
                    help="The physical element.", metavar="physicalElement")
    parser.add_argument("--creatingEntity", action="store", dest="creatingEntity", 
                    help="(optional) The creating entity", metavar="creatingEntity")
    
    return parser.parse_args()

def main():
    user_args = get_args()

    if user_args.host:
        DataAccessLayer.changeEDEXHost(user_args.host)

    req = DataAccessLayer.newDataRequest("satellite")

    if not user_args.sectorID or not user_args.physicalElement:
        print >> sys.stderr, "sectorID or physicalElement not provided"
        return
    req.setParameters(user_args.physicalElement)
    req.addIdentifier("sectorID", user_args.sectorID)
    
    if user_args.creatingEntity:
        req.addIdentifier("creatingEntity", user_args.creatingEntity)

    msg = "";
    times = DataAccessLayer.getAvailableTimes(req)
    for time in times:
        timeStr = str(time)
        msg += timeStr[0:19] + ".0" + timeStr[19:] + "\n"

    print msg.strip()

if __name__ == '__main__':
    main()
