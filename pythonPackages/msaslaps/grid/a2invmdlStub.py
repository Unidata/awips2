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

# Gets inventories of gridded data from the A-II database.  The data is output
# to stdout as ASCII.
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    2014-10-22      3599          nabowle        Initial modification. Convert to DAF.
#    2014-11-17      3599          nabowle        Fix call to get_args().
#

import argparse
import numpy
import sys

from awips.dataaccess import DataAccessLayer

def get_args():
    parser = argparse.ArgumentParser(conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="host",
                    help="EDEX server hostname (optional)", metavar="hostname")
    parser.add_argument("--srcId", action="store", dest="srcId", 
                    help="Unique alphanumeric name for gridded data source",
                    metavar="srcId")
    parser.add_argument("--varAbrev", action="store", dest="varAbrev", 
                    help="Variable abreviation", metavar="varAbrev")
    parser.add_argument("--lvlOne", action="store", dest="lvlOne", 
                    help="Level One value", metavar="lvlOne", type=float)
    parser.add_argument("--lvlTwo", action="store", dest="lvlTwo", 
                    help="Level Two value", metavar="lvlTwo", type=float)
    parser.add_argument("--lvlName", action="store", dest="lvlName", 
                    help="Master level name", metavar="lvlName")
    parser.add_argument("--mode", action="store", dest="mode", default="time",
                    help="Mode - time, plane, field, or fieldplane")
    return parser.parse_args()

def main():
    user_args = get_args()

    if user_args.host:
        DataAccessLayer.changeEDEXHost(user_args.host)

    req = DataAccessLayer.newDataRequest("grid")

    if not user_args.srcId:
        print >> sys.stderr, "srcId not provided"
        return
    req.addIdentifier("info.datasetId", user_args.srcId)
    
    if user_args.varAbrev:
        req.setParameters(user_args.varAbrev)
    if user_args.lvlName is not None:
        req.addIdentifier("info.level.masterLevel.name", user_args.lvlName)
    if user_args.lvlOne is not None:
        req.addIdentifier("info.level.levelonevalue", numpy.float64(user_args.lvlOne))
    if user_args.lvlTwo is not None:
        req.addIdentifier("info.level.leveltwovalue", numpy.float64(user_args.lvlTwo))

    mode = user_args.mode
    if mode not in ["time", "plane", "field", "fieldplane"]:
        print >> sys.stderr, "mode must be one of time, plane, field, or fieldplane."
        return

    msg = "";
    if mode == "time":
        times = DataAccessLayer.getAvailableTimes(req)
        for time in times:
            timeStr = str(time)
            if "--" in timeStr:
                timeStr = timeStr[0:-22] + ".0" + timeStr[-22:-1] + ".0" + timeStr[-1]
            msg += timeStr[0:19] + ".0" + timeStr[19:] + "\n"
    elif mode == "plane":
        levels = DataAccessLayer.getAvailableLevels(req)
        for level in levels:
            msg += level_to_string(level) + "\n"
    elif mode == "field":
        params = DataAccessLayer.getAvailableParameters(req)
        msg = "\n".join(params)
    else: #fieldplane
        params = DataAccessLayer.getAvailableParameters(req)
        for param in params:
            msg += param + ":\n"
            req.setParameters(param)
            levels = DataAccessLayer.getAvailableLevels(req)
            if levels:
                levelStr = []
                for level in levels:
                    levelStr.append(level_to_string(level))
                msg += " ".join(levelStr) + " \n"

    print msg.strip("\n")


def level_to_string(level):
    name = level.getMasterLevel().getName()
    lvlOne = str(level.getLevelonevalue())
    lvlTwo = str(level.getLeveltwovalue())
    msg = name + " " + lvlOne
    if lvlTwo not in ["None", "-999999.0"]:
        msg += " " + lvlTwo
    return msg

if __name__ == '__main__':
    main()
