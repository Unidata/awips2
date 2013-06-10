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

import sys

import LogStream

from com.raytheon.edex.plugin.gfe.smartinit import IFPDB

#--------------------------------------------------------------------------
# Main program purges all grids from a database.
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# process function
#--------------------------------------------------------------------------
def process(dbname):
    LogStream.logEvent("Purging all grids from: ", dbname)

    # get list of parms
    db = IFPDB(dbname)
    parms = db.getKeys()

    # cycle through each parm, get inventory, and store None grid to
    # remove the grids
    for p in range(0, parms.size()):
        we = db.getItem(str(parms.get(p)))
        inv = we.getKeys()
        for i in range(0, inv.size()):
            we.removeItem(inv.get(i))
            
#--------------------------------------------------------------------------
# Main program
#--------------------------------------------------------------------------
def executeFromJava(databaseID):
    LogStream.logEvent("PurgeAllGrids starting")
    try:
        process(databaseID)
        LogStream.logEvent("PurgeAllGrids finished")
        sys.exit(0)
    except SystemExit:
        pass
    except:
        LogStream.logProblem("Caught exception\n", LogStream.exc())
    

