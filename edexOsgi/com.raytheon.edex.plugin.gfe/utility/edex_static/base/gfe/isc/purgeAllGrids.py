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
import string, sys, re, time, types, getopt, LogStream, fnmatch, os
import JUtil
import siteConfig
pytime = time
from com.raytheon.edex.plugin.gfe.smartinit import IFPDB
from com.raytheon.edex.plugin.gfe.server import GridParmManager

#--------------------------------------------------------------------------
# Main program purges all grids from a database.
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# Prints usage statement.
#--------------------------------------------------------------------------
def usage():
    ustr = """\
Usage: purgeAllGrids -h hostname -p rpcport -d databaseID
    -h hostname:    ifpServer host name
    -p rpcport:     rpc port number of ifpServer
    -d databaseID:  database identifier\n"""
    sys.stderr.write(ustr)

#--------------------------------------------------------------------------
# Parses command line options and sets internal flags
#--------------------------------------------------------------------------
def getOpts(argv):
    Options = {'host': None, 'port': None, 'databaseID': None, 'purgeAll':None}
    try:
        optl, args = getopt.getopt(argv[1:], "h:p:d:a:")
        for opt in optl:
            if opt[0] == '-h':
                Options['host'] = opt[1]
            elif opt[0] == '-p':
                Options['port'] = int(opt[1])
            elif opt[0] == '-d':
                Options['databaseID'] = opt[1]
            elif opt[0] == '-a':
                Options['purgeAll'] = opt[1]
    except:
        usage()

    if Options['host'] is None:
        Options['host'] = siteConfig.GFESUITE_SERVER
    
    if Options['port'] is None:
        Options['port'] = siteConfig.GFESUITE_PORT

    if Options['host'] is None or Options['port'] is None:
        usage()
        raise SyntaxError, "Command line error"

    return Options

#--------------------------------------------------------------------------
# process function
#--------------------------------------------------------------------------
def process(opts): 

    

    if opts['purgeAll'] is None:
        purgeDB(opts['databaseID'])
    else:
        dbList = JUtil.javaStringListToPylist(GridParmManager.getDbInventory(opts['purgeAll']).getPayload())
        for db in dbList:
            purgeDB(db)




def purgeDB(dbname):
    # get list of parms
    LogStream.logEvent("Purging all grids from: ", dbname)
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
def main(argv):
    argv = JUtil.javaStringListToPylist(argv)
    Options = getOpts(argv)
    LogStream.logEvent("PurgeAllGrids starting")
    process(Options)
    LogStream.logEvent("PurgeAllGrids finished")

