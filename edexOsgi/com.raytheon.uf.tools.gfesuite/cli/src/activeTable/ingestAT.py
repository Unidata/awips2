##
##

#
# Port of ingestAT code from AWIPS1
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ??/??/??                      wldougher      Initial Creation.
#    02/13/13        1447          dgilling       Re-ported to better match
#                                                 requestAT/sendAT.
# 
#


import argparse
import collections
import logging
import os
import sys

import MergeVTEC
from awips import TimeUtil
from awips import UsageArgumentParser



logging.basicConfig(format="%(asctime)s %(name)s %(levelname)s:  %(message)s", 
                    datefmt="%H:%M:%S", 
                    level=logging.INFO)
log = logging.getLogger('ingestAT')



class CaseInsensitiveStringSet(collections.Set):
    def __init__(self, iterable):
        self.__internalSet = frozenset(iterable)
        
    def __contains__(self, x):
        return x.upper() in (item.upper() for item in self.__internalSet)
    
    def __len__(self):
        return len(self.__internalSet)
    
    def __iter__(self):
        return iter(self.__internalSet)

class StoreDrtTimeAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        drtInfoTuple = TimeUtil.determineDrtOffset(values)
        setattr(namespace, self.dest, drtInfoTuple[0])

class ReadDeleteFileAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        with open(values, 'rb') as fd:
            fileData = fd.read()
            setattr(namespace, self.dest, fileData)
        os.remove(values)


#--------------------------------------------------------------------
# decode command line   ingestAT -f remoteActiveTable [-z drtMode]
#                                -a activeTableName [-n] -X xmlInfo
#                                -s site
#--------------------------------------------------------------------
def process_command_line():
    parser = UsageArgumentParser.UsageArgumentParser(prog='ingestAT', conflict_handler="resolve")
    parser.add_argument("-h", action="store", dest="serverHost",
                        required=True, metavar="serverHost")
    parser.add_argument("-p", action="store", type=int, dest="serverPort",
                        required=True, metavar="serverPort")
    parser.add_argument("-f", action="store", dest="fname",
                        required=True, metavar="remoteActiveTable")
    parser.add_argument("-z", action=StoreDrtTimeAction, dest="ztime",
                        metavar="drtMode")
    parser.add_argument("-a", action="store", dest="atName", 
                        choices=CaseInsensitiveStringSet(['OPERATIONAL', 'PRACTICE']), 
                        default='OPERATIONAL')
    parser.add_argument("-n", action="store_false", dest="makeBackups")
    parser.add_argument("-X", action=ReadDeleteFileAction, dest="xmlIncoming",
                        metavar="xmlInfo")
    parser.add_argument("-s", action="store", dest="site", metavar="siteID",
                        required=True)
    return parser.parse_args()

def main():
    options = process_command_line()
    log.debug("Command-line options: " + repr(options))
    
    try:
        inputIsGZIP = True
        removeRemoteFile = False
        fromIngestAT = True
        MergeVTEC.merge(options.serverHost, options.serverPort, options.site,
          removeRemoteFile, options.fname, options.atName, inputIsGZIP, 
          options.ztime, options.makeBackups, options.xmlIncoming, 
          fromIngestAT)
    except:
        log.exception("MergeVTEC fail: ")
        
    try:
        os.remove(options.fname)
    except OSError:
        pass


if __name__ == '__main__':
    main()

