#!/usr/bin/env python

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

"""Generate site specific text products.

This script is run at install time to customize a set of the text products 
for a given site.

SOFTWARE HISTORY
Date            Ticket#        Engineer    Description
------------    ----------     ----------- --------------------------
Jul 09, 2008    1222           jelkins     Split command line loader from class
Jan 26, 2015    4033           randerso    Fix logging broken by #3685

@author: jelkins
"""
__version__ = "1.0"

from sys import argv
from os.path import basename
from os.path import dirname
from os.path import abspath
from os.path import join

from ufpy import ThriftClient
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.request import ConfigureTextProductsRequest

SCRIPT_DIR = abspath(dirname(argv[0]))

# ---- Setup Logging ----------------------------------------------------------
import logging
from time import strftime, gmtime
timeStamp = strftime("%Y%m%d", gmtime())
logFile = '/awips2/edex/logs/configureTextProducts-'+timeStamp+'.log'

LOG = logging.getLogger("configureTextProducts")
LOG.setLevel(logging.DEBUG)
handler = logging.FileHandler(logFile)
handler.setLevel(logging.DEBUG)
formatter = logging.Formatter("%(levelname)-5s %(asctime)s [%(process)d:%(thread)d] %(filename)s: %(message)s")
handler.setFormatter(formatter)
for h in LOG.handlers:
    LOG.removeHandler(h)
LOG.addHandler(handler)

class ConfigureTextProducts:
    """Command Line Interface for the TextProductsGenerator
    """ 

    USAGE = """Usage: %prog [OPTIONS...] SITE_ID [DESTINATION_DIR]
    
    This script automatically configures the GFESuite set of text formatters 
    based on an afos2awips PIL/WMOID table and a given SITE_ID.  Text formatters
    are placed into the given DESTINATION_DIR.
    
    For example: %prog OAX ~/awips/edex/opt/data/utility/cave_static/site/OAX/gfe/userPython/textProducts"""

    def __init__(self):
        """Class constructor
        
        This constructor initializes the OptionParser
        """
        from optparse import OptionParser
        
        self.__optionParser = OptionParser(ConfigureTextProducts.USAGE)
        
        self.programName = self.__optionParser.get_prog_name()

    def main(self):
        """System entry point.
        
        Executes this script from the command line.
        
        @type args: list
        @param args: contains the commands used to launch the script
        """
        
        
        # get the command line options
        (option, arg) = self.__parseOptions()
        
        request = ConfigureTextProductsRequest()
        request.mode = option.mode.lower()
        request.template = option.template
        request.site = arg[0]
        if (len(arg) > 1):
            request.destinationDir = arg[1]
        else:
            request.destinationDir = None
        
        response = None
        try:
            thriftClient = ThriftClient.ThriftClient(option.host)
            response = thriftClient.sendRequest(request)
        except Exception, e:
            self.__error(e, 2)
        
        if response is not None and \
           response.msg is not None and \
           not "" == response.msg:
            self.__error(response.msg, 2)
            
        ## OLD CODE
        # --- setup the product generator ----------------------------------
#        from Generator import Generator
#        
#        try:
#            productGenerator = Generator(arg[0],arg[1],LOG)
#        except LookupError:
#            self.__error("invalid site: %s" % arg[0],2)
#        except OSError, (number,detail):
#            self.__error("invalid destination (%s): %s" % (detail,arg[1]), 2)
#        
#        if option.template is not None:
#            try:
#                productGenerator.setTemplate(option.template)
#            except IOError, detail:
#                self.__error("invalid template (%s): %s" % (detail,option.template),2)
#        
#        # --- run the product generator -------------------------------------
#        try:
#            exec "productGenerator."+option.mode.lower()+"()"
#        except AttributeError:
#            self.__optionParser.error(
#            "incorrect mode: %s \n Try `%s --help' for more information."
#            % (option.mode,self.programName))
        ## END OLD CODE

    # ---- Private Utility Functions ----------------------------------------

    def __error(self,message,status):
        """Output an error message and exit to the system
        
        Generates non-usage type errors.  For usage errors use the
        __optionParser.error() method
        
        @param message: a message to output before exiting to the system
        @type message: string
        
        @param status: a number to return to the system on exit
        @type status: number
        """
        
        from sys import exit
        
        LOG.error(message)
        exit(status)

    def __parseOptions(self):
        """Parse command line options and arguments
        
        Setup usage, options, and check for required number of arguments
        
        @return: a tuple of options and arguments
        @rtype: tuple 
        """
        
        self.__optionParser.add_option("--template",metavar="DIR",
                                help="base template DIR")
        
        self.__optionParser.add_option("-m", "--mode",
                                default="CREATE",
                                help="create, delete, or display a PIL/WMOID table"
                                )
        
        self.__optionParser.add_option("--host",
                                       default="localhost",
                                       help="the machine to configure (defaults to localhost)")
        
        from optparse import OptionGroup
        
        modeGroup = OptionGroup(self.__optionParser, "MODE is one of the following")
        
        modeGroup.add_option("-- CREATE", action="store_true",
                               help="creates the formatter templates [default]")
        
        modeGroup.add_option("-- DELETE", action="store_true",
                               help="deletes the formatter templates")
        
        modeGroup.add_option("-- INFO", action="store_true",
                               help="list the PIL/WMOID information for this SITE")
        
        modeGroup.add_option("-- ALLINFO", action="store_true",
                               help="list the PIL/WMOID information for all sites")
        
        self.__optionParser.add_option_group(modeGroup)
        
        (options, args) = self.__optionParser.parse_args()
        
        progName = self.__optionParser.get_prog_name()
        
        if len(args) < 1:
            self.__optionParser.error("incorrect number of arguments\n"+
                               "Try `"+progName+" --help' for more information.")
            
        return (options, args)
    
# check if run from the command line
if __name__ == "__main__":
    
    ConfigureTextProducts().main()
