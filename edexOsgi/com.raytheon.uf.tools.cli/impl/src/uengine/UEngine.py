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
import os
import sys
import types

import lib.CommandLine as CL
import lib.InputOutput as IO
import lib.CommHandler as CH
import lib.Message as MSG
import lib.Util as util

import conf.UEConfig as config

##############################################################################
# Class providing a command line interface to the EDEX Micro Engine (uEngine)
# script runner. The package design, is to allow this class to run as a command
# line executable. Execution is controlled via command line flags; the uEngine
# script is entered via standard input (STDIN). On Linux, this allows the use
# of file re-direction (<) to obtain the script from a file.
#
# Configuration Files:
#    There are two configuration files that determine the behavior of this script:
#       SiteConfig.py: contains site specific configuration data
#       UECongfig.py:  contains application configuration data 
#
# Example usage:
#    UEngine.py -r python -s MESSAGE "Hello from Omaha" < data/HelloWorld.txt
#
#    In this example, the uEngine script is contained in "data/HelloWorld.txt".
#    "Hello from Omaha" will replace %MESSAGE%. The modified script will be
#    submitted to the "python" script runner on the EDEX server.
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/28/08        1585          MW Fegan       Initial Creation.
#    12/09/08        1709          MW Fegan       Moved subscription handling
#                                                  to separate class.
#    12/09/10        7656          cjeanbap       Retrieve environment variable.
#    03/18/11        NCEP230       mgamazaychikov Added handling of fullMessage flag
##############################################################################
class UEngine:
    def __init__(self,name='uEngine',args=None,script=""):
        self.script = ""
        self.args = args
        self.commands = None
        self.name = name

    # performs macro expansion style substitution on the script
    #
    # args:
    #   dict:   dictionary containing the substitution key:value pairs
    #   script: the script to modify
    # return:
    #   the script following replacement
    def __transform(self,dict,script):
        retVal = script
        for key in dict.keys():
            retVal = retVal.replace("%"+key+"%",dict.get(key))
        return retVal

    # reads a script from standard input
    # the results of the read are stored in the 'script' attribute.
    def __readScript(self):
        # read the script
        if self.script == "":
            io = IO.InputOutput()
            io.setStream(sys.stdin) 
            text = io.read()
        else:
            text = self.script
        # perform any substitutions
        if 'substitution' in self.commands:
            list = self.commands.get('substitution')
            dict = util.convListToDict(list)
            self.script = self.__transform(dict,text)
        else :
            self.script = text

    # reads the commend line and performs any validation
    #
    # returns:
    #   0: if the command line validates
    #   1: if the help flag is encountered
    # raises:
    #   ArgError if the command line does not validate
    def __readCommandLine(self):
        # parse the command line
        cl = CL.CommandLine(self.name,config.flags)
        if self.args != None:
            cl.setArgs(self.args)
        else:
            cl.setArgs(sys.argv[1:])
        
        cl.parse()
        if cl.isHelp():
            cl.usage()
            return 1
        
        cl.mergeListsToString()
        self.commands = cl.getCommands()

        # validate the command line arguments
        return self.__validateCommands()
    
    # Validates the command line options when executed in 'runner' mode.
    # In 'runner' mode, the command line is invalid in the following cases:
    #    1. no runner is specified
    #    2. an operation is specified
    #    3. a file name is specified
    #    4. command line arguments are provided
    #
    # returns:
    #   0: if the command line validates
    # raises:
    #   ArgError if the command line does not validate
    def __validateCommands(self):
        if 'runner' not in self.commands:
            raise CL.ArgError("Must specify a 'runner' flag for uEngine script execution")
        if 'operation' in self.commands:
            raise CL.ArgError("Cannot specify an 'operation' flag for uEngine script execution")
        if 'file' in self.commands:
            raise CL.ArgError("Cannot specify a 'file' flag for uEngine script execution")
        if 'command' in self.commands:
            raise CL.ArgError("Cannot specify an 'command' flag for uEngine script execution")
        return 0
        
    
    # Retrieves the response from the message returned from the uEngine service.
    # The responses are returned as a new line delineated string. The format
    # of the string is:
    #     [Error] Response:
    #     <reply tag 1>
    #     <reply tag 2>
    #
    # args:
    #   msg:   Message object containing uEngine response
    # return:
    #   [status,header,response]
    #     status:   True if the response represents an error, False otherwise
    #     header:   A header to use when displaying the result
    #     response: the response as a new-line separated string
    # 
    def __getResponse(self,msg):
        path = config.response
        for resp in config.responses:
            if msg.checkResponseType(resp):
                dict = config.responses[resp]
                error = dict['error']
                retval = msg.getResponse(dict['tag'],dict['name'],dict['incTag'])
                if 'optional' in dict:
                    retval.extend(msg.getResponse(dict['tag'],dict['optional']))
                header = ""
                if error:
                    header = "Error "
                header += "Response:"
                return error, header, retval
        return True,"Null Response:", "No Data Returned From Server"

    # performs the steps necessary to execute the uEngine script
    #
    # return:
    #    0: indicates the execution was successful
    #    1: indicates the execution was unsuccessful
    def __runScript(self):
        io = IO.InputOutput()
        # pass the script to EDEX
        runner = self.commands.get('runner')
        service = config.endpoint.get(runner)
        
        # submit the input to the server and obtain result
        connection=str(os.getenv("DEFAULT_HOST", "localhost") + ":" + os.getenv("DEFAULT_PORT", "9581"))
        ch = CH.CommHandler(connection,service)
        ch.process(self.script)
        
        # expect message 200, if not print error message and return error code
        if not ch.isGoodStatus():
           util.reportHTTPResponse(ch.formatResponse())
           return 1
        if 'fullMessage' in self.commands:
            # Return the full XML message to the appropriate stream
            io.setStream(sys.stdout)
            io.writeln(data=ch.getContents())
            retVal = 0
        else:
            # Pull the responses element out of the xml
            msg = MSG.Message()
            msg.parse(ch.getContents())
            # process the response and send results to the appropriate stream
            error,hdr,resp = self.__getResponse(msg)
            if error:
                io.setStream(sys.stderr)
                retVal = 1
            else:
               io.setStream(sys.stdout)
               retVal = 0
            io.writeln(data=hdr)
            io.writeln(data=resp)
        return retVal    
    
    # main class method. Perform the actual script running.
    #
    # return:
    #    0: indicates the execution was successful
    #    1: indicates the execution was unsuccessful
    def execute(self):
        status = 0
        # parse the command line
        try:
            status = self.__readCommandLine()
            if status != 0:
                exit(status)
        except Exception,e:
            util.printMessage(sys.stderr, "Error", repr(e))
            return 1
        
        # read the script from standard input
        # and perform any substitution
        self.__readScript()

        # process the request
        status = self.__runScript()

        # return the status
        return status

##############################################################################
# default execution; allows the class to be run as an application
##############################################################################
if __name__ == "__main__":
    ue = UEngine()
    status = ue.execute()
    exit(status)
