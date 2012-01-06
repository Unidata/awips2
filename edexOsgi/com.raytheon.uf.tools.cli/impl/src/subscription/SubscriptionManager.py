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

import conf.SMConfig as config
##############################################################################
# Class providing a command line interface to the EDEX Subscription Service
# (SubscribeSrv) end-point. The package design, is to allow this class to run
# as a command line executable. Execution is controlled via command line flags;
# the subscription script is be entered via standard input (STDIN). On Linux,
# this allows the use of file re-direction (<) to obtain the script from a file.
#
# Configuration Files:
#    There are two configuration files that determine the behavior of this script:
#       SiteConfig.py: contains site specific configuration data
#       SMCongfig.py:  contains application configuration data 
#
# Example usage:
#    SubscriptionManager.py -o add -t timer -p "0 * * * * ?" -r python -s MESSAGE "Hello from Omaha" < src/data/HelloWorld.py
#
#    In this example, the uEngine script is contained in "data/HelloWorld.txt".
#    "Hello from Omaha" will replace %MESSAGE%. The modified script will be
#    submitted to the "python" script runner on the EDEX server. The triggering
#    condition for the script is the cron expression indicating the script will
#    file every minute.
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    12/09/08        1709          mfegan         Initial Creation.
#    12/07/10        7656          cjeanbap       Retrieve environment variable.
#    0414/11         5163          cjeanbap       NWRWAVES Setting AFOS text triggers in AWIPS II
##############################################################################
class SubscriptionManager:
    def __init__(self,name='SubManager',args=None,script=""):
        self.script = ""
        self.args = args
        self.commands = None
        self.name = name

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
        
        return self.__validateArgs()

    # Validates the command line options. The command line is invalid in
    # the following cases:
    #    1. no operation is specified
    #    2. no product is specified (add,delete)
    #    3. substitution and file are exclusive
    #    4. commands can only be specified is a script file is specified
    #    5. a required command line option is missing
    #    6. a command line option has an invalid value
    #
    # returns:
    #   0: if the command line validates
    #   1: if the help flag is encountered
    # raises:
    #   ArgError if the command line does not validate
    def __validateArgs(self):
        # private validation functions
        def promptForOK(command,text):
            io = IO.InputOutput()
            msg = "'%s' operation as requested will %s" % (command,text)
            io.writeln(sys.stdout,msg)
            msg = "Confirm operation [Y|N]: "
            io.write(sys.stdout,msg)
            resp = io.readln(sys.stdin)
            return 'Y' == resp.upper()
            
        def validateOption(operation,key,values):
            if key in self.commands and not validArgument(self.commands.get(key),values):
                err = "For subscription maintenance; '%s' operation requires '%s' to be one of %s" % (mode, operation, key, values)
                raise CL.ArgError(err)
            return
        def validateArguments(commands,values):
            for key in commands.keys():
                if key not in values:
                    err = "For subscription maintenance; '%s' is not a valid argument" % (key)
                    raise CL.ArgError(err)
            return
        def validArgument(name,values):
            try:
                values.index(name)
                return True
            except:
                return False
        def countArgs(vals,cmds):
            cnt = 0
            for cmd in vals:
                if cmd in cmds:
                    cnt += 1
            return cnt

        # validate the operation
        if 'operation' not in self.commands:
            raise CL.ArgError("For subscription maintenance; must specify operation")
        else:
            oper = self.commands.get('operation')
            if not validArgument(oper,config.operations):
                raise CL.ArgError("For subscription maintenance; '" + oper +"' is not a valid operation")
        
        # common validation rules
        if 'substitution' in self.commands and 'file' in self.commands:
            raise CL.ArgError("For subscription maintenance; cannot specify substitution values for an external script.")
        if 'command' in self.commands and 'file' not in self.commands:
            raise CL.ArgError("For subscription maintenance; cannot specify command line arguments without specifying an external script")
        
        # operation validation rules
        # per operation validation rules
        oper = self.commands.get('operation')
        if oper == 'add':
            if 'update' in self.commands:
                raise CL.ArgError("For subscription maintenance; update values are invalid for 'add' operation")
            if 'index' in self.commands:
                raise CL.ArgError("For subscription maintenance; index specification is invalid for 'add' operation")
            cnt = countArgs(config.addread,self.commands)
            if cnt < 3:
                raise CL.ArgError("For subscription maintenance; 'add' operation requires all of " + str(config.addread))
            
            try:
                if 'runner' in self.commands and self.commands['runner'] == 'pil':
                    self.commands['runner'] = 'ldad'
                    
                if 'type' in self.commands and self.commands['type'] == 'pil':
                    self.commands['type'] = 'ldad'
                
                validateOption('add','runner',config.runners)
                validateOption('add','type',config.triggers)
            except:
                raise
        
        elif oper == 'read':
            if 'update' in self.commands:
                raise CL.ArgError("For subscription maintenance; update values are invalid for 'read' operation")
            if 'index' in self.commands:
                raise CL.ArgError("For subscription maintenance; index specification is invalid for 'read' operation")
            cnt = countArgs(config.addread,self.commands)
            if cnt == 0:
                raise CL.ArgError("For subscription maintenance; 'read' must specify a one of " + str(config.addread))

            try:
                if 'runner' in self.commands and self.commands['runner'] == 'pil':
                    self.commands['runner'] = 'ldad'
                    
                if 'type' in self.commands and self.commands['type'] == 'pil':
                    self.commands['type'] = 'ldad'
                
                validateOption('read','runner',config.runners)
                validateOption('read','type',config.triggers)
            except:
                raise
        
        elif oper == 'delete':
            # need to validate naked delete -- subscription -o delete
            if len(self.commands) == 1:
                ok = promptForOK(oper,"clear the entire subscriptions table")
                if not ok:
                    return 1
            # verify that commands with 'index' have no additional arguments.
            if 'index' in self.commands and len(self.commands) != 2:
                raise CL.ArgError("For subscription maintenance; 'delete' operation with an 'index' option cannot have additional options")

            if 'update' in self.commands:
                raise CL.ArgError("For subscription maintenance; update values are invalid for 'delete' operation")

            try:
                if 'runner' in self.commands and self.commands['runner'] == 'pil':
                    self.commands['runner'] = 'ldad'
                    
                if 'type' in self.commands and self.commands['type'] == 'pil':
                    self.commands['type'] = 'ldad'
                
                if 'trigger' in self.commands:
                    validateOption('delete','runner',config.runners)
                    validateOption('delete','type',config.triggers)
            except:
                raise

        elif oper == 'update':
            if 'index' not in self.commands:
                raise CL.ArgError("For subscription maintenance; index specification is required for 'update' operation")
            if 'update' not in self.commands:
                raise CL.ArgError("For subscription maintenance; update values are required for 'update' operation")
            try:
                validateArguments(self.commands,config.update)
            except:
                raise
        else:
            raise CL.ArgError("For subscription maintenance; " + oper + " is not a valid operation")
        return 0
    # determines is the requested operation includes a script to read.
    # The operation has a script to read is:
    #     1. the operation is 'add' and an external script file has not been specified.
    # Note: this method assumes the command line has been read and validated.
    #
    # args: NONE
    #
    # return: True if we should read the script
    def __hasScript(self):
        retval = False
        oper = self.commands.get('operation')
        if oper == 'add':
            retval = 'file' not in self.commands
        else:
            retval = False
        return retval

    # reads a script from standard input
    # the results of the read are stored in the 'script' attribute.
    def __readScript(self):
        if self.script == "":
            io = IO.InputOutput()
            io.setStream(sys.stdin)
            self.script = io.read()
        return
    
    # performs the steps necessary to submit a subscription request
    # return:
    #    0: indicates the execution was successful
    #    1: indicates the execution was unsuccessful
    def __processRequest(self):
        msg = self.__createMessage()
        service = config.endpoint.get('subscribe')   
        connection=str(os.getenv("DEFAULT_HOST", "localhost") + ":" + os.getenv("DEFAULT_PORT", "9581"))
        ch = CH.CommHandler(connection, service)
        ch.process(msg)
        
        if not ch.isGoodStatus():
            util.reportHTTPResponse(ch.formatResponse())
        
        retVal = self.__processResponse(ch.getContents())
        
        return retVal
    
    # Processes the request and reports the results. Data is
    # reported to standard output, errors are reported to standard
    # error.
    #
    # args:
    #   msg: the message returned from the server
    # return:
    #   0 if the message contained valid results, 0 otherwise
    def __processResponse(self,msg):
        psr = MSG.Message(False)
        psr.parse(msg)
        status = 0
        io = IO.InputOutput()
        # process the return message
        for prop in psr.getProperties():
            name = prop['name']
            value = prop['value']
            if name == 'STDERR':
                parts = value.split(':',2)
                if parts[0] == 'ERROR':
                    status = 1
                    value = parts[1]
                io.setStream(sys.stderr)
            else:
                io.setStream(sys.stdout)
            io.writeln(data=value)
        return status

    # Creates the subscription request message.
    
    # return:
    #   the request message
    def __createMessage(self):
        msg = MSG.Message(True)
        msg.initializeMessage(False)
        for key in self.commands:
            if key == 'mode':
                pass
            elif key == 'substitution':
                dict = util.convListToDict(self.commands.get(key))
                for sub in dict:
                    msg.addProperty(name='substitution',value=str(sub)+":"+str(dict.get(sub)))
            elif key == 'update':
                dict = util.convListToDict(self.commands.get(key))
                for sub in dict:
                    msg.addProperty(name='update',value=str(sub)+":"+str(dict.get(sub)))
            else:
                msg.addProperty(name=key,value=self.commands.get(key))
        if self.__hasScript():
            msg.addProperty(name='script',value=self.script)
        return msg.getXML()
    
    # main class method. Performs the subscription update request.
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
                util.printMessage(sys.stderr,"Canceled","Unable to process request -- exiting...")
                return status
        except Exception,e:
            util.printMessage(sys.stderr,"Error",repr(e))
            return 1
        
        # read the script from standard input if required
        if self.__hasScript():
            self.__readScript()

        # process the request
        return self.__processRequest()

##############################################################################
# default execution; allows the class to be run as an application
##############################################################################
if __name__ == "__main__":
    ue = SubscriptionManager()
    status = ue.execute()
    exit(status)