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
##############################################################################
# Contains configuration information specific to the Subscription Manager tool
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#                                                 Initial Creation.
#    Feb 23, 2016    4716          rferrel        Added help for AWIPS commands.
##############################################################################

##
import os
import sys

import lib.CommandLine as CL
import lib.InputOutput as IO
import lib.Util as util
import subscription.SubscriptionManager as SM

import conf.TDBConfig as config
import collections

from awips import ThriftClient
from dynamicserialize.dstypes.com.raytheon.uf.common.message import Message, Header
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.text.dbsrv import TextDBRequest

##############################################################################
# General exception to be raised when errors occur when processing messages
##############################################################################
class MessageError(Exception):
    def __init__(self,value,cause=None):
          self.value = value
          self.cause = cause
    def __str__(self):
        msg = 'MessageError: ' + repr(self.value)
        if self.cause is not None:
            msg += "\n caused by " + repr(self.cause)
        return msg
    def __repr__(self):
        return self.__str__()

# appends value to list assoticated with key in multimap if the key isn't
# present in the map. map must be defaultdict so that list is auto created
def appendIfNotPresent(multimap, key, value):
    if key not in multimap:
        multimap[key].append(value)


##############################################################################
# Class implementing the text database (textdb) Command Line Interface (CLI)
# tool.
#
# Configuration Files:
#    There are two configuration files that determine the behavior of this script:
#       SiteConfig.py: contains site specific configuration data
#       TDBCongfig.py: contains general configuration data for textdb 
#
# Example Usage:
#     textdb.py -s -r NE
#
#     echo "This is a test." | textdb.py write CCCNNNXX
#
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/28/08        1584          MW Fegan       Initial Creation.
#    12/09/08        1709          MW Fegan       Submit scripts via subscription manager.
#    06/09/10        2187          cjeanbap       Added operational mode functionality.
#    08/02/10        2187          cjeanbap       Added additional operational mode
#                                                 functionality.
#    10/01/10        6338          cjeanbap       Added functionality for site node.
#    10/29/10        7354          cjeanbap       Updated if-statement to handle None
#    12/07/10        7656          cjeanbap       Retrieve environment variable.
#    04/07/11        8686          cjeanbap       Fixed $ACTION has -i associated
#    05/12/14       16954          kshrestha      Added Multiple flag functionality for textdb
#    08/15/14        2926          bclement       Fixed hasSubOperations()
#    08/22/14        2926          bclement       Switched to ThriftClient
#    09/05/14        2926          bclement       Moved TextDBRequest to common
#    02/05/16        5269          skorolev       Removed Handles the Watch Warn Trigger requests
##############################################################################
class TextDB:

    # initializer.
    def __init__(self):
        self.commands = None
        self.xml = ""
        
    # prints the "official" usage message
    def __usage(self):
        util.printMessage(sys.stderr,body=config.USAGE_MESSAGE)

    # prints the AFOS Commands summary
    def __afosCmds(self):
        util.printMessage(sys.stderr,body=config.AFOS_CMDS)

    # prints the AWIPS Commands summary
    def __awipsCmds(self):
        util.printMessage(sys.stderr,body=config.AWIPS_CMDS)

    # determines if the command line specifies an operation that requires
    # reading the product from standard input
    def __hasProduct(self):
        if 'stdin' in self.commands:
            return True
        return False

    # Determines if the command line specifies an operation that requires
    # processing an 'ldad' script request. 
    def __isScriptRequest(self):
        for item in config.ldadcmds:
            if item in self.commands:
                return True
        return False

    # determines if the command is a legacy command, i.e. 'read' or 'write'
    #
    # return:
    #    True if the command is a legacy command,
    #    False otherwise
    def __isLegacyCommand(self):
        if CL.DEFAULT_KEY in self.commands:
            if len(self.commands.get(CL.DEFAULT_KEY)) > 1:
                return True
        return False
    
    # Determines if the command line specifies an operation that requires
    # processing trigger script request. 
    def __isTriggerScriptRequest(self):
        for item in config.triggercmds:
            if item in self.commands:
                return True
        return False  

    # Attempts to transform the legacy command into a valid flag based
    # command.
    #
    # raises:
    #    InputOutputError if an error occurs. 
    #
    # TODO: fix to allow for multiple commands
    def __correctLegacyCommand(self):
        try:
            default = self.commands.pop(CL.DEFAULT_KEY)
            command = default.pop(0)
            command = config.flags.get(command)[0]
            self.commands[command] = default
        except Exception as e:
            raise IO.InputOutputError('Unable to correct legacy command line',e)
    
    # Removes the 'default' key from self.commands. If this results in an empty
    # commands dictionary, a 'help' command is added.
    def __removeDefault(self):
        self.commands.pop(CL.DEFAULT_KEY,None)
        if len(self.commands) == 0:
            self.commands[CL.HELP_KEY] = []

    # This method merges data lists into space separated strings.
    # It updates entries in self.commands. 
    def __mergeData(self):
        for key in self.commands.keys():
            try:
                config.mergeData.index(key)
                self.commands[key] = [" ".join(self.commands[key])]
            except Exception as e:
                # nothing to do ...
                # simply means the key isn't in config.mergeData
                pass

    # Checks to see if the number of commands obtained from the command
    # line is appropriate. The number of commands is appropriate if one
    # or more of the following conditions is met:
    #    1) there is at least one command.
    #    2) there is a single command.
    #    3) one of the commands is the 'help' command.
    #    4) all of the commands are specified in config.mayJoin.
    # It is assumed that the commands are in self.commands.
    #
    # raise:
    #    ArgError if the number of commands is not appropriate
    def __checkCommandCount(self):
        if self.commands == None:
            raise CL.ArgError("Invalid command count - NULL command line provided.")
        count = len(self.commands)
        if count == 0:
            raise CL.ArgError("Invalid command count - Empty command line provided.")
        elif count == 1:
            for key in self.commands.keys():
                length = len(self.commands.get(key))
                if key == CL.HELP_KEY:
                    return
                elif key is CL.DEFAULT_KEY:
                    if length < 2:
                        raise CL.ArgError("Invalid command count - legacy command requires argument")
                elif key in config.mergeData:
                    return
                else:
                    # determine if the command has 1 ... n args
                    args = config.message.get(key,{})[config.MSG_ARGS][config.MSG_VALUE]
                    if args == -1:
                        if length == 0:
                            raise CL.ArgError("Invalid command count - '" + key + 
                                              "' requires at least one argument")
                    else:
                        if args == -2:
                            return
                        elif length != args:
                            raise CL.ArgError("Invalid command count - '" + key + "' requires " + 
                                              str(args) + " args, " + str(length) + " supplied")
            return
        for command in self.commands.keys():
            try:
                config.mayJoin.index(command)
            except:
                raise CL.ArgError("Invalid command count - JOIN command includes invalid option(s)")
        return

    # Reads the product value from standard input. The text read is added
    # to the commands dictionary.
    #
    # raises:
    #   InputOutputError if an error occurs
    def __readProduct(self):
        try:
            io = IO.InputOutput()
            io.setStream(sys.stdin)
            text = io.read()
            for stdin in self.commands['stdin']:
                self.commands[stdin].append(text)
        except Exception as e:
            raise IO.InputOutputError("Unable to read product from standard input",e)

    # Generates the request message to be sent to the EDEX server.
    #
    # return:
    #    the generated message in a serializable object
    # raises:
    #    MessageError if any error occurs
    def __generateRequestMessage(self):
        try:
            multimap = collections.defaultdict(list)
            commands = self.commands['command']
            isJoin = False
            for command in commands:
                data = self.commands[command]
                message = config.message[command]
                args = message[config.MSG_ARGS][config.MSG_VALUE]
                for tuple in message[config.MSG_START:]:
                    key = tuple[config.MSG_KEY]
                    value = tuple[config.MSG_VALUE]
                    if ((key == "SUBOP") and (value == "JOIN")):
                        isJoin = True
                    if isinstance(value,int):
                        if args == config.MSG_VAR_ARGS:
                            for item in data:
                                multimap[key].append(item)
                        else:
                            l = len(commands)
                            if (l == 1) or (l == 3):
                                if ((key == "SITE") and (self.commands.get("site_node") is not None) and (len(self.commands.get("site_node")) == 0)):
                                    val = str(os.getenv("sitename"))
                                    appendIfNotPresent(multimap, key, val)
                                elif(isJoin == True):
                                    appendIfNotPresent(multimap, key, data)
                                    isJoin = False
                                else:
                                    appendIfNotPresent(multimap, key, data[value])
                            else:
                                appendIfNotPresent(multimap, key, data)
                    else:
                        appendIfNotPresent(multimap, key, value)
            operationalMode = os.getenv("OPERATIONAL_MODE", 'TRUE')
            multimap['operational'].append(operationalMode)
            return Message(header=Header(multimap=multimap))
        except Exception as e:
            raise MessageError('unable to create message for textdb request',e)

    # reads the command line and sets up the command data structure
    #
    # raises:
    #    ArgError if an error occurs
    def __readCommandLine(self):
        # parse the command line
        cl = CL.CommandLine('textdb',config.flags,True)
        args = self.__correctVersionRequest(sys.argv)
        args = self.__correctLdadRequest(args)
        cl.setArgs(args[1:])
        cl.parse()

        self.commands = cl.getCommands()
        try:
            self.__checkCommandCount()
            if self.__hasSubOperations():
                self.__correctSubOpCommandLine()
        except Exception as e:
            raise CL.ArgError("Invalid command line - contains inappropriate combination of operations",e)

        # check for and morph a "legacy" command line
        if self.__isLegacyCommand():
            self.__correctLegacyCommand()
        else:
            self.__removeDefault()

        # some of the commands require "merged" data. 
        self.__mergeData()
        
        # validate/modify commands as needed
        # add a 'command' key pointing to the command line entry
        commands = []
        for command in self.commands.keys():
            commands.append(command) 
        self.commands['command'] = commands
        
        # add a 'read_product' dictionary entry if the command includes a product
        # The commands that have products are defined in config.products
        for key in config.products:
            self.commands['stdin'] = []
            for command in self.commands.keys():
                if command == key:
                    self.commands['stdin'].append(command)
        if len(self.commands['stdin']) == 0:
            self.commands.pop('stdin')

        # add a 'runner' dictionary entry
        self.commands['runner'] = 'textdb'
#    10/28/08        1585          mfegan         Initial Creation.
#    01/14/09        1652          mfegan         Converted to BASH script.
#    06/05/09        2453          mfegan         Remove "./" paths.
#    12/21/09        2493          mfegan         Remove export of LD_PRELOAD.

    # Processes the request and reports the results. Data is
    # reported to standard output, errors are reported to standard
    # error.
    #
    # args:
    #   msg: the message returned from the server
    # return:
    #   0 if the message contained valid results, 0 otherwise
    def __processRequestResponse(self,msg):
        status = 0
        io = IO.InputOutput()
        # process the return message
        for prop in msg.getHeader().getProperties():
            name = prop.getName()
            value = prop.getValue()
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

    # Submits the request to the server and waits for a response.
    #
    # args:
    #   msg: the message to submit to the server
    # return:
    #   the message received from the from the server,
    #   or Null if a connection error occurred.
    # raise:
    #   ThriftRequestException if any error occurred.
    def __submitRequestMessage(self,msg):
        # send the request to the server
        host = os.getenv("DEFAULT_HOST", "localhost")
        port = os.getenv("DEFAULT_PORT", "9581")
        tClient = ThriftClient.ThriftClient(host, port)
        req = TextDBRequest()
        req.setMessage(msg)
        return tClient.sendRequest(req)

    # Handles the LDAD (script) requests by
    #   1. morphing the command line to match the micro-engine command line
    #   2. passing the modified command line to the micro-engine client
    #
    # return:
    #    returns the result of executing the micro-engine client
    # raise:
    #    propagates any exception received
    def __handleScriptRequest(self):
        cmd = self.commands.get('command')[0]
        fmt = config.convldad.get(cmd)
        args = self.commands.get(cmd)
        if (config.ldadput == cmd or config.ldaddel == cmd):
            if (len(args) == 2):
                args[1] = "|".join(args[1].split(' '))
            if (len(args) >=3):
                args = [args[0],"|".join(args[1:])]
        if (config.ldadput == cmd):
            args.append('%TRIGGER%')
        cline = (fmt % tuple(args)).split(' ')
        if (config.ldadput == cmd):
            temp = cline[len(cline)-3]
            cline[len(cline)-3] = " ".join(temp.split('|'))
        if (config.ldaddel == cmd):
            temp = cline[len(cline)-1]
            cline[len(cline)-1] = " ".join(temp.split('|'))

        sm = SM.SubscriptionManager(name='textdb',args=cline)
        return sm.execute()

    #
    #
    #
    def __deleteSubscriptions(self):
        cline = ['-o','delete']
        sm = SM.SubscriptionManager(name='textdb',args=cline)
        sm.execute()

    # Determine if command line has sub operations
    # Returns true if any flags in self.commands[CL.DEFAULT_KEY]
    # are in config.mayJoin
    #
    def __hasSubOperations(self):
        for key in self.commands.keys():
            if key is CL.DEFAULT_KEY:
               flags = self.commands.get(key)
               #specifically looking for config.flags of subJoins
               for flag in flags:
                   configFlag = config.flags.get(flag)
                   # some flags aren't in configs.flags
                   if configFlag and configFlag[0] in config.mayJoin:
                       return True
        return False

    # Correct the sub operational command line .
    #
    #
    def __correctSubOpCommandLine(self):
        for key in self.commands.keys():
            if key is CL.DEFAULT_KEY:
                # determine if the command line has subops
                # for subJoin in self.commands.get(key):
                myTempDict =  dict()
                tempKey = key
                length = len(self.commands.get(key))
                subJoins = self.commands.get(key)                        
                for pos in range(0, length, 2):
                    value = config.flags.get(subJoins[pos])[0]
                    subJoins.remove(subJoins[pos])
                    subJoins.insert(pos, value)
                for idx in range(0, len(subJoins), 2):
                    subKey = subJoins[idx]                    
                    subValue = subJoins[idx+1]
                    myTempDict[subKey] = subValue
                    
                self.commands.pop(tempKey)
                self.commands = myTempDict                    
                    
    # Performs a command line morph to handle the -v flag.
    #
    # args:
    #   cmds: list containing the command line arguments
    #
    # return:
    #    returns the modified command line. if the version flag is
    #    not present, the command line is unchanged
    def __correctVersionRequest(self,cmds):
        if '-v' in cmds:
            if len(cmds) == 3:
                cmds.insert(2,'-r')
            else:
                cmds.insert(2,'-a')
            return cmds
        else:
            return cmds
        
    def __correctLdadRequest(self,cmds):
        if '-pil' in cmds:
            index = cmds.index('-pil')
            cmds.remove('-pil')
            cmds.insert(index,'-l')
            return cmds
        elif '-ldad' in cmds:
            index = cmds.index('-ldad')
            cmds.remove('-ldad')
            cmds.insert(index,'-l')
            return cmds
        else:
            return cmds
    # Main action method for the class. Reads and processes the command line;
    # sends the commands to the text database server; processes the return
    # message; writes results to standard output.
    #
    # return:
    #    0 if processing was successful
    #    1 if either processing was unsuccessful or a 'help' page was requested.
    def execute(self):
        try:
            status = self.__readCommandLine()
            
            if CL.HELP_KEY in self.commands:
                self.__usage()
                return 1
            
            if 'afos_cmds' in self.commands:
                self.__afosCmds()
                return 1
            
            if 'awips_cmds' in self.commands:
                self.__awipsCmds()
                return 1
            
            if self.__isScriptRequest():
                self.__handleScriptRequest()
                return 0
            
            if self.__isTriggerScriptRequest():
                self.__handleWatchWarnRequest()
                return 0
            
            if self.__hasProduct():
                self.__readProduct()

            msg = self.__generateRequestMessage()

            msg = self.__submitRequestMessage(msg)

            if msg == None:
                return 0

            status = self.__processRequestResponse(msg)
            
        except Exception as e:
            util.printMessage(sys.stderr,
                              header='Error processing request',
                              body=str(e))
            return 1
        # return the status
        return status

##############################################################################
# default execution; allows the class to be run as an application
##############################################################################
if __name__ == "__main__":
    tdb = TextDB()
    status = tdb.execute()
    exit(status)
