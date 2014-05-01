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

##############################################################################
# define the default key. This will be set as the key if the command line
# doesn't start with a flag. By defining this as the module level, it is
# available for use by clients of this module as well.
##############################################################################
DEFAULT_KEY = 'DEFAULT'

##############################################################################
# define the default key. This will be set as the key if the command line
# either contains the'-h' command or is empty.By defining this as the module
# level, it is available for use by clients of this module as well.
##############################################################################
HELP_KEY = 'help'

##############################################################################
# define the command line help/usage flag. This flag is automatically available
# for users of this package. By defining this as the module level, it is
# available for use by clients of this module as well.
##############################################################################
HELP_FLAG = '-h'

##############################################################################
# A class that handles parsing of a commend line.
#
# The expected command line is
#     [flags] operands [flags [operands]
#
# The flags can be single or multiple flags, but define a single operation.
# That operation is applied to the entire operand list. As an option, you 
# can turn off the combining of multiple flags into a single flag.
#
# The client is responsible for capturing the command line prior to utilizing
# this class. Although the command line is normally obtained using "sys.argv[1:]",
# having the client obtain the command line allows this class to be used when 
# another mechanism is used to provide the "command line".
#
# Usage:
#    import CommandLine as CL
#    cl = CL.CommandLine(config.flags)
#    cl.setArgs(sys.argv[1:])
#    cl.parse()
#    commands = cl.getCommands() 
# 
# Note: The program using this module must provide a dictionary containing
#       the valid command line flags. The structure of this dictionary is
#
#         flags = {'flag1',('command1','description of command1'),
#                  'flag2',('command2','description of command2'),
#                  ...
#                  'flagn',('commandn','description of commandn'),}
#
#       Note also that all command flag sets have an implied -h help command.
# 
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/28/08        1584          mfegan         Initial Creation.
##############################################################################
class CommandLine:
    
    # Initializer.
    #
    # args:
    #   prog:    name of the program using this class
    #   flags:   dictionary containing the recognized flags and operations
    #   combine: flag; set to combine consecutive flags into a single operator 
    def __init__(self,prog,flags=None,combine=False):
        self.args = []
        self.commands = {}
        self.prog = prog
        self.flags = flags
        self.__addHelpFlag()
        self.vals = []
        self.combine = combine

    # Formats a string representation of the class.
    def __str__(self):
        retval = 'CommandLine['
        retval += str(self.commands)
        retval +=']'
        return retval

    # sets the command line to parse.
    #
    # args:
    #   args: list containing the command line arguments.
    def setArgs(self,args):
        self.args = args

    # Parses the previously set argument list. 
    # data structure:
    # commands = { 'cmd1':['arg1','arg2',...,'argn'],
    #              'cmd2':['arg1','arg2',...,'argn'],
    #              ...
    #              'cndn':['arg1','arg2',...,'argn']}
    def parse(self):
        
        # combine any multiple flags
        if self.combine:
            self.__merge()

        # short circuit - if no command line arguments, set the help flag
        if len(self.args) == 0:
            command = self.flags[HELP_FLAG][0]
            self.commands[command] = []
            return
            
        command = DEFAULT_KEY
        arguments = []
        
        # parse the command line arguments
        for arg in self.args:
            if self.__isFlag(arg):
                if self.__isValidFlag(arg) :
                    # means we have hit a new command
                    flag = self.flags[arg][0]
                    if DEFAULT_KEY != command:
                        if command in self.commands:
                            self.commands[command].extend(arguments)
                        else:
                            self.commands[command] = arguments
                    # save off the default command if it has arguments
                    else:
                        if len(arguments) != 0:
                            self.commands[command] = arguments
                    command = self.flags[arg][0]
                    arguments = []
            # it's an argument, add it to the list
            else:
                arguments.append(arg)
        else:
            if command in self.commands:
                self.commands[command].extend(arguments)
            else:
                self.commands[command] = arguments
    
    # Iterates the commands obtained from the command line and
    # replaces any single element lists with simple strings.
    def mergeListsToString(self):
        for key in self.commands.keys():
            val = self.commands.get(key)
            if len(val) == 1:
                self.commands[key] = val[0]

    # returns the dictionary containing the parsed commands.
    def getCommands(self):
        return self.commands

    # returns a dictionary containing the single
    # entry for the key 
    def getCommand(self,key):
        return {key:self.commands.get(key)}
    
    # Merges any consecutive flags into a single flag. The help flag
    # is not merged with other flags. This method updates self.args.
    def __merge(self):
        # short circuit - take care of the case of a single flag
        if len(self.args) == 1 and self.__isFlag(self.args[0]):
            return
        
        # process the command line
        prev = ""
        temp = []
        for arg in self.args:
            if self.__isFlag(arg):
                if arg == HELP_FLAG:
                    temp.append(arg)
                elif "" == prev:
                    prev = arg
                else:
                    prev += arg[1:] 
            else:
                if "" != prev:
                    temp.append(prev)
                    prev = ""
                temp.append(arg)
        # save the modified arguments
        self.args = temp

    # Adds the standard help flag (-h) if the flags dictionary
    # provided by the client doesn't include a help flag.
    def __addHelpFlag(self):
        help = {HELP_FLAG:(HELP_KEY,'print usage message')}
        if self.flags is None:
            self.flags = help
        else:
            self.flags.update(help)
    
    def isHelp(self):
        return HELP_KEY in self.commands
    
    # determines if the argument is a flag
    def __isFlag(self,arg):
        return len(arg) == 2 and arg.find('-') == 0
    
    # Determines if the argument is one of the valid command line flags.
    def __isValidFlag(self,arg):
        return arg in self.flags
    
    # prints a standardized usage message.
    def usage(self):
        print 'Usage:'
        print '   ' + self.prog + ' [flag(s)] arguments' 
        print
        print ' flags are'
        for key in self.flags.keys():
            tmp = self.flags.get(key)
            print '   ' + key,
            if tmp is not None:
                print "(" + tmp[0] + ")",
                print tmp[1],
                if len(tmp) == 3:
                    print tmp[2]
                else:
                    print
        print

##############################################################################
# defines the Exception raised by methods in this package
##############################################################################
class ArgError(Exception):
    def __init__(self,value,cause=None):
      self.value = value
      self.cause = cause
    def __str__(self):
        msg = 'ArgError: ' + repr(self.value)
        if self.cause is not None:
            msg += "\n caused by " + repr(self.cause)
        return msg
    def __repr__(self):
        return self.__str__()