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

from optparse import OptionParser
from ufpy import NotificationMessage
from argparse import ArgumentParser
import os, sys

#
# Provides a command-line utility to send messages GFE-specific messages to
# alertviz
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/14/10                      dgilling       Initial Creation.
#    11/04/10        5849          cjeanbap		  Updated Notification package location
#    06/09/11        9841          rferrel        fixed help and argument order
#                                                 for sendNotificationMsg.   
#    11/29/12        DR14016       jzeng          change priority definition from 0-3 to 1-4
# 
#
class PrintHelpOnErrorParser(ArgumentParser):
    def error(self, message):
        sys.stderr.write('%s: error: %s\n' % (self.prog, message))
        self.print_help()
        sys.exit(2) 

def main():
        prog = sys.argv[0].split('/')[::-1][0].split('.')[0]
	usage = "%(prog)s -h hostname -p port [-r] [-s] [-u] [-a] [-c class] -m message"
	parser = PrintHelpOnErrorParser(usage=usage, conflict_handler="resolve", prog=prog)
	parser.add_argument("-h", action="store", dest="host",
	                  help="Host name upon which the alertViz server is running.", 
	                  metavar="hostname")
	parser.add_argument("-p", action="store", type=int, dest="port", default=9581,
	                  help="Port upon which the alertViz server is running.",
	                  metavar="port")
	group = parser.add_mutually_exclusive_group()
	group.add_argument("-r", action="store_const", const=4, dest="priority",
	                  help="Send as a routine message. (default)")
	group.add_argument("-s", action="store_const", const=2, dest="priority", 
	                  help="Send as a significant message.")
	group.add_argument("-u", action="store_const", const=1, dest="priority", 
	                  help="Send as an urgent message.")
	group.add_argument("-a", action="store_const", const=3, dest="priority", 
	                  help="Send as an alert message.")
	parser.add_argument("-c", action="store", dest="msgClass", 
	                  help="Message class.", metavar="class", default="GFE")
	parser.add_argument("-m", action="store", dest="message", required=True, 
	                  help="Message contents.", metavar="message")
	parser.set_defaults(priority=3)
	options = parser.parse_args()
		
	# Currently sendNotificationMsg requires the optional (flag) arguments
	# be placed prior to the positional arguments.
	commandLine = "/awips2/fxa/bin/sendNotificationMsg"
	if options.host is not None:
		commandLine += " --host=" + options.host
	if options.port is not None:
		commandLine += " --port=" + str(options.port)
	commandLine += " GFE " + options.msgClass + " "  + \
	        str(options.priority) + " \"" + options.message + "\""
	
	os.system(commandLine)

if __name__ == "__main__":
   main()  
