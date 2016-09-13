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

import time
import sys
from argparse import ArgumentParser
import os, socket

#
# Provides a command-line utility to send messages to alertviz
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/30/08                      chammack       Initial Creation.
#    10/22/10		 5849		   cjeanbap		  Updated to use AlertVizMessage.		
#    12/08/10		 7656		   cjeanbap 	  Retrieve environment variable.
#    05/03/11        5149          cjeanbap       Updated usage statement.
#    07/27/15        4654          skorolev       Added filters and input test.
#

from awips import NotificationMessage

class PrintHelpOnErrorParser(ArgumentParser):
   def error(self, message):
      sys.stderr.write('%s: error: %s\n' % (self.prog, message))
      self.print_help()
      sys.stderr.write( """
The host and port default value can be changed by setting 
environment variables DEFAULT_HOST and DEFAULT_PORT. 

Example usage:
    sendNotificationMsg TOPO RADAR 0 "XYZ product has been generated."
    sendNotificationMsg ANNOUNCER LOCAL 0 "XYZ only to machine dev01" --destination dev01
    sendNotificationMsg --host={edex_localization_host_name} --port=9581 SCAN RADAR 0 "XYZ product has been generated."     
    sendNotificationMsg RADAR DEFAULT 0 "TEST OF filters in sendNotification FOR ALERTVIZ." -f "REGION=CR,DESK=MARINE,SITE=OAX"
"""
    )
      sys.exit(2) 

def main():
   prog = sys.argv[0].split('/')[::-1][0].split('.')[0]
   usage = "%(prog)s [--host hostname] [--port portnumber] source category priority message [-d destination] [-f filtersInput]"
   parser = PrintHelpOnErrorParser(usage=usage, conflict_handler="resolve", prog=prog)
   parser.add_argument("--host", "-h", action="store", dest="host",
                          help="Host name upon which the alertViz server is running. Default: localhost", 
                          metavar="hostname")
   parser.add_argument("--port", "-p", action="store", type=int, dest="port", default = -1,
                          help="Port upon which the alertViz server is running. Default: 9581",
                          metavar="port")
   parser.add_argument("source",  action="store", nargs=1,
                      help="Required Alert Visualization source")
   parser.add_argument("category", nargs=1,
                      help="Required Alert Visualization category")
   parser.add_argument("priority", type=int, nargs=1,
                      help="Required Alert Visualization priority. An integer in the range 0-5")
   parser.add_argument("message", nargs=1,
                      help="Required Alert message to be sent.")
   parser.add_argument("--destination", "-d", nargs='?',
                      help="Optional machine to send message to when category is LOCAL. Default: localhost")
   parser.add_argument("--filtersInput","-f", nargs='?',
                      help="Optional filters to send message to specific localization level. For instance -f 'SITE=OAX'")
   
   args = parser.parse_args()

   portno = args.port
   message = args.message[0]
   category = args.category[0]
   priority = args.priority[0]
   source = args.source[0]
   destination = args.destination
   filtersInput = args.filtersInput

   if portno < 0:
      portno=os.getenv("DEFAULT_PORT", "9581");
   if category == 'LOCAL':
       portno = 61999
       if destination is not None:
           hostname = destination
       else:
           hostname = socket.gethostname()
   elif args.host is not None:
      hostname = args.host
   else:
      hostname=os.getenv("DEFAULT_HOST","localhost");

   if priority < 0 or priority > 5:
      parser.error('bad value for priority=%d' % priority)
      
   if filtersInput is not None:
      if not filtersInput.strip():
         raise ValueError('Filters input must not be empty or blank')
      else:
         filters=dict(item.strip().split("=") for item in filtersInput.strip().split(","))
   else:
       filters={}
       
   msg = NotificationMessage.NotificationMessage(host=hostname, port=portno, message=message,
           category=category, priority=priority, source=source, filters=filters)
   msg.send()

if __name__ == "__main__":
   main()  


