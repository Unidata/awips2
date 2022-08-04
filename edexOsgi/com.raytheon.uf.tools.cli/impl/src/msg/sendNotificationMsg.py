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

from __future__ import print_function

import sys
from argparse import ArgumentParser
import os, socket
import traceback

#
# Provides a command-line utility to send messages to alertviz
#  
#    
#     SOFTWARE HISTORY
#    
#    Date           Ticket#      Engineer      Description
#    ------------   ----------   -----------   --------------------------
#    09/30/08                    chammack      Initial Creation.
#    10/22/10       5849         cjeanbap      Updated to use AlertVizMessage.
#    12/08/10       7656         cjeanbap      Retrieve environment variable.
#    05/03/11       5149         cjeanbap      Updated usage statement.
#    07/27/15       4654         skorolev      Added filters and input test.
#    03/05/18       6899         dgilling      Handle exception from NotificationMessage.send.
#    Apr 16, 2020   8144         tgurney       Reference AlertViz stomp port
#                                              specified in NotificationMessage
#    May 14, 2020   8144         tgurney       Change delivery strategy for
#                                              local-delivery messages
#    Jun 19, 2020   8184         tgurney       Fix broken -d option
#    May 25, 2022   DR 23144     aghanava      Modify workstation filter to use the short name.
#

from ufpy import NotificationMessage

class PrintHelpOnErrorParser(ArgumentParser):
    def error(self, message):
        sys.stderr.write('%s: error: %s\n' % (self.prog, message))
        self.print_help()
        sys.stderr.write("""
The host and port default value can be changed by setting 
environment variables DEFAULT_HOST and DEFAULT_PORT. 

Example usage:
    sendNotificationMsg TOPO RADAR 0 "XYZ product has been generated."
    sendNotificationMsg ANNOUNCER LOCAL 0 "XYZ only to machine dev01" --destination dev01.example.com
    sendNotificationMsg --host={edex_localization_host_name} --port=9581 SCAN RADAR 0 "XYZ product has been generated."     
    sendNotificationMsg RADAR DEFAULT 0 "TEST OF filters in sendNotification FOR ALERTVIZ." -f "REGION=CR,DESK=MARINE,SITE=OAX"
"""
    )
        sys.exit(2) 


def main():
    default_host = os.getenv("DEFAULT_HOST", "localhost")
    default_port = os.getenv("DEFAULT_PORT", "9581")
    prog = os.path.splitext(os.path.basename(sys.argv[0]))[0]
    usage = "%(prog)s [--host hostname] [--port portnumber] source category priority message [-d destination] [-f filtersInput]"
    parser = PrintHelpOnErrorParser(usage=usage, conflict_handler="resolve",
                                    prog=prog)
    parser.add_argument("--host", "-h", action="store", dest="host",
                        help="Host name upon which the EDEX server is " +
                        "running. Default: localhost",
                        default=default_host, metavar="hostname")
    parser.add_argument("--port", "-p", action="store", type=int, dest="port",
                           help="Port upon which the EDEX server is running. " +
                           "Default: 9581",
                           default=default_port, metavar="port")
    parser.add_argument("source",  action="store", nargs=1,
                       help="Required Alert Visualization source")
    parser.add_argument("category", nargs=1,
                       help="Required Alert Visualization category")
    parser.add_argument("priority", type=int, nargs=1,
                       help="Required Alert Visualization priority. " +
                       "An integer in the range 0-5")
    parser.add_argument("message", nargs=1,
                       help="Required Alert message to be sent.")
    parser.add_argument("--destination", "-d", nargs='?',
                       help="Optional machine to send message to when " +
                       "category is LOCAL. Default: this machine")
    parser.add_argument("--filtersInput","-f", nargs='?',
                       help="Optional filters to send message to specific " +
                       "localization level. For instance -f 'SITE=OAX'")

    args = parser.parse_args()

    portno = args.port
    message = args.message[0]
    category = args.category[0]
    priority = args.priority[0]
    source = args.source[0]
    destination = args.destination
    hostname = args.host
    filtersInput = args.filtersInput
    
    if destination is not None and category.upper() != 'LOCAL':
        parser.error('cannot specify a destination unless category is LOCAL')
        
    if priority < 0 or priority > 5:
        parser.error('priority must be one of: 0,1,2,3,4,5 (given: %d)' % priority)

    if filtersInput is not None:
        if not filtersInput.strip():
            raise ValueError('Filters input must not be empty or blank')
        else:
            filters = dict(item.strip().split("=") for item in filtersInput.strip().split(","))
    else:
        filters = {}


    # True local delivery is no longer possible with the current (19.3.4)
    # AlertViz design, since the AlertViz port numbers are now chosen
    # dynamically based on uid and there may be any number of AlertViz
    # running on any given workstation. So we have to use global delivery and
    # add a WORKSTATION filter to the message, so that only the chosen
    # destination host accepts the message, and all the other hosts discard it.
    #
    # As of this writing, the name of the WORKSTATION localization level at a
    # given workstation is equal to
    # com.raytheon.uf.common.util.SystemUtil.getHostName()
    # Some testing of different methods suggested that Python's socket.getfqdn()
    # is most likely to match the result of SystemUtil.getHostName().
    if category == 'LOCAL':
        if destination is None:
            fqdn = socket.getfqdn()
        else:
            fqdn = socket.getfqdn(destination)
        try:
            socket.gethostbyname(fqdn)
        except Exception as e:
            print("Failed to resolve the hostname " + fqdn)
            traceback.print_exc()
            sys.exit(1)
        filters['WORKSTATION'] = fqdn.split('.')[0]

    try:
        kwargs = {
           'host': hostname,
           'port': portno,
           'message': message,
           'category': category,
           'priority': priority,
           'source': source,
           'filters': filters
           }
        msg = NotificationMessage.NotificationMessage(**kwargs)
        msg.send()
    except Exception as e:
        print("Failed to send message to {}:{}".format(hostname, portno))
        traceback.print_exc()

if __name__ == "__main__":
    main()
