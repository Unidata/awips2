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
#
# Provides a command-line utility to deactivate a site
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/10/14         #3623        randerso        Initial Creation.
#
## 

import os
import sys
import time

from dynamicserialize.dstypes.com.raytheon.uf.common.site.requests import DeactivateSiteRequest

from awips import ThriftClient
from awips import UsageArgumentParser

from ActivationTopicListener import ActivationTopicListener

def main():
    args = validateArgs()
    
    request = DeactivateSiteRequest(args.site, args.plugin)
    thriftClient = ThriftClient.ThriftClient(args.host, args.port, "/services")
    
    thread = ActivationTopicListener(args.jmsHost, args.jmsPort)
    
    try:
        thread.start()
        time.sleep(1) # sleep to allow thread to connect to JMS broker
        
        print "\nSending site deactivation request for "+args.site
        thriftClient.sendRequest(request)
        
        print "\nMonitoring site activation messages."
        thread.join()
            
    except KeyboardInterrupt:
        pass
    except Exception, ex:
        import traceback
        traceback.print_exc()
    finally:
        thread.stop()
    
def validateArgs():
    parser = UsageArgumentParser.UsageArgumentParser(conflict_handler="resolve", prog='deactivateSite')
    parser.add_argument("-h", action="store", dest="host",
                      help="host name of edex request server",
                      default=str(os.getenv("DEFAULT_HOST", "localhost")),
                      metavar="hostname")
    parser.add_argument("-r", action="store", type=int, dest="port", 
                      help="port number of edex request server",
                      default=int(os.getenv("DEFAULT_PORT", "9581")),
                      metavar="port")
    parser.add_argument("-j", action="store", dest="jmsHost",
                      help="host name of JMS broker", 
                      default=str(os.getenv("JMS_HOST", "localhost")),
                      metavar="jmsHost")
    parser.add_argument("-q", action="store", type=int, dest="jmsPort", 
                      help="port number of JMS broker",
                      default=int(os.getenv("JMS_PORT", "5672")),
                      metavar="jmsPort")
    parser.add_argument("-p", action="store", dest="plugin", required=False,
                      help="plugin",
                      default="gfe",
                      metavar="plugin")
    parser.add_argument("-s", action="store", dest="site", required=True,
                      help="site to deactivate",
                      metavar="site")
    args = parser.parse_args()
    
    return args
    

if __name__ == '__main__':
    main()