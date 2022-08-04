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


import socket
import sys
import os

#
# Provides a command-line compatible version of fxaAnnounce
#  
#    
#     SOFTWARE HISTORY
#    
#    Date           Ticket#      Engineer      Description
#    ------------   ----------   -----------   --------------------------
#    09/30/08                    chammack      Initial Creation.
#    10/19/10       5849         cjeanbap      Change method being invoked.
#    11/03/10       5849         cjeanbap      Set default port
#    12/06/10       7655         cjeanbap      Use environment variables.
#    04/14/11       4514         cjeanbap      Change SYSTEM to Systat
#    05/03/11       5149         cjeanbap      Updated usage statement.
#    11/11/2011     11484        rferrel       Added SYSTEM for WSHNIMNAT
#    07/27/15       4654         skorolev      Added level filters. Added input test.
#    01/27/2017     DR19287      lshi          Restored support for non-dictionary
#                                              data type of the 4th argument
#    05/25/2017     DR20016      D. Friedman   Fix 4th/5th argument handling.
#    Apr 16, 2020   8144         tgurney       Reference AlertViz stomp port
#                                              specified in NotificatioMessage
#    May 14, 2020   8144         tgurney       Change delivery strategy for
#                                              local-delivery messages
#

from ufpy import NotificationMessage 

def usage():
  print('')
  print('Usage: fxaAnnounce announcement displayerType importance [audioFile] [filters]')
  print('')
  print('Ensure the EDEX Localization Hostname and Port in the /{install_dir}/fxa/bin/setup.env file')
  print('is initially setup (once) before executing otherwise an Error will occur.')
  print('')
  print('   displayerType: RADAR, SYSTAT, LOCAL, SYSTEM')
  print('   importance: URGENT (1), SIGNIFICANT (2), ROUTINE (4)')
  print('   audioFile: Path to audio file')
  print('   filters: Optional filters to send message to specific localization level. ')
  print('            For instance "SITE=OAX,DESK=MARINE"')
  print('')
  sys.exit(1)

if len(sys.argv) < 4 or len(sys.argv) > 6:
   usage() 

announcement = sys.argv[1]
displayerType = sys.argv[2]
importance = sys.argv[3]
audioFile = None
fltrs = None

for arg in sys.argv[4:6]:
    if fltrs is None and audioFile is None and '=' not in arg:
        audioFile = arg
    elif '=' in arg:
        if fltrs is not None:
            usage()
        arg = arg.strip()
        if arg:
            fltrs=dict(item.strip().split("=", 1) for item in arg.split(","))
        else:
            raise ValueError('Filters input must not be empty or blank')
    else:
        usage()

if importance.upper() == "URGENT":
   pri = 1
elif importance.upper() == "SIGNIFICANT":
   pri = 2
elif importance.upper() == "ROUTINE":
   pri = 4
else:
   print('fxaAnnounce: Invalid importance argument')
   usage()
   exit(2) 

hostname=os.getenv("DEFAULT_HOST","localhost")
portnum=os.getenv("DEFAULT_PORT",9581)
source="ANNOUNCER"

if displayerType.upper() == "RADAR":
   cat = "RADAR"
elif displayerType.upper() == "SYSTAT" or displayerType.upper() == "SYSTEM":
   cat = "SYSTAT"
elif displayerType.upper() == "LOCAL":
   cat = "LOCAL"
   if fltrs is None:
       fltrs = {}
   fltrs['WORKSTATION'] = socket.getfqdn()
else:
   print('fxaAnnounce: Invalid displayer type argument')
   usage()
   exit(2)
msg = NotificationMessage.NotificationMessage(host=hostname, port=portnum,
        message=announcement, source=source, category=cat, priority=pri,
        audioFile=audioFile, filters=fltrs)
msg.send()
