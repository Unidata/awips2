#!/home/jkorman/awips/bin/python
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

import httplib
import sys
import os

xml = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><message><header><properties value=\"read:OMAMTROMA\" name=\"command\"/></header></message>"

conn = httplib.HTTP("localhost:9581")
conn.putrequest("POST", "/services/textdbsrv")
conn.putheader("Content-Type", "text/plain")
conn.putheader("Content-Length", "%d" % len(xml))
conn.endheaders()
conn.send(xml)

# Success/Failure messages 
reply, messages, headers = conn.getreply()
result = conn.getfile().read()
conn.close()
# expect message 200, if not exit
if reply != 200:
   print reply + " " + messages
   exit (1)

print  result

