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
# This code provided purely as an example.
# NOT OFFICIALLY SUPPORTED
# 

import NotificationMessage.ttypes
import thrift.transport.TTransport
import SelfDescribingBinaryProtocol

f = open("/tmp/testSerialization")
trans = thrift.transport.TTransport.TFileObjectTransport(f)
proto = SelfDescribingBinaryProtocol.SelfDescribingBinaryProtocol(trans)
print proto.readMessageBegin()
s = NotificationMessage.ttypes.com_raytheon_edex_msg_DataURINotificationMessage()
s.read(proto)
print s

