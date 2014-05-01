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


from thrift.protocol.TProtocol import *
from thrift.protocol.TBinaryProtocol import *
from struct import pack, unpack


#
# Partially compatible AWIPS-II Thrift Binary Protocol
#
# <B>Missing functionality:</B>
# <UL>
# <LI> Custom Serializers
# <LI> Float support
# <LI> Inheritance
# </UL>
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/11/09                      chammack        Initial Creation.
#    
# 
#

class SelfDescribingBinaryProtocol(TBinaryProtocol):
  def readFieldBegin(self):
    type = self.readByte()
    if type == TType.STOP:
      return (None, type, 0)
    name = self.readString()
    id = self.readI16()
    return (name, type, id)

  def readStructBegin(self):
     return self.readString()  

  def writeStructBegin(self, name):
     self.writeString(name)

  def writeFieldBegin(self, name, type, id):
     self.writeByte(type)
     self.writeString(name)
     self.writeI16(id)

