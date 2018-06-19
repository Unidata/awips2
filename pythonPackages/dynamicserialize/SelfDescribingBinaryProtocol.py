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
# <LI> Inheritance
# </UL>
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    11/11/09                      chammack        Initial Creation.
#    06/09/10                      njensen            Added float, list methods
#    Apr 24, 2015    4425          nabowle         Add F64List support. 
#    
# 
#

import struct, numpy

FLOAT = 64

intList = numpy.dtype(numpy.int32).newbyteorder('>')
floatList = numpy.dtype(numpy.float32).newbyteorder('>')
longList = numpy.dtype(numpy.int64).newbyteorder('>')  
shortList = numpy.dtype(numpy.int16).newbyteorder('>')
byteList = numpy.dtype(numpy.int8).newbyteorder('>')
doubleList = numpy.dtype(numpy.float64).newbyteorder('>')

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

  def readFloat(self):
      d = self.readI32()
      dAsBytes = struct.pack('i', d)
      f = struct.unpack('f', dAsBytes)
      return f[0]

  def writeFloat(self, f):
      dAsBytes = struct.pack('f', f)
      i = struct.unpack('i', dAsBytes)
      self.writeI32(i[0])

  def readI32List(self, sz):
      buff = self.trans.readAll(4*sz)
      val = numpy.frombuffer(buff, dtype=intList, count=sz)
      return val

  def readF32List(self, sz):
      buff = self.trans.readAll(4*sz)
      val = numpy.frombuffer(buff, dtype=floatList, count=sz)
      return val

  def readF64List(self, sz):
      buff = self.trans.readAll(8*sz)
      val = numpy.frombuffer(buff, dtype=doubleList, count=sz)
      return val

  def readI64List(self, sz):
      buff = self.trans.readAll(8*sz)
      val = numpy.frombuffer(buff, dtype=longList, count=sz)
      return val

  def readI16List(self, sz):
      buff = self.trans.readAll(2*sz)
      val = numpy.frombuffer(buff, dtype=shortList, count=sz)
      return val

  def readI8List(self, sz):
      buff = self.trans.readAll(sz)
      val = numpy.frombuffer(buff, dtype=byteList, count=sz)
      return val

  def writeI32List(self, buff):
      b = numpy.asarray(buff, intList)
      self.trans.write(numpy.getbuffer(b))

  def writeF32List(self, buff):
      b = numpy.asarray(buff, floatList)
      self.trans.write(numpy.getbuffer(b))

  def writeF64List(self, buff):
      b = numpy.asarray(buff, doubleList)
      self.trans.write(numpy.getbuffer(b))

  def writeI64List(self, buff):
      b = numpy.asarray(buff, longList)
      self.trans.write(numpy.getbuffer(b))

  def writeI16List(self, buff):
      b = numpy.asarray(buff, shortList)
      self.trans.write(numpy.getbuffer(b))

  def writeI8List(self, buff):
      b = numpy.asarray(buff, byteList)
      self.trans.write(numpy.getbuffer(b))
