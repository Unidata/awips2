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
# A port of the Java DynamicSerializeManager.  Should be used to read/write
# DynamicSerialize binary data.
#
#
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/09/10                      njensen       Initial Creation.
#    
# 
#

from thrift.transport import TTransport
import SelfDescribingBinaryProtocol, ThriftSerializationContext

class DynamicSerializationManager:
    
    def __init__(self):
        self.transport = None    
    
    def _deserialize(self, ctx):
        return ctx.deserializeMessage()
    
    def deserializeBytes(self, bytes):
        ctx = self._buildSerializationContext(bytes)
        ctx.readMessageStart()
        obj = self._deserialize(ctx)
        ctx.readMessageEnd()
        return obj
    
    def _buildSerializationContext(self, bytes=None):        
        self.transport = TTransport.TMemoryBuffer(bytes)
        protocol = SelfDescribingBinaryProtocol.SelfDescribingBinaryProtocol(self.transport)
        return ThriftSerializationContext.ThriftSerializationContext(self, protocol)
    
    def serializeObject(self, obj):
        ctx = self._buildSerializationContext()
        ctx.writeMessageStart("dynamicSerialize")
        self._serialize(ctx, obj)
        ctx.writeMessageEnd()
        return self.transport.getvalue() 
    
    def _serialize(self, ctx, obj):
        ctx.serializeMessage(obj)