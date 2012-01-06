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
import PointData.ttypes
import thrift.transport.TTransport
import thrift.transport.THttpClient
import SelfDescribingBinaryProtocol
import time
from StringIO import StringIO


t1 = time.time()
memTrans = thrift.transport.THttpClient.THttpClient('http://localhost:9581/services/thrift')
protoWrite = SelfDescribingBinaryProtocol.SelfDescribingBinaryProtocol(memTrans)
protoWrite.writeMessageBegin('dynamicSerialize', 0, 0)

c1 = PointData.ttypes.com_raytheon_uf_common_pointdata_PointDataRequestMessageConstraint(constraintType=0, parameter='location.stationId', value='KOMA')
msg = PointData.ttypes.com_raytheon_uf_common_pointdata_PointDataRequestMessage(allLevels=True, constraints=[], parameters=['temperature', 'rawMETAR'], pluginName='obs')

msg.write(protoWrite)

protoWrite.writeMessageEnd()
memTrans.flush()
msg = memTrans.message
print msg


proto = SelfDescribingBinaryProtocol.SelfDescribingBinaryProtocol(memTrans)
print proto.readMessageBegin()
s = PointData.ttypes.com_raytheon_uf_common_pointdata_PointDataThriftContainer()
t3 = time.time()
s.read(proto)
t2 = time.time()
print 'Took %0.3f ms ' % ((t2-t1) * 1000.0)
print 'Deserialize Took %0.3f ms ' % ((t2-t3) * 1000.0)
print s

 
# doRead() 
