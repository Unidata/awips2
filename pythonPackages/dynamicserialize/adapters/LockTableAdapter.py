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
# Adapter for com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/22/13                      rjpeter       Initial Creation.
#    
# 
#

from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.lock import LockTable
from dynamicserialize.dstypes.com.raytheon.uf.common.dataplugin.gfe.server.lock import Lock

ClassAdapter = 'com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable'

def serialize(context, lockTable):
    index=0
    wsIds = {lockTable.getWsId().toString() : index}
    index += 1
    locks = lockTable.getLocks()
    lockWsIdIndex = []
    for lock in locks:
        wsIdString = lock.getWsId().toString()
        
        if wsIds.has_key(wsIdString):
            lockWsIdIndex.append(wsIds[wsIdString])
        else:
            lockWsIdIndex.append(index)
            wsIds[wsIdString] = index
            index += 1

    context.writeObject(lockTable.getParmId())
    
    context.writeI32(index)
    for wsId in sorted(wsIds, key=wsIds.get):
        context.writeObject(wsId)
        
    context.writeI32(len(locks))
    for lock, wsIndex in zip(locks, lockWsIdIndex):
        serializer.writeI64(lock.getStartTime())
        serializer.writeI64(lock.getEndTime())
        serializer.writeI32(wsIndex)
    
def deserialize(context):
    parmId = context.readObject()
    numWsIds = context.readI32()
    wsIds = []
    for x in xrange(numWsIds):
        wsIds.append(context.readObject())
        
    numLocks = context.readI32()
    locks = []
    for x in xrange(numLocks):
        lock = Lock()
        lock.setParmId(parmId)
        lock.setStartTime(context.readI64())
        lock.setEndTime(context.readI64())
        lock.setWsId(wsIds[context.readI32()])
        locks.append(lock)
        
    lockTable = LockTable()
    lockTable.setParmId(parmId)
    lockTable.setWsId(wsIds[0])
    lockTable.setLocks(locks)

    return lockTable