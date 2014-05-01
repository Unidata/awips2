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
# Test for h5pyDatastore
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/16/10                      njensen       Initial Creation.
#    
# 
#

import numpy, os

from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.request import *
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.response import *
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage.records import *
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage import *
from pypies.impl import H5pyDataStore

FILENAME = '/tmp/pythonTest.h5'
datastore = H5pyDataStore.H5pyDataStore()

def testStore():
    req = StoreRequest()
    req.setOp('STORE_ONLY')
    req.setFilename(FILENAME)
    
    rec = FloatDataRecord()
    rec.setName('Data')
    rec.setGroup('/testGroup')
    rec.setSizes([10, 5])
    data = numpy.zeros((50,), numpy.float32)
    data.fill(3.0)
    rec.setFloatData(data)
    rec.setDimension(2)
    req.setRecords([rec])
    
    resp = datastore.store(req)
    if resp.getFailedRecords():      
        return False
    else:
        return True


def testPartialWrite():
    req = StoreRequest()
    req.setOp('REPLACE')
    req.setFilename(FILENAME)
    
    rec = FloatDataRecord()
    rec.setName('Data')
    rec.setGroup('/testGroup')
    rec.setSizes([2, 4])
    data = numpy.zeros((8,), numpy.float32)
    data.fill(5.0)
    rec.setFloatData(data)
    rec.setDimension(2)
    rec.setMinIndex([2,0])
    req.setRecords([rec])
    
    resp = datastore.store(req)    
    if resp.getFailedRecords():
        return False
    else:
        return True

def testGroupRetrieve():
    req = GroupsRequest()
    req.setFilename(FILENAME)
    req.setGroups(['/testGroup'])    
    
    sreq = Request()
    sreq.setType('ALL')
    req.setRequest(sreq)
    
    resp = datastore.retrieveGroups(req)
    return True
    
    
def testDelete():
    req = DeleteRequest()
    req.setFilename(FILENAME)
    req.setLocations(['/testGroup/Data'])
    
    resp = datastore.delete(req)
    return True

def testCreateDataset():
    req = CreateDatasetRequest()
    req.setFilename(FILENAME)
    rec = FloatDataRecord()
    rec.setFillValue(-8888.8)
    rec.setDimension(2)
    rec.setGroup("/StaticTopo")
    rec.setName("Data")
    rec.setSizes([4652, 5648])
    req.setRecord(rec)
    datastore.createDataset(req)
    
    return True


def main():
    if os.path.exists(FILENAME):
        os.remove(FILENAME)
        
    passed = 0
    failed = 0
    methods = [testStore, testPartialWrite, testGroupRetrieve, testDelete, testCreateDataset]
    for m in methods:        
        x = m()
        if x:            
            passed += 1
        else:
            failed += 1
            print m.__name__, "failed"
    
    print passed, "tests passed"
    print failed, "tests failed"
    
if __name__ == '__main__':
    main()
    
    
