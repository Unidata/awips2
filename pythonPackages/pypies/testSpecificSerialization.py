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
# Test for lightning serialization
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/13/10                      njensen       Initial Creation.
#    
# 
#

import dynamicserialize
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.request import *
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage import Request

from pypies.impl import H5pyDataStore

def main():
    request = DatasetDataRequest()
    request.setFilename("/common/njensen/awips/edex/data/hdf5/obs/2010/09/15/11/metar.h5")
    request.setDatasetGroupPath(['presWeather', 'windGust', 'visibility', 'seaLevelPress', 'tempFromTenths', 'pressChange3Hour', 'rawMETAR', 'pressChangeChar', 'temperature', 'dpFromTenths', 'windSpeed', 'skyCover', 'dewpoint', 'longitude', 'latitude', 'windDir'])
    req = Request()
    req.setType('YLINE')
    req.setIndices([4,14])
    request.setRequest(req)
    ds = H5pyDataStore.H5pyDataStore()
    result = ds.retrieveDatasets(request)
    print "result", result
    obj = dynamicserialize.serialize(result)
    f = open('/tmp/pyLight', 'w')
    f.write(obj)
    f.close()
    #print obj
    
if __name__ == '__main__':
    main()
    