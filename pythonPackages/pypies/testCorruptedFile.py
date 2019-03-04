##
##


#
# Test for H5pyDataStore handling corrupted files
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/12/10                      njensen       Initial Creation.
#    
# 
#

import dynamicserialize
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.request import *
import httplib

def main():
    req = RetrieveRequest()
    req.setIncludeInterpolated(False)
    req.setGroup('/bufrua')
    req.setFilename('/home/njensen/temp/corruptHdf5/bufrua-2010-10-07-18.h5')
    
    message = dynamicserialize.serialize(req)
    
    httpConn = httplib.HTTPConnection("localhost", 9582, timeout=90)
    httpConn.connect()
    httpConn.request("POST", '/', message)
    response = httpConn.getresponse()
    if (response.status != 200):
        print response, response.status, response.msg
        print dir(response)
        raise RuntimeError("Unable to post request to server")
        
    rval = dynamicserialize.deserialize(response.read())
    httpConn.close()
    print rval.getError()
    
if __name__ == '__main__':
    main()
    
    
