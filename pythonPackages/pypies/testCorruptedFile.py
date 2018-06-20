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
    
    
