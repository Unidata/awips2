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
# Main processing module of pypies.  Receives the http request through WSGI,
# deserializes the request, processes it, and serializes the response
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/17/10                      njensen        Initial Creation.
#    01/11/13                      bkowal         Pypies will now read the hdf5 root from configuration
#    01/17/13        1490          bkowal         Relocated the configuration of pypies
#    06/12/13        2102          njensen        Raise uncaught exceptions to force http code 500
#    11/06/14        3549          njensen        Log time to receive data
#    07/30/15        1574          nabowle        Handle DeleteOrphansRequest
#    11/15/16        5992          bsteffen       Log size
#    Jun 25, 2019    7885          tgurney        Python 3 fixes
#    Sep 28, 2021    8608          mapeters       Add special handling for certain error types
#
#

from werkzeug import Request, Response
import errno
import time, logging
import pypies
from pypies import IDataStore
import dynamicserialize
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.request import *
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.response import *

logger = pypies.logger
timeMap = pypies.timeMap
hdf5Dir = pypies.hdf5Dir

from pypies.impl import H5pyDataStore
datastore = H5pyDataStore.H5pyDataStore()

datastoreMap = {
    StoreRequest: (datastore.store, "StoreRequest"),
    RetrieveRequest: (datastore.retrieve, "RetrieveRequest"),
    DatasetNamesRequest: (datastore.getDatasets, "DatasetNamesRequest"),
    DatasetDataRequest: (datastore.retrieveDatasets, "DatasetDataRequest"),
    GroupsRequest: (datastore.retrieveGroups, "GroupsRequest"),
    DeleteRequest: (datastore.delete, "DeleteRequest"),
    DeleteFilesRequest: (datastore.deleteFiles, "DeleteFilesRequest"),
    CreateDatasetRequest: (datastore.createDataset, "CreateDatasetRequest"),
    RepackRequest: (datastore.repack, "RepackRequest"),
    CopyRequest: (datastore.copy, "CopyRequest"),
    DeleteOrphansRequest: (datastore.deleteOrphanFiles, "DeleteOrphansRequest")
}

@Request.application
def pypies_response(request):
    timeMap.clear()
    try:
        startTime = time.time()
        try:
            data=request.data
            timeMap['receiveData']=time.time()-startTime
            timeMap['size']=len(data)
            startTime = time.time()
            obj = dynamicserialize.deserialize(data)
        except:
            msg = 'Error deserializing request: ' + IDataStore._exc()
            logger.error(msg)
            resp = ErrorResponse()
            resp.setError(msg)
            return __prepareResponse(resp)
        timeMap['deserialize']=time.time()-startTime
        # add the hdf5 directory path to the file name
        filename = hdf5Dir + obj.getFilename()
        obj.setFilename(filename)

        clz = obj.__class__
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug(str(clz) + ": " + obj.getFilename())
        success = False
        if clz in datastoreMap:
            try:
                resp = datastoreMap[clz][0](obj)
                success = True
            except Exception as e:
                msg = 'Error processing ' + datastoreMap[clz][1] +' on file ' + obj.getFilename() + ': ' + IDataStore._exc()
                logger.error(msg)
                resp = ErrorResponse(msg, __getType(e))
        else:
            msg = 'IDataStore unable to process request of type ' + str(obj.__class__)
            logger.error(msg)
            resp = ErrorResponse()
            resp.setError(msg)

        startSerialize = time.time()
        httpResp = __prepareResponse(resp)
        if success:
            endTime = time.time()
            timeMap['serialize'] = endTime - startSerialize
            timeMap['total'] = endTime - startTime
            logger.info({'request':datastoreMap[clz][1], 'time':timeMap, 'file':obj.getFilename()})
        return httpResp
    except:
        # Absolutely should not reach this, if we do, need to fix code
        logger.error("Uncaught exception! " + IDataStore._exc())
        # want to re-raise the error as that will cause PyPIES to return http error code 500
        raise


def __prepareResponse(resp):
    try:
        serializedResp = dynamicserialize.serialize(resp)
    except:
        resp = ErrorResponse()
        errorMsg = 'Error serializing response: ' + IDataStore._exc()
        logger.error(errorMsg)
        resp.setError(errorMsg)
        # hopefully the error response serializes ok, if not you're kind of screwed
        serializedResp = dynamicserialize.serialize(resp)
    return Response(serializedResp)

def __getType(exception):
    if isinstance(exception, PermissionError):
        # subclass of OSError for errno EPERM and EACCES
        return 'PERMISSIONS'
    elif isinstance(exception, OSError):
        if exception.errno == errno.ENOSPC:
            return 'DISK_SPACE'
    return 'OTHER'
