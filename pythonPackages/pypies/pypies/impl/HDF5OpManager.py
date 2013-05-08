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
# Semi-port from Java HDF5OpManager, handles read requests
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/20/10                      njensen       Initial Creation.
#    02/20/13        DR 15662      M.Porricelli  Modified __do2DPointRequest
#                                                to check for null points
#    
# 
#

import numpy, pypies, logging, time
import h5py.selections
from pypies import StorageException, NotImplementedException
logger = pypies.logger
timeMap = pypies.timeMap

def read(ds, request):
    t0=time.time()
    rt = request.getType()
    if logger.isEnabledFor(logging.DEBUG):
        logger.debug('requestType=' + rt)
    result = None
    indices = request.getIndices()
    if rt == 'ALL':
        if ds.len():
            result = ds.value
        else:
            result = numpy.zeros((0,), ds.dtype.type)
    elif rt == 'POINT':
        points = request.getPoints()
        ndims = len(ds.shape)
        if ndims == 1:
            indices = []
            for pt in points:
                indices.append(pt.getX())
            result = __do1DPointRequest(ds, indices)
        elif ndims == 2:
            result = __do2DPointRequest(ds, points)        
    elif rt == 'XLINE':
        # if a line query was used, but it's only 1d, this is really
        # a point query. We could use hyperslabs to do this, but
        # it would be a lot slower than a regular point query.
        if len(ds.shape) == 1:
            result = __do1DPointRequest(ds, indices)
        else:
            sel = h5py.selections.HyperSelection(ds.shape)
            sel[()] = False
            for n in indices:
                sel[:,n] = True
            result = ds[sel]
            nLines = len(indices)
            if len(result) > nLines:
                result.resize(len(result) / nLines, nLines)
    elif rt == 'YLINE':
        # if a line query was used, but it's only 1d, this is really
        # a point query. We could use hyperslabs to do this, but
        # it would be a lot slower than a regular point query.
        if len(ds.shape) == 1:
            result = __do1DPointRequest(ds, indices)
        else:
            sel = h5py.selections.HyperSelection(ds.shape)
            sel[()] = False
            for n in indices:
                sel[n] = True
            result = ds[sel]        
            nLines = len(indices)
            if len(result) > nLines:
                result.resize(nLines, len(result) / nLines)
    elif rt == 'SLAB':
        minIndex = request.getMinIndexForSlab()
        maxIndex = request.getMaxIndexForSlab()
        
        # Get all sizes and slices in reverse order
        slices = []
        sizes = []
        numIndices = len(minIndex)
         # Get all sizes and slices in reverse order
        for i in reversed(range(numIndices)):
            slices.append(slice(minIndex[i], maxIndex[i]))
            sizes.append(maxIndex[i] - minIndex[i])
        
        sel = h5py.selections.HyperSelection(ds.shape)
        # mask the request slices
        sel[()] = False
        sel[tuple(slices)] = True
        
        # Resize data to desired slab size        
        result = ds[sel]
        result.resize(tuple(sizes))
    else:
        raise NotImplementedException('Only read requests supported are ' +
                                      'ALL, POINT, XLINE, YLINE, and SLAB')
    t1=time.time()

    if timeMap.has_key('read'):
        timeMap['read']+=t1-t0
    else:
        timeMap['read']=t1-t0

    return result


def __do1DPointRequest(ds, indices):
    points = numpy.asarray(indices)
    points.resize(len(indices), 1)
    sel = h5py.selections.PointSelection(ds.shape)
    sel.set(points)
    return ds[sel]

def __do2DPointRequest(ds, points):
    indices = []
    for pt in points:
        if pt is not None:       
            indices.append((pt.getY(), pt.getX()))
        else:
            indices.append((float('nan'),float('nan')))
    arr = numpy.asarray(indices)
    sel = h5py.selections.PointSelection(ds.shape)
    sel.set(arr)
    return ds[sel]
    
    
