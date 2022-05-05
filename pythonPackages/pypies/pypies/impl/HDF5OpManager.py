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
#    09/19/2018      7435          ksunil        Eliminate compression/decompression on HDF5
#    04/30/2019      7814          dgilling      Make compatible with h5py 2.x.
#    Jun 25, 2019    7885          tgurney       Python 3 fixes
#    09/17/2019      7930          randerso      Fix slab request performance
#    Oct 30, 2019    7962          tgurney       Fix 2D point requests
#    Nov  5, 2019    7885          tgurney       Fix 1D point requests
#    01/28/2020      7985          ksunil        Removed the compression changes introduced in 7435
#    Jul 20, 2021    8594          dgilling      Prevent 1D point requests from modifying state of 
#                                                input request.
#

import numpy, pypies, logging, time
from pypies import NotImplementedException
import h5py

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
            result = ds[()]
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
            result = ds[:, sorted(indices)]
    
    elif rt == 'YLINE':
        # if a line query was used, but it's only 1d, this is really
        # a point query. We could use hyperslabs to do this, but
        # it would be a lot slower than a regular point query.
        if len(ds.shape) == 1:
            result = __do1DPointRequest(ds, indices)
        else:
            result = ds[sorted(indices)]

    elif rt == 'SLAB':
        minIndex = request.getMinIndexForSlab()
        maxIndex = request.getMaxIndexForSlab()

        # Get all sizes and slices in reverse order
        slices = []
        numIndices = len(minIndex)
         # Get all sizes and slices in reverse order
        for i in reversed(range(numIndices)):
            slices.append(slice(minIndex[i], maxIndex[i]))

        # Resize data to desired slab size
        result = ds[tuple(slices)]
    else:
        raise NotImplementedException('Only read requests supported are ' +
                                      'ALL, POINT, XLINE, YLINE, and SLAB')
    t1=time.time()

    if 'read' in timeMap:
        timeMap['read']+=t1-t0
    else:
        timeMap['read']=t1-t0
    return result


def __do1DPointRequest(ds, indices):
    points = numpy.array(indices, copy=True)
    points.resize(len(indices), 1)
    sel = h5py._hl.selections.PointSelection(ds.shape)
    sel.set(points)
    return ds[sel]


def __do2DPointRequest(ds, points):
    arr = numpy.asarray(tuple((pt.getY(), pt.getX()) for pt in points if pt))
    sel = h5py._hl.selections.PointSelection(ds.shape)
    sel.set(arr)
    return ds[sel]


