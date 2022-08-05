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
# Semi-port from Java, builds the result storage records from a read
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/21/10                      njensen       Initial Creation.
#    09/19/13        2309          bsteffen      Fix group name in returned
#                                                records.
#    Nov 14, 2013    2393          bclement      removed interpolation
#    Apr 24, 2015    4425          nabowle       Add DoubleDataRecord
#    Jul 27, 2015    4402          njensen       return fill value of None if fill_time_never
#    Feb 16, 2016    3857          tgurney       Handle lowercase compression type (e.g. "lzf")
#    Feb 16, 2016    3857          tgurney       Add min index to slab retrieval response
#    Sep 19, 2018    7435          ksunil        Eliminate compression/decompression on HDF5
#    May 22, 2019    7847          bsteffen      Always return NONE for compression type of requested data.
#    Jun 25, 2019    7885          tgurney       Python 3 fixes
#    Jan 28, 2020    7985          ksunil        Removed the compression changes introduced in 7435
#    Jun 08, 2022    8866          mapeters      Set max sizes on record in createStorageRecord()
#

import numpy
import pypies
import logging
import time
from h5py import h5d
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage import *
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage.records import *
logger = pypies.logger
timeMap = pypies.timeMap

typeToClassMap = {
                  numpy.int8: ByteDataRecord,
                  numpy.int16: ShortDataRecord,
                  numpy.int32: IntegerDataRecord,
                  numpy.int64: LongDataRecord,
                  numpy.float32: FloatDataRecord,
                  numpy.float64: DoubleDataRecord,
                  numpy.object_: StringDataRecord,
                  numpy.string_: StringDataRecord
}


def createStorageRecord(rawData, ds, req):
    """
    Create and return new storage record.

    Args:
            rawData: numpy.ndarray object
            ds: h5py Dataset object
            req: Request object

    Returns:
            *DataRecord object, depends on type of data requested.
    """
    t0 = time.time()

    t = typeToClassMap[rawData.dtype.type]
    inst = t()
    name = ds.name
    parentName = '/'
    slashIndex = name.rfind('/')
    if slashIndex > -1:
        parentName = name[0:slashIndex].replace('::', '/')
        name = name[slashIndex+1:]
    inst.setName(name)
    inst.setGroup(parentName)
    inst.putDataObject(rawData)
    inst.setDimension(len(ds.shape))

    if logger.isEnabledFor(logging.DEBUG):
        logger.debug("rawData.shape " + str(rawData.shape))
    sizes = [int(x) for x in reversed(rawData.shape)]
    inst.setSizes(sizes)

    if ds.maxshape:
        maxSizes = [int(x) if x else 0 for x in reversed(ds.maxshape)]
        inst.setMaxSizes(maxSizes)

    fillValue = None
    if (ds._dcpl.get_fill_time() != h5d.FILL_TIME_NEVER and
          rawData.dtype.type != numpy.object_ and
          rawData.dtype.type != numpy.string_):
        fillValue = numpy.zeros((1,), rawData.dtype)
        ds._dcpl.get_fill_value(fillValue)
        inst.setFillValue(fillValue[0])

    attrs = {}
    for key in ds.attrs.keys():
        try:
            val = ds.attrs[key]
            if type(val) is numpy.ndarray:
                val = val[0]
            attrs[key] = val
        except TypeError:
            # Skip over attributes that h5py cannot map into numpy datatypes
            pass
    inst.setDataAttributes(attrs)
    props = StorageProperties()
    if ds.chunks:
        props.setChunked(True)
    else:
        props.setChunked(False)
    props.setCompression('NONE')
    inst.setProps(props)

    minIndex = req.getMinIndexForSlab()
    if minIndex is not None:
        inst.setMinIndex(numpy.array(list(minIndex), numpy.int64))

    t1 = time.time()
    if 'createRecord' in timeMap:
        timeMap['createRecord'] += t1 - t0
    else:
        timeMap['createRecord'] = t1 - t0

    return inst
