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
#    
# 
#

import numpy, pypies, logging, time
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
                  numpy.object_: StringDataRecord,
                  numpy.string_: StringDataRecord
}

def createStorageRecord(rawData, ds):
    t0=time.time()

    t = typeToClassMap[rawData.dtype.type]
    inst = t()
    name = ds.name
    parentName = '/'
    slashIndex = name.rfind('/')
    if slashIndex > -1:
        parentName = name[0:slashIndex]
        name = name[slashIndex+1:]
    inst.setName(name)
    inst.setGroup(parentName)
    inst.putDataObject(rawData)
    inst.setDimension(len(ds.shape))    
    sizes = []
    if logger.isEnabledFor(logging.DEBUG):
        logger.debug("rawData.shape " + str(rawData.shape))
    for x in rawData.shape:
        sizes.append(long(x))
    sizes.reverse()
    inst.setSizes(sizes)
        
    fillValue = None
    if rawData.dtype.type != numpy.object_ and rawData.dtype.type != numpy.string_:        
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
    compression = 'NONE'
    if ds.compression:
        compression = ds.compression
    if compression != 'LZF' and compression != 'ZLIB':
        compression = 'NONE'
    props.setCompression(compression)
    # TODO downscaled?
    inst.setProps(props)
    
    t1=time.time()
    if timeMap.has_key('createRecord'):
        timeMap['createRecord']+=t1-t0
    else:
        timeMap['createRecord']=t1-t0
    
    return inst
