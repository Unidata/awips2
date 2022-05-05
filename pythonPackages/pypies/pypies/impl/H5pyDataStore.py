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
# h5py implementation of IDataStore
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/16/10                      njensen        Initial Creation.
#    05/03/11           9134       njensen        Optimized for pointdata
#    10/09/12                      rjpeter        Optimized __getGroup for retrievals
#    01/17/13        DR 15294      D. Friedman    Clear out data in response
#    02/12/13           1608       randerso       Added support for explicitly deleting groups and datasets
#    Nov 14, 2013       2393       bclement       removed interpolation
#    Jan 17, 2014       2688       bclement       added file action and subprocess error logging
#    Mar 19, 2014       2688       bgonzale       added more subprocess logging.  Return value from
#                                                 subprocess.check_output is not return code, but is
#                                                 process output.  h5repack has no output without -v arg.
#    Apr 24, 2015       4425       nabowle        Add DoubleDataRecord
#    Jun 15, 2015   DR 17556      mgamazaychikov  Add __doMakeReadable method to counteract umask 027 daemon
#                                                 and make copied files world-readable
#    Jul 27, 2015       4402       njensen        Set fill_time_never on write if fill value is None
#    Jul 30, 2015       1574       nabowle        Add deleteOrphanFiles()
#    Aug 20, 2015   DR 17726      mgamazaychikov  Remove __doMakeReadable method
#    Sep 14, 2015       4868       rjpeter        Updated writePartialHDFData to create the dataset if
#                                                 it doesn't exist.
#    Oct 20, 2015       4982       nabowle        Verify datatypes match when replacing data.
#    Feb 15, 2016       3857       tgurney        Handle lowercase compression type (e.g. 'lzf')
#    Feb 24, 2016       5389       nabowle        Orphan purging now uses subdirectory patterns with
#                                                 separate reftimes, if applicable.
#    Feb 26, 2016       5420       tgurney        Store and check for time of last repack
#    Feb 29, 2016       5420       tgurney        Remove timestampCheck arg from copy()
#    Apr 22, 2016       5389       nabowle        Fix purging false-orphans.
#    Nov 15, 2016       5992       bsteffen       Support storing compressed records.
#    Dec 22, 2016    DR 19477      mporricelli    Check that file exists in copy() before attempting file action
#    May 05, 2017       6256       tgurney        Set restricted file permissions on copy()
#    May 12, 2017       6256       tgurney        Set restricted file permissions on repack
#    Jun 14, 2017       6256       tgurney        Set permissions on correct files (bug fix)
#    Aug 16, 2018       6720       njensen        Improved __getNode() error message, removed __getGroup()
#    Sep 14, 2018       6448       dlovely        Fixed issue with h5repack and LZF compression
#    Sep 19, 2018       7435       ksunil         Eliminate compression/decompression on HDF5
#    Oct 11, 2018       7306       dgilling       Force getDatasets to always return
#                                                 ndarray of type numpy.string_.
#    May 15, 2019       7847       dlovely        Fix for segmentation fault using LZF compression
#    Jun 25, 2019       7885       tgurney        Python 3 fixes
#    Jul 23, 2019       7885       tgurney        Decode h5repack subprocess output
#    Aug 13, 2019       7885       tgurney        Fix str/bytes issue with
#                                                 delete requests
#    Jan 28, 2020       7985       ksunil         Removed the compression changes introduced in 7435
#    Feb 11, 2020       7628       bsteffen       Ensure __retrieve only returns datasets.
#    Jan 17, 2022       8729       rjpeter        Call method cleanUp

import fnmatch
import h5py
import logging
import numpy
import os
import re
import shutil
import stat # for h5repack
import subprocess # for h5repack
import time
import traceback

import pypies
from datetime import datetime, timedelta
from pypies import IDataStore, StorageException, NotImplementedException
from pypies import MkDirLockManager as LockManager
from . import HDF5OpManager, DataStoreFactory

from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage import *
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage.records import *
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.records import *
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.response import *

logger = pypies.logger
timeMap = pypies.timeMap
fullDiskThreshold = pypies.fullDiskThreshold

vlen_str_type = h5py.special_dtype(vlen=bytes)

FILE_PERMISSIONS = 0o0640

dataRecordMap = {
    ByteDataRecord: numpy.byte,
    ShortDataRecord: numpy.short,
    IntegerDataRecord: numpy.int32,
    LongDataRecord: numpy.int64,
    FloatDataRecord: numpy.float32,
    DoubleDataRecord: numpy.float64,
    StringDataRecord: bytes,
}

DEFAULT_CHUNK_SIZE = 256
FILESYSTEM_BLOCK_SIZE = 4096
REQUEST_ALL = Request()
REQUEST_ALL.setType('ALL')

PURGE_REGEX = re.compile('(/[a-zA-Z]{1,25})/([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9]{2}):[0-9]{2}:[0-9]{2}')

# matches the date formats used in hdf5 filenames.
ORPHAN_REGEX = re.compile('(19|20)(\d\d)-?(0[1-9]|1[012])-?(0[1-9]|[12][0-9]|3[01])')
# a buffer, in seconds, to ignore purging recently-modified files.
ORPHAN_MOD_BUFFER = 300


class H5pyDataStore(IDataStore.IDataStore):

    def __init__(self):
        pass

    def store(self, request):
        fn = request.getFilename()
        errorResp = self.__checkDiskSpace(fn)
        if errorResp:
            return errorResp

        recsAndMetadata = request.getRecordsAndMetadata()
        recs = [rm.getRecord() for rm in recsAndMetadata]
        self.__prepareRecordsToStore(recs)
        f, lock = self.__openFile(fn, 'w')
        try:
            op = request.getOp()
            status = StorageStatus()
            exc = []
            failRecs = []
            ss = None
            t0 = time.time()
            for r in recs:
                try:
                    ss = self.__writeHDF(f, r, op)
                except:
                    logger.warn("Exception occurred on file " + fn + ":" + IDataStore._exc())
                    exc.append(IDataStore._exc())
                    # Clear out data so we don't send the whole thing back to the client.
                    # NOTE: This assumes pypies no longer needs the data
                    r.putDataObject(None)
                    failRecs.append(r)

            if ss:
                status.setOperationPerformed(ss['op'])
                if 'index' in ss:
                    status.setIndexOfAppend(ss['index'])
            t1 = time.time()
            timeMap['store']= t1 - t0
            resp = StoreResponse()
            resp.setStatus(status)
            resp.setExceptions(exc)
            resp.setFailedRecords(failRecs)
            return resp
        finally:
            t0 = time.time()
            f.close()
            t1 = time.time()
            timeMap['closeFile']=t1-t0
            LockManager.releaseLock(lock)

    def __prepareRecordsToStore(self, records):
        for r in records:
            if hasattr(r, 'prepareStore'):
                t0 = time.time()
                r.prepareStore()
                t1 = time.time()
                timeKey = r.prepareStore.__name__
                if timeKey in timeMap:
                    timeMap[timeKey] += t1 - t0
                else:
                    timeMap[timeKey] = t1 - t0

    def __writeHDF(self, f, record, storeOp):
        props = record.getProps()
        if (props and not props.getChunked() and props.getCompression() and
                props.getCompression() != 'NONE'):
            raise StorageException('Data must be chunked to be compressed')

        data = record.retrieveDataObject()
        rootNode=f['/']
        group = self.__getNode(rootNode, record.getGroup(), None, create=True)
        if record.getMinIndex() is not None and len(record.getMinIndex()):
            ss = self.__writePartialHDFDataset(f, data, record.getDimension(), record.getSizes(), record.getName(),
                                               group, props, self.__getHdf5Datatype(record), record.getMinIndex(),
                                               record.getMaxSizes(), record.getFillValue())
        else:
            ss = self.__writeHDFDataset(f, data, record.getDimension(), record.getSizes(), record.getName(),
                                   group, props, self.__getHdf5Datatype(record), storeOp, record)

        f.flush()
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug("Stored group " + str(record.getGroup()) + " to group " + str(group))
        return ss

    def __writeHDFDataset(self, f, data, dims, szDims, dataset, group, props, dataType, storeOp, rec):
        nDims = len(szDims)
        szDims1 = [None,] * nDims
        maxDims = [None,] * nDims
        recMaxDims = rec.getMaxSizes()
        for i in range(nDims):
            szDims1[i] = szDims[nDims - i - 1]
            if recMaxDims is None or recMaxDims[i] == 0:
                maxDims[i] = None
            else:
                maxDims[i] = recMaxDims[i]

        if type(data) is numpy.ndarray and data.shape != tuple(szDims1):
            data = data.reshape(szDims1)

        ss = {}
        if dataset in group:
            ds = group[dataset]
            if storeOp == 'STORE_ONLY':
                raise StorageException('Dataset ' + str(dataset) + ' already exists in group ' + str(group))
            elif storeOp == 'APPEND':
                if dims == 1:
                    newSize = [ds.shape[0] + szDims1[0]]
                elif dims == 2:
                    newSize = [ds.shape[0] + szDims1[0], ds.shape[1]]
                else:
                    raise StorageException('More than 2 dimensions not currently supported.')
                startIndex = ds.shape[0]
                ds.resize(newSize)
                ds[startIndex:] = data
                ss['op'] = 'APPEND'
                indices = [startIndex]
                if len(ds.shape) > 1:
                    indices.append(0)
                ss['index'] = indices
            elif storeOp == 'REPLACE' or storeOp == 'OVERWRITE':
                if ds.dtype.type != data.dtype.type:
                    raise StorageException("Cannot " + storeOp + " data of type " + ds.dtype.name + " with data of type " + data.dtype.name + " in " + f.filename + " " + group.name + ".")
                if ds.shape != data.shape:
                    ds.resize(data.shape)
                ds[()] = data
                ss['op'] = 'REPLACE'
        else:
            chunk = self.__calculateChunk(nDims, dataType, storeOp, maxDims)
            compression = None
            if props:
                compression = props.getCompression()
            fillValue = rec.getFillValue()

            ds = self.__createDatasetInternal(group, dataset, dataType, szDims1, maxDims, chunk, compression, fillValue)
            #ds = group.create_dataset(dataset, szDims1, dataType, maxshape=maxDims, chunks=chunk, compression=compression)

            ds[()] = data
            ss['op'] = 'STORE_ONLY'

        self.__writeProperties(rec, ds)
        return ss

    def __getHdf5Datatype(self, record):
        if record.__class__ in dataRecordMap:
            dtype = dataRecordMap[record.__class__]
        else:
            dtype = record.determineStorageType()
        if dtype is bytes:
            size = record.getMaxLength()
            if size > 0:
                dtype = h5py.h5t.py_create('S' + str(size))
            else:
                dtype = vlen_str_type
        return dtype


    # Common use case of arrays are passed in x/y and orientation of data is y/x
    def __reverseDimensions(self, dims):
        revDims = [None, ] * len(dims)
        for i in range(len(dims)):
            revDims[i] = dims[len(dims) - i - 1]
        return revDims

    def __calculateChunk(self, nDims, dataType, storeOp, maxDims):
        if nDims == 1:
            if dataType != vlen_str_type:
                sizeOfEntry = numpy.dtype(dataType).itemsize
                chunkSize = int(FILESYSTEM_BLOCK_SIZE // sizeOfEntry)
                chunk = [chunkSize]
            else:
                chunk = [DEFAULT_CHUNK_SIZE]
        elif nDims == 2:
            if storeOp != 'APPEND':
                chunk = [DEFAULT_CHUNK_SIZE] * 2
            else:
                # Since a filesystem will read in at least one block, we optimize
                # to read as large a chunk as possible without reading more
                # than one block.  Therefore, if it's 2 dimensional and an
                # append (pointdata), dynamically calculate chunk based on
                # filesystem's blocksize and fixed second dimension.
                # So fixed dimension gets chunk size of:
                #        blocksize / (fixed dimension size * bytesize)
                # For example, float of dimensions (infinite, 6) would be
                #        (int(4096 / (6 * 4)), 6)
                if dataType != vlen_str_type:
                    sizeOfEntry = numpy.dtype(dataType).itemsize
                else:
                    sizeOfEntry = None

                secondDim = maxDims[1]
                if sizeOfEntry:
                    chunkSize = int(FILESYSTEM_BLOCK_SIZE / (secondDim * sizeOfEntry))
                    chunk = [chunkSize, secondDim]
                else:
                    chunk = [DEFAULT_CHUNK_SIZE, secondDim]
        else:
            raise NotImplementedException("Storage of " + str(nDims) + " dimensional " +
                                   "data with mode " + storeOp + " not supported yet")

        # ensure chunk is not bigger than dimensions
        if maxDims is not None:
            for i in range(nDims):
                chunk[i] = chunk[i] if maxDims[i] is None else min(chunk[i], maxDims[i])

        chunk = tuple(chunk)
        return chunk

    def __writeProperties(self, dr, dataset):
        attrs = dr.getDataAttributes()
        if attrs:
            for key in attrs:
                dataset.attrs[key] = attrs[key]

    def __writePartialHDFDataset(self, f, data, dims, szDims, dataset, group, props, dataType,
                                                minIndex, maxSizes, fillValue):
        # Change dimensions to be Y/X
        szDims1 = self.__reverseDimensions(szDims)
        offset = self.__reverseDimensions(minIndex)

        data = data.reshape(szDims1)

        ss = {}
        if dataset in group:
            ds=group[dataset]
            ss['op'] = 'REPLACE'
            if ds.dtype.type != data.dtype.type:
                raise StorageException("Cannot REPLACE data of type " + ds.dtype.name + " with data of type " + data.dtype.name + " in " + f.filename + " " + group.name + ".")
        else:
            if maxSizes is None:
                raise StorageException('Dataset ' + dataset + ' does not exist for partial write.  MaxSizes not specified to create initial dataset')

            maxDims = self.__reverseDimensions(maxSizes)
            nDims = len(maxDims)
            chunk = self.__calculateChunk(nDims, dataType, 'STORE_ONLY', maxDims)
            compression = None
            if props:
                compression = props.getCompression()
            ds = self.__createDatasetInternal(group, dataset, dataType, maxDims, None, chunk, compression, fillValue)
            ss['op'] = 'STORE_ONLY'

        if ds.shape[0] < data.shape[0] or ds.shape[1] < data.shape[1]:
            raise StorageException('Partial write larger than original dataset.  Original shape [' + str(ds.shape) + '], partial ')

        endIndex = [offset[0] + szDims1[0], offset[1] + szDims1[1]]
        ds[offset[0]:endIndex[0], offset[1]:endIndex[1]] = data

        return ss


    def delete(self, request):
        fn = request.getFilename()
        f, lock = self.__openFile(fn, 'w')
        resp = DeleteResponse()
        resp.setSuccess(True)
        deleteFile = False

        try:
            datasets = request.getDatasets()
            if datasets is not None:
                for dataset in datasets:
                    ds = None
                    try :
                        ds = self.__getNode(f, None, dataset)
                    except Exception as e:
                        logger.warn('Unable to find uri [' + str(dataset) + '] in file [' + str(fn) + '] to delete: ' + IDataStore._exc())

                    if ds:
                        parent = ds.parent
                        if isinstance(ds.name, str):
                            parent.id.unlink(ds.name.encode())
                        else:
                            parent.id.unlink(ds.name)

            groups = request.getGroups()
            if groups is not None:
                for group in groups:
                    gp = None
                    try :
                        gp = self.__getNode(f, group)
                    except Exception as e:
                        logger.warn('Unable to find uri [' + str(group) + '] in file [' + str(fn) + '] to delete: ' + IDataStore._exc())

                    if gp:
                        parent = gp.parent
                        if isinstance(gp.name, str):
                            parent.id.unlink(gp.name.encode())
                        else:
                            parent.id.unlink(gp.name)

        finally:
            # check if file has any remaining data sets
            # if no data sets, flag file for deletion
            deleteFile = False
            try:
                f.flush()
                deleteFile = not self.__hasDataSet(f)
            except Exception as e:
                logger.error('Error occurred checking for dataSets in file [' + str(fn) + ']: ' + IDataStore._exc())

            t0 = time.time()
            f.close()
            t1 = time.time()
            timeMap['closeFile']=t1-t0

            if deleteFile:
                logger.info('Removing empty file ['+ str(fn) + ']')
                try:
                    os.remove(fn)
                except Exception as e:
                    logger.error('Error occurred deleting file [' + str(fn) + ']: ' + IDataStore._exc())


            LockManager.releaseLock(lock)
        return resp

    # recursively looks for data sets
    def __hasDataSet(self, group):
        for key in group:
            child=group[key]
            if type(child) == h5py.highlevel.Dataset:
                return True
            elif type(child) == h5py.highlevel.Group:
                if self.__hasDataSet(child):
                    return True
        return False

    def retrieve(self, request):
        fn = request.getFilename()
        f, lock = self.__openFile(fn, 'r')
        try:
            group = request.getGroup()
            req = request.getRequest()
            rootNode=f['/']
            if req:
                ds = self.__getNode(rootNode, group, request.getDataset())
                result = [self.__retrieveInternal(ds, req)]
            else:
                groupNode = self.__getNode(rootNode, group)
                result = self.__retrieve(groupNode)
            resp = RetrieveResponse()
            resp.setRecords(result)
            return resp
        finally:
            t0 = time.time()
            f.close()
            t1 = time.time()
            timeMap['closeFile']=t1-t0
            LockManager.releaseLock(lock)

    def __retrieve(self, group):
        records = []
        datasets = list(group.keys())
        for ds in datasets:
            dataset = group[ds]
            if type(dataset) == h5py.highlevel.Dataset:
                rec = self.__retrieveInternal(dataset, REQUEST_ALL)
                records.append(rec)

        return records

    def __retrieveInternal(self, ds, req):
        rawData = HDF5OpManager.read(ds, req)
        rec = DataStoreFactory.createStorageRecord(rawData, ds, req)
        return rec

    def retrieveDatasets(self, request):
        t6 = time.time()
        fn = request.getFilename()
        t0 = time.time()
        f, lock = self.__openFile(fn, 'r')
        t1 = time.time()
        t2 = 0
        t3 = 0
        names = []
        paramMap = {}
        timeSpentReading = 0
        try:
            names = request.getDatasetGroupPath()
            req = request.getRequest()
            result = []
            rootNode=f['/']
            for dsName in names:
                ds = self.__getNode(rootNode, None, dsName)
                t2 = time.time()
                rawData = HDF5OpManager.read(ds, req)
                t3 = time.time()
                diff = t3 - t2
                timeSpentReading += diff
                paramMap[dsName] = ('%.3f' % (diff))
                rec = DataStoreFactory.createStorageRecord(rawData, ds, req)
                result.append(rec)

            resp = RetrieveResponse()
            resp.setRecords(result)
            return resp
        finally:
            f.close()
            t4 = time.time()
            LockManager.releaseLock(lock)
            t5 = time.time()
            if logger.isEnabledFor(logging.DEBUG):
                logger.debug("pid=" + str(os.getpid()) + " filename=" + fn +
                        ", numberOfDatasets/Parameters=" + str(len(names)) +
                        ", getLockTime=" +  ('%.3f' % (t1-t0)) +
                        ", readDataTime=" + ('%.3f' % (timeSpentReading)) +
                        ", releaseLockTime=" + ('%.3f' % (t5-t4)) +
                        ", retrieveDatasetsTotal=" + ('%.3f' % (t4-t6)) +
                        ", perParamRead=" + str(paramMap))

    def retrieveGroups(self, request):
        fn = request.getFilename()
        f, lock = self.__openFile(fn, 'r')
        try:
            groups = request.getGroups()
            req = request.getRequest()
            recs = []
            rootNode=f['/']
            for group in groups:
                grp = self.__getNode(rootNode, group)
                datasets = grp.keys()
                for ds in datasets:
                    dsNode=grp[ds]
                    rawData = HDF5OpManager.read(dsNode, req)
                    rec = DataStoreFactory.createStorageRecord(rawData, dsNode, req)
                    recs.append(rec)
            resp = RetrieveResponse()
            resp.setRecords(recs)
            return resp
        finally:
            t0 = time.time()
            f.close()
            t1 = time.time()
            timeMap['closeFile']=t1-t0
            LockManager.releaseLock(lock)

    def getDatasets(self, request):
        fn = request.getFilename()
        f, lock = self.__openFile(fn, 'r')
        try:
            grpName = request.getGroup()
            grp = self.__getNode(f['/'], grpName)
            ds = list(grp.keys())
            return numpy.array(ds, dtype=numpy.string_)
        finally:
            t0 = time.time()
            f.close()
            t1 = time.time()
            timeMap['closeFile']=t1-t0
            LockManager.releaseLock(lock)

    def deleteFiles(self, request):
        fn = request.getFilename()
        if os.path.exists(fn):
            if os.path.isdir(fn):
                self.__recursiveDeleteFiles(fn,request.getDatesToDelete())
            else:
                self.__removeFile(fn)

        # if it didn't succeed, an exception will be thrown and therefore
        # an error response returned
        resp = DeleteResponse()
        resp.setSuccess(True)
        return resp

    def createDataset(self, request):
        fn = request.getFilename()
        errorResp = self.__checkDiskSpace(fn)
        if errorResp:
            return errorResp

        f, lock = self.__openFile(fn, 'w')
        try:
            rec = request.getRecord()
            props = rec.getProps()
            if props and not props.getChunked() and props.getCompression != 'NONE':
                raise StorageException("Data must be chunked to be compressed")
            grp = rec.getGroup()
            group = self.__getNode(f['/'], grp, None, create=True)

            # reverse sizes for hdf5
            szDims = rec.getSizes()
            szDims1 = self.__reverseDimensions(szDims)
            szDims = tuple(szDims1)

            chunks = None
            if props and props.getChunked():
                chunks = (DEFAULT_CHUNK_SIZE,) * len(szDims)

            compression = None
            if props:
                compression = props.getCompression()
            dtype = self.__getHdf5Datatype(rec)
            datasetName = rec.getName()
            fillValue = rec.getFillValue()
            ds = self.__createDatasetInternal(group, datasetName, dtype, szDims,
                                                        szDims, chunks, compression, fillValue)
            self.__writeProperties(rec, ds)
            f.flush()
            resp = StoreResponse()
            return resp
        finally:
            t0 = time.time()
            f.close()
            t1 = time.time()
            timeMap['closeFile']=t1-t0
            LockManager.releaseLock(lock)

    def __createDatasetInternal(self, group, datasetName, dtype, szDims,
                                                        maxDims=None, chunks=None, compression=None, fillValue=None):

        plc = h5py.h5p.create(h5py.h5p.DATASET_CREATE)
        if fillValue is not None:
            fVal = numpy.zeros(1, dtype)
            fVal[0] = fillValue
            plc.set_fill_value(fVal)
            plc.set_fill_time(h5py.h5d.FILL_TIME_IFSET)
        elif dtype != vlen_str_type:
            plc.set_fill_time(h5py.h5d.FILL_TIME_NEVER)

        if chunks:
            plc.set_chunk(chunks)
        if compression is not None and compression.upper() == 'LZF':
            plc.set_shuffle()
            plc.set_filter(h5py.h5z.FILTER_LZF, h5py.h5z.FLAG_OPTIONAL)

        szDims = tuple(szDims)
        if maxDims is not None:
            maxDims = tuple(x if x is not None else h5py.h5s.UNLIMITED for x in maxDims)


        space_id = h5py.h5s.create_simple(szDims, maxDims)
        type_id = h5py.h5t.py_create(dtype, logical=True)

        h5py.h5d.create(group.id, datasetName.encode(), type_id, space_id, plc)
        ds = group[datasetName]
        return ds

    def __recursiveDeleteFiles(self, dir, datesToDelete):
        if os.path.exists(dir) and os.path.isdir(dir):

            if datesToDelete is None:
                self.__removeDir(dir)
            else:
                files = os.listdir(dir)
                for f in files:
                    fullpath = dir + '/' + f
                    if os.path.isdir(fullpath):
                        self.__recursiveDeleteFiles(fullpath, datesToDelete)
                    else:
                        for deleteDate in datesToDelete:
                            try:
                                matches = PURGE_REGEX.search(deleteDate)
                                ymd = matches.group(2)+'-'+matches.group(3)
                                if f.find(ymd) != -1:
                                    self.__removeFile(fullpath)
                            except:
                                pass

    def __removeFile(self, path):
        gotLock = False
        try:
            gotLock, lock = LockManager.getLock(path, 'a')
            if gotLock:
                os.remove(path)
            else:
                raise StorageException('Unable to acquire lock on file ' + path + ' for deleting')
        finally:
            if gotLock:
                LockManager.releaseLock(lock)

    def __removeDir(self, path, onlyIfEmpty=False):
        gotLock = False
        try:
            gotLock, lock = LockManager.getLock(path, 'a')
            if gotLock:
                if onlyIfEmpty:
                    os.rmdir(path)
                else:
                    shutil.rmtree(path)
            else:
                raise StorageException('Unable to acquire lock on file ' + path + ' for deleting')
        finally:
            if gotLock:
                LockManager.releaseLock(lock)

    def __openFile(self, filename, mode='r'):
        if mode == 'r' and not os.path.exists(filename):
            raise StorageException('File ' + filename + ' does not exist')
        gotLock, fd = LockManager.getLock(filename, mode)
        t0 = time.time()
        if not gotLock:
            raise StorageException('Unable to acquire lock on file ' + filename)
        try:
            if mode == 'w' and os.path.exists(filename):
                mode = 'a'
            f = h5py.File(filename, mode)
        except Exception as e:
            msg = "Unable to open file " + filename + ": " + IDataStore._exc()
            logger.error(msg)
            LockManager.releaseLock(fd)
            raise e

        t1 = time.time()
        timeMap['openFile']=t1-t0
        return f, fd

    def __getNode(self, rootNode, groupName, dsName=None, create=False):
        t0 = time.time()

        # expected output to be node for /group1::group2::group3/dataSet
        # expected output of /group1::group2::group3/dataSet-interpolated/1
        if groupName:
            if dsName:
                toNormalize=groupName + '/' + dsName
            else:
                toNormalize=groupName
        elif dsName:
            toNormalize=dsName
        else:
            # both None, return root node as default
            return rootNode
        tokens=toNormalize.split('/')

        # remove any empty tokens
        tokens = [token for token in tokens if token]

        dsNameToken=None
        if dsName:
            # data set name was given, keep last token for ds name
            dsNameToken = tokens.pop()

        # need to check final token for -interpolated
        isInterpToken = None
        if tokens:
            isInterpToken = tokens[-1]
            if isInterpToken.endswith('-interpolated'):
                del tokens[-1]
                if dsNameToken:
                    dsNameToken = isInterpToken + '/' + dsNameToken
                else:
                    dsNameToken = isInterpToken

        if tokens:
            basePath='::'.join(tokens)
        else:
            basePath=None

        node = None
        if create:
            if basePath is None:
                node = rootNode
            if basePath in rootNode.keys():
                node = rootNode[basePath]
            else:
                node = rootNode.create_group(basePath)

            if dsNameToken:
                for token in dsNameToken.split('/'):
                    if token in node.keys():
                        node = node[token]
                    else:
                        node = node.create_group(token)
        else:
            if dsNameToken:
                if basePath:
                    basePath += '/' + dsNameToken
                else:
                    basePath = dsNameToken

            try:
                if basePath:
                    node = rootNode[basePath]
                else:
                    node = rootNode
            except:
                msg = "Unable to get dataset "
                if basePath:
                    msg += basePath
                msg += ": " + IDataStore._exc()
                logger.error(msg)
                raise StorageException(msg)

        t1 = time.time()
        if 'getGroup' in timeMap:
            timeMap['getGroup']+=t1-t0
        else:
            timeMap['getGroup']=t1-t0

        return node



    def __link(self, group, linkName, dataset):
        # this is a hard link
        group[linkName] = dataset

        # untested code, this would be if we went with symbolic links, this is unpublished h5py API
        #id = group.id
        #id.link(dataset.name, linkName, h5py.h5g.LINK_SOFT)

    def copy(self, request):
        resp = FileActionResponse()
        file = request.getFilename()
        repack = request.getRepack()
        action = self.__doCopy if not repack else self.__doRepack
        if os.path.exists(file):
            pth = os.path.split(file)[0]
            self.__doFileAction(file, pth, request.getOutputDir(), action, resp, request.getRepackCompression())
        else:
            actionName = self.__getFileActionName(action)
            logger.error("Error performing action '" + actionName + "' on file " + file + " - No such file or directory")
            resp.setFailedFiles([file])
        return resp

    def __doCopy(self, filepath, basePath, outputDir, compression):
        shutil.copy(filepath, outputDir)
        destFile = os.path.join(outputDir, os.path.basename(filepath))
        destExists = os.path.isfile(destFile)
        if destExists:
            # This is exclusively for archive--it is assumed that the file is
            # being copied to a NFS share, so it needs restricted permissions
            os.chmod(destFile, FILE_PERMISSIONS)

    __doCopy.__display_name__ = 'copy'

    def repack(self, request):
        resp = FileActionResponse()
        pth = request.getFilename()
        files = self.__listHdf5Files(pth)
        compression = request.getCompression()
        for f in files:
            self.__doFileAction(f, pth, None, self.__doRepack, resp, compression, 'lastRepacked')
        return resp

    def __listHdf5Files(self, pth):
        results = []
        for base, dirs, files in os.walk(pth):
            goodfiles = fnmatch.filter(files, '*.h5')
            results.extend(os.path.join(base, f) for f in goodfiles)
        return results

    def __doRepack(self, filepath, basePath, outDir, compression):
        t0 = time.time()
        # call h5repack to repack the file
        if outDir is None:
            repackedFullPath = filepath + '.repacked'
        else:
            repackedFullPath = filepath.replace(basePath, outDir)
        #7847 - Fix for segmentation fault using LZF compression in newer
        # versions of HDF5. LZF support was from 1.6.5-1.8.3 per
        # https://support.hdfgroup.org/services/filters.html
        if compression == 'LZF':
            compression = 'GZIP=1'
        cmd = ['h5repack', '-f', compression, filepath, repackedFullPath]
        if logger.isEnabledFor(logging.DEBUG):
            cmd.insert(1, '-v')
        success = True
        ret = None
        try:
            ret = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
            # if CalledProcessError is not raised then success
        except subprocess.CalledProcessError as e:
            logger.error("Subprocess call failed: " + str(e))
            logger.error("Subprocess args: " + " ".join(cmd))
            logger.error("Subprocess output: " + e.output.decode())
            success = False
        except:
            logger.exception("Unexpected error from h5repack")
            success = False
        finally:
            logger.debug("h5repack output: " + str(ret))

        if success:
            # repack was successful, replace the old file if we did it in the
            # same directory, otherwise leave it alone
            if outDir is None:
                os.remove(filepath)
                os.rename(repackedFullPath, filepath)
                os.chmod(filepath, stat.S_IWUSR | stat.S_IWGRP | stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
            else:
                # File is being copied to a different directory. As of May 2017
                # this code path is only taken by archive, when doing a
                # copy+repack. In this case it is assumed that the file is
                # being copied to an NFS share, so it needs restricted
                # permissions.
                os.chmod(repackedFullPath, FILE_PERMISSIONS)
        else:
            # remove failed new file if there was one
            if os.path.exists(repackedFullPath):
                os.remove(repackedFullPath)
            if outDir is not None:
                # repack failed, but they wanted the data in a different
                # directory, so just copy the original data without the repack
                shutil.copy(filepath, repackedFullPath)
                # chmod here is for setting correct permissions on /archive
                # files. This code needs to be revisited if this code path is
                # used by something other than archive in the future.
                os.chmod(repackedFullPath, FILE_PERMISSIONS)
        t1 = time.time()
        if 'repack' in timeMap:
            timeMap['repack'] += t1 - t0
        else:
            timeMap['repack'] = t1 - t0
        return success

    __doRepack.__display_name__ = 'repack'

    def __getFileActionName(self, fileAction):
        """Retrieve __display_name__ attribute from function object or __name__ if not present"""
        if hasattr(fileAction, '__display_name__'):
            actionName = fileAction.__display_name__
        else:
            actionName = fileAction.__name__
        return actionName

    def __doFileAction(self, filepath, basePath, outputDir, fileAction, response, compression='NONE', timestampAttr=None):
        """
        Perform specified action on a file.

        Args:
                filepath: hdf5 file to operate on
                basePath: Used by specified file action
                outputDir: Used by specified file action
                fileAction: Action to perform. Must take arguments
                    (filepath, basePath, outputDir, compression)
                response: Response object. Will be populated with list of
                    files on which the operation succeeded or failed
                compression: Optional compression type
                timestampAttr: Optional, name of hdf5 attribute that stores
                    a timestamp indicating when the action was
                    last performed on this file
        Returns:
                None
        """

        lock = None

        try:
            f, lock = self.__openFile(filepath, 'a')
            proceedWithAction = True
            if timestampAttr is not None and timestampAttr in f.attrs.keys():
                    # int() required because float timestamps are getting
                    # truncated to 5 significant digits by h5
                    lastActionTime = int(f.attrs[timestampAttr])
                    lastModifiedTime = int(os.path.getmtime(filepath))
                    if lastActionTime > lastModifiedTime:
                        proceedWithAction = False
            if proceedWithAction:
                # update time even if action will fail, cause if it fails at
                # this time there's no point in retrying.  put time in the near future
                # cause the modified time will be after the action and rename
                if timestampAttr is not None:
                    f.attrs[timestampAttr] = int(time.time() + 30)
                f.close()

                success = fileAction(filepath, basePath, outputDir, compression)

                # update response
                if success:
                    getter = response.getSuccessfulFiles
                    setter = response.setSuccessfulFiles
                else:
                    actionName = self.__getFileActionName(fileAction)
                    logger.warn("Action '" + actionName + "' failed on file " + filepath)
                    getter = response.getFailedFiles
                    setter = response.setFailedFiles
                responseList = getter()
                if responseList:
                    responseList += [filepath]
                else:
                    responseList = [filepath]
                setter(responseList)
        except Exception as e:
            actionName = self.__getFileActionName(fileAction)
            logger.error("Error performing action '" + actionName + "' on file " + filepath + ": " + str(e))
            logger.error(traceback.format_exc())
            failed = response.getFailedFiles()
            if failed:
                failed += [filepath]
            else:
                failed = [filepath]
            response.setFailedFiles(failed)
        finally:
            if lock:
                LockManager.releaseLock(lock)

    def deleteOrphanFiles(self, request):
        path = request.getFilename()
        oldestDateMap = request.getOldestDateMap()
        logger.info('Starting orphan file purge for ' + path)
        startTime = time.time()
        deletedFiles = []
        failedFiles = []
        if oldestDateMap:
            purgeMap = dict()
            # compile the key to a regex, and remap it to the datetime
            for key, oldestDate in oldestDateMap.items():
                oldestDatetime = datetime.utcfromtimestamp(oldestDate.getTime()//1000)
                purgeMap[re.compile(key)] = oldestDatetime

            # Walk the plugin directory looking for orphaned files from the
            # bottom-up so we can remove any empty directories as we go.
            # base is the current path, and dirs and files are the directories
            # and files in the current directory specified by base.
            for base, dirs, files in os.walk(path, topdown=False):
                datafiles = fnmatch.filter(files, '*.h5')
                compareTime = None
                # Find a path regex that matches our current directory, if
                # possible.
                for purgeKey, oldestDatetime in purgeMap.items():
                    if purgeKey.search(base):
                        compareTime = oldestDatetime
                        break

                # For each file, figure out if it matches the file pattern for
                # hdf5 files that we may want to purge and if it has recently
                # been modified.
                # If a matching directory regex was found above, we can use the
                # mapped date to determine if a file should be purged.
                # If a matching pattern was not found, and isn't recently modified
                # it will be purged.
                for f in datafiles:
                    matches = ORPHAN_REGEX.search(f)
                    if matches:
                        f = os.path.join(base, f)
                        mtime = os.path.getmtime(f)
                        fileAgeSeconds = time.time() - mtime

                        # Files that are recently modified are considered not to
                        # be orphaned. This protects newly written files.
                        if fileAgeSeconds > ORPHAN_MOD_BUFFER:
                            stringDate = matches.group(1) + matches.group(2) + \
                                             matches.group(3) + matches.group(4)
                            filedate = datetime.strptime(stringDate, "%Y%m%d")
                            if compareTime:
                                # file matches a known reftime. compare the filename
                                # against that reftime.
                                if filedate < compareTime:
                                    try:
                                        logger.info("Deleting orphaned file " + f + ", age " + str(datetime.utcnow() - filedate))
                                        self.__removeFile(f)
                                        deletedFiles.append(f)
                                    except Exception as e:
                                        logger.error("Error deleting file " + f + ": " + str(e))
                                        logger.error(traceback.format_exc())
                                        failedFiles.append(f)
                            else:
                                # The file matches ORPHAN_REGEX, but did
                                # not match any known subdirectory pattern for the
                                # plugin and isn't a recently modified file.
                                try:
                                    logger.info("Deleting orphaned file " + f + ", age " + str(datetime.utcnow() - filedate) + ", unmodified for " + str(timedelta(seconds=fileAgeSeconds)))
                                    self.__removeFile(f)
                                    deletedFiles.append(f)
                                except Exception as e:
                                    logger.error("Error deleting file " + f + ": " + str(e))
                                    logger.error(traceback.format_exc())
                                    failedFiles.append(f)
                # cleanup any empty directories at this depth
                fullPathDirs = []
                fullPathDirs.extend(os.path.join(base, d) for d in dirs)
                fullPathDirs.append(os.path.join(base))
                for d in fullPathDirs:
                    if os.path.exists(d) and not os.listdir(d):
                        try:
                            logger.info("Deleting empty directory " + d)
                            self.__removeDir(d, True)
                        except OSError as ose:
                            # Deletion may fail if something writes to it
                            # between checking and trying to delete it, which is
                            # acceptable.
                            if not os.listdir(d):
                                logger.error("Error removing empty directory " + d + ": " + str(ose))
                        except StorageException as se:
                            logger.error("Error removing empty directory " + d + ": " + str(se))

        endTime = time.time()
        logger.info('Completed orphan file purge in {:.3f}ms'.format((endTime-startTime)*1000) )
        LockManager.cleanUp(path)
        resp = FileActionResponse()
        resp.setSuccessfulFiles(deletedFiles)
        resp.setFailedFiles(failedFiles)
        return resp

    def __checkDiskSpace(self, fn):
        if fullDiskThreshold >= 100:
            return None
        usage = None
        try:
            pathToCheck = fn
            while pathToCheck and not os.path.exists(pathToCheck):
                pathToCheck = os.path.dirname(pathToCheck)
            usage = shutil.disk_usage(pathToCheck)
        except:
            logger.error(f'Unable to determine disk usage for {fn}, skipping threshold check: {IDataStore._exc()}')

        if usage and usage.used / usage.total * 100 >= fullDiskThreshold:
            message = f'Disk is full (over {fullDiskThreshold}% threshold) for file: {fn}'
            logger.error(message)
            return ErrorResponse(message, 'DISK_SPACE')
        return None
