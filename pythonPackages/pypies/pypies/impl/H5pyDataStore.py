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
#    06/16/10                      njensen       Initial Creation.
#    05/03/11        9134          njensen       Optimized for pointdata
#    10/09/12                      rjpeter       Optimized __getGroup for retrievals
#
#
#

import h5py, os, numpy, pypies, re, logging, shutil, time, types
import fnmatch
import subprocess, stat  #for h5repack
from pypies import IDataStore, StorageException, NotImplementedException
from pypies import MkDirLockManager as LockManager
#from pypies import LockManager
import HDF5OpManager, DataStoreFactory

from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage import *
from dynamicserialize.dstypes.com.raytheon.uf.common.datastorage.records import *
from dynamicserialize.dstypes.com.raytheon.uf.common.pypies.response import *

logger = pypies.logger
timeMap = pypies.timeMap

vlen_str_type = h5py.new_vlen(str)

dataRecordMap = {
                 ByteDataRecord: numpy.byte,
                 ShortDataRecord: numpy.short,
                 IntegerDataRecord: numpy.int32,
                 LongDataRecord: numpy.int64,
                 FloatDataRecord: numpy.float32,
                 StringDataRecord: types.StringType,
}


DEFAULT_CHUNK_SIZE = 256
FILESYSTEM_BLOCK_SIZE = 4096
DOWNSCALE_THRESHOLD = 512
REQUEST_ALL = Request()
REQUEST_ALL.setType('ALL')

PURGE_REGEX = re.compile('(/[a-zA-Z]{1,25})/([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9]{2}):[0-9]{2}:[0-9]{2}')

class H5pyDataStore(IDataStore.IDataStore):

    def __init__(self):
        pass

    def store(self, request):
        fn = request.getFilename()
        f, lock = self.__openFile(fn, 'w')
        try:
            recs = request.getRecords()
            op = request.getOp()
            status = StorageStatus()
            exc = []
            failRecs = []
            ss = None
            t0=time.time()
            for r in recs:
                try:
                    if r.getProps() and r.getProps().getDownscaled():
                        ss = self.__storeInterpolated(f, r, op)
                    else:
                        ss = self.__writeHDF(f, r, op)
                except:
                    logger.warn("Exception occurred on file " + fn + ":" + IDataStore._exc())
                    exc.append(IDataStore._exc())
                    failRecs.append(r)

            if ss:
                status.setOperationPerformed(ss['op'])
                if ss.has_key('index'):
                    status.setIndexOfAppend(ss['index'])
            t1=time.time()
            timeMap['store']=t1-t0
            resp = StoreResponse()
            resp.setStatus(status)
            resp.setExceptions(exc)
            resp.setFailedRecords(failRecs)
            return resp
        finally:
            t0=time.time()
            f.close()
            t1=time.time()
            timeMap['closeFile']=t1-t0
            LockManager.releaseLock(lock)


    def __writeHDF(self, f, record, storeOp):
        props = record.getProps()
        if props and not props.getChunked() and props.getCompression() and \
            props.getCompression() != 'NONE':
            raise StorageException('Data must be chunked to be compressed')

        data = record.retrieveDataObject()
        rootNode=f['/']
        group = self.__getNode(rootNode, record.getGroup(), None, create=True)
        if record.getMinIndex() is not None and len(record.getMinIndex()):
            ss = self.__writePartialHDFDataset(f, data, record.getDimension(), record.getSizes(),
                                               group[record.getName()], props, record.getMinIndex())
        else:
            ss = self.__writeHDFDataset(f, data, record.getDimension(), record.getSizes(), record.getName(),
                                   group, props, self.__getHdf5Datatype(record), storeOp, record)

            if props and props.getDownscaled():
                intName = record.getGroup() + '/' + record.getName() + '-interpolated'
                intGroup = self.__getNode(rootNode, intName, None, create=True)
                self.__link(intGroup, '0', group[record.getName()])

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
            if recMaxDims == None or recMaxDims[i] == 0:
                maxDims[i] = None
            else:
                maxDims[i] = recMaxDims[i]

        if type(data) is numpy.ndarray and data.shape != tuple(szDims1):
            data = data.reshape(szDims1)

        ss = {}
        if dataset in group.keys():
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
                indices = [long(startIndex)]
                if len(ds.shape) > 1:
                    indices.append(long(0))
                ss['index'] = indices
            elif storeOp == 'REPLACE' or storeOp == 'OVERWRITE':
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
        dtype = dataRecordMap[record.__class__]
        if dtype == types.StringType:
            from h5py import h5t
            size = record.getMaxLength()
            if size > 0:
                dtype = h5t.py_create('S' + str(size))
            else:
                dtype = vlen_str_type
            #dtype.set_strpad(h5t.STR_NULLTERM)
        return dtype


    def __calculateChunk(self, nDims, dataType, storeOp, maxDims):
        if nDims == 1:
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
            raise NotImplementedException("Storage of " + str(nDims) + " dimensional " + \
                                   "data with mode " + storeOp + " not supported yet")
        chunk = tuple(chunk)
        return chunk

    def __writeProperties(self, dr, dataset):
        attrs = dr.getDataAttributes()
        if attrs:
            for key in attrs:
                dataset.attrs[key] = attrs[key]

    def __storeInterpolated(self, f, rec, op):
        if op != 'REPLACE' and op != 'STORE_ONLY':
            raise StorageException("Only replace and store modes are supported with interpolation enabled")

        # store the base product
        ss = self.__writeHDF(f, rec, op)

        sizes = rec.getSizes()
        newSzX = sizes[1]
        newSzY = sizes[0]
        originalName = rec.getName()
        originalGroup = rec.getGroup()
        level = 0

        # avoid recursive links by turning off downscaling
        rec.getProps().setDownscaled(False)
        from PIL import Image
        import time

        while newSzX > DOWNSCALE_THRESHOLD or newSzY > DOWNSCALE_THRESHOLD:
            data = rec.retrieveDataObject()
            # data is potentially 1-dimensional from serialization, ensure it goes to correct 2 dimensions
            data = data.reshape([newSzX, newSzY])
            newSzX = newSzX / 2
            newSzY = newSzY / 2
            level += 1
            rec.setName(str(level))
            rec.setGroup(originalGroup + '/' + originalName + '-interpolated')
            # satellite data comes in as signed bytes but pil requires unsigned bytes
            data = numpy.array(data + 127, dtype=numpy.uint8)
            image = Image.fromarray(data)
            image = image.resize((newSzY, newSzX))
            downsized = numpy.array(image)
            # transform back to signed bytes
            downsized = numpy.array(downsized - 127, dtype=numpy.int8)
            rec.putDataObject(downsized)
            rec.setSizes([newSzY, newSzX])
            self.__writeHDF(f, rec, op)

        return ss

    def __writePartialHDFDataset(self, f, data, dims, szDims, ds, props,
                                                minIndex):
        # reverse sizes for hdf5
        szDims1 = [None, ] * len(szDims)
        for i in range(len(szDims)):
            szDims1[i] = szDims[len(szDims) - i - 1]
        offset = [None, ] * len(minIndex)
        for i in range(len(minIndex)):
            offset[i] = minIndex[len(minIndex) - i - 1]

        # process chunking
#        chunkSize = None
#        if data.dtype != numpy._string and data.dtype != numpy._object:
#            chunkSize = DEFAULT_CHUNK_SIZE
#        else:
#            chunkSize = 1
#        chunk = [chunkSize] * len(szDims)
        data = data.reshape(szDims1)

        endIndex = [offset[0] + szDims1[0], offset[1] + szDims1[1]]
        ds[offset[0]:endIndex[0], offset[1]:endIndex[1]] = data
        return {'op':'REPLACE'}


    def delete(self, request):
        fn = request.getFilename()
        f, lock = self.__openFile(fn, 'w')
        resp = DeleteResponse()
        resp.setSuccess(True)
        deleteFile = False

        try:
            locs = request.getLocations()
            for dataset in locs:
                ds = self.__getNode(f, None, dataset)
                grp = ds.parent
                grp.id.unlink(ds.name)

        finally:
            # check if file has any remaining data sets
            # if no data sets, flag file for deletion
            deleteFile = False
            try:
                f.flush()
                deleteFile = not self.__hasDataSet(f)
            except Exception, e:
                logger.error('Error occurred checking for dataSets in file [' + str(fn) + ']: ' + IDataStore._exc())

            t0=time.time()
            f.close()
            t1=time.time()
            timeMap['closeFile']=t1-t0

            if deleteFile:
                try:
                    os.remove(fn)
                except Exception, e:
                    logger.error('Error occurred deleting file [' + str(fn) + ']: ' + IDataStore._exc())


            LockManager.releaseLock(lock)
        return resp

    # recursively looks for data sets
    def __hasDataSet(self, group):
        for key in group.keys():
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
                result = self.__retrieve(groupNode, request.getIncludeInterpolated())
            resp = RetrieveResponse()
            resp.setRecords(result)
            return resp
        finally:
            t0=time.time()
            f.close()
            t1=time.time()
            timeMap['closeFile']=t1-t0
            LockManager.releaseLock(lock)



    def __retrieve(self, group, includeInterpolated=False):
        records = []
        datasets = group.keys()
        for ds in datasets:
            interpDs = ds.endswith('-interpolated')
            if includeInterpolated and interpDs:
                subresults = self.__retrieve(group[ds], False)
                if subresults:
                    records += subresults
            elif not interpDs:
                rec = self.__retrieveInternal(group[ds], REQUEST_ALL)
                records.append(rec)

        return records

    def __retrieveInternal(self, ds, req):
        rawData = HDF5OpManager.read(ds, req)
        rec = DataStoreFactory.createStorageRecord(rawData, ds)
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
                rec = DataStoreFactory.createStorageRecord(rawData, ds)
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
                logger.debug("pid=" + str(os.getpid()) + " filename=" + fn + \
                        ", numberOfDatasets/Parameters=" + str(len(names)) + \
                        ", getLockTime=" +  ('%.3f' % (t1-t0)) + \
                        ", readDataTime=" + ('%.3f' % (timeSpentReading)) + \
                        ", releaseLockTime=" + ('%.3f' % (t5-t4)) + \
                        ", retrieveDatasetsTotal=" + ('%.3f' % (t4-t6)) + \
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
                    rec = DataStoreFactory.createStorageRecord(rawData, dsNode)
                    recs.append(rec)
            resp = RetrieveResponse()
            resp.setRecords(recs)
            return resp
        finally:
            t0=time.time()
            f.close()
            t1=time.time()
            timeMap['closeFile']=t1-t0
            LockManager.releaseLock(lock)

    def getDatasets(self, request):
        fn = request.getFilename()
        f, lock = self.__openFile(fn, 'r')
        try:
            grpName = request.getGroup()
            grp = self.__getNode(f['/'], grpName)
            ds = grp.keys()
            return ds
        finally:
            t0=time.time()
            f.close()
            t1=time.time()
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
            szDims1 = [None, ] * len(szDims)
            for i in range(len(szDims)):
                szDims1[i] = szDims[len(szDims) - i - 1]
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
            t0=time.time()
            f.close()
            t1=time.time()
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
        if chunks:
            plc.set_chunk(chunks)
        if compression == 'LZF':
            plc.set_shuffle()
            plc.set_filter(h5py.h5z.FILTER_LZF, h5py.h5z.FLAG_OPTIONAL)

        szDims = tuple(szDims)
        if maxDims is not None:
            maxDims = tuple(x if x is not None else h5py.h5s.UNLIMITED for x in maxDims)

        space_id = h5py.h5s.create_simple(szDims, maxDims)
        type_id = h5py.h5t.py_create(dtype, logical=True)

        id = h5py.h5d.create(group.id, datasetName, type_id, space_id, plc)
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
                        removeFile = False
                        for deleteDate in datesToDelete:
                            try:
                                matches = PURGE_REGEX.search(deleteDate)
                                pluginName=matches.group(1)
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

    def __removeDir(self, path):
        gotLock = False
        try:
            gotLock, lock = LockManager.getLock(path, 'a')
            if gotLock:
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
        t0=time.time()
        if not gotLock:
            raise StorageException('Unable to acquire lock on file ' + filename)
        try:
            if mode == 'w' and os.path.exists(filename):
                mode = 'a'
            f = h5py.File(filename, mode)
        except Exception, e:
            msg = "Unable to open file " + filename + ": " + IDataStore._exc()
            logger.error(msg)
            LockManager.releaseLock(fd)
            raise e

        t1=time.time()
        timeMap['openFile']=t1-t0

        return f, fd

    def __getNode(self, rootNode, groupName, dsName=None, create=False):
        t0=time.time()

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
        tokens = filter(None, tokens)

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
                group = None
                if groupName:
                    group = groupName
                    if dsName:
                        group += '/' + dsName
                elif dsName:
                    group = dsName

                # check old group structure
                node = self.__getGroup(rootNode, group)

        t1=time.time()
        if timeMap.has_key('getGroup'):
            timeMap['getGroup']+=t1-t0
        else:
            timeMap['getGroup']=t1-t0

        return node

    # deprecated, should only be called in transition period
    def __getGroup(self, rootNode, name):
        if name is None or len(name.strip()) == 0:
            # if no group is specific default to base group
            grp = rootNode
        else:
            try:
                group=name
                if group.startswith('/'):
                    group = group[1:]
                grp = rootNode[group]
            except:
                raise StorageException("No group " + name + " found")

        return grp

    def __link(self, group, linkName, dataset):
        # this is a hard link
        group[linkName] = dataset

        # untested code, this would be if we went with symbolic links, this is unpublished h5py API
        #id = group.id
        #id.link(dataset.name, linkName, h5py.h5g.LINK_SOFT)

    def copy(self, request):
        resp = FileActionResponse()
        file = request.getFilename()
        pth = os.path.split(file)[0]
        repack = request.getRepack()
        action = self.__doCopy if not repack else self.__doRepack
        self.__doFileAction(file, pth, request.getOutputDir(), action, resp, request.getRepackCompression(), request.getTimestampCheck())
        return resp

    def __doCopy(self, filepath, basePath, outputDir, compression):
        shutil.copy(filepath, outputDir)
        success = (os.path.isfile(os.path.join(outputDir, os.path.basename(filepath))))
        return success

    def repack(self, request):
        resp = FileActionResponse()
        pth = request.getFilename()
        files = self.__listHdf5Files(pth)
        compression = request.getCompression()
        for f in files:
            self.__doFileAction(f, pth, None, self.__doRepack, resp, compression)
        return resp

    def __listHdf5Files(self, pth):
        results = []
        for base, dirs, files in os.walk(pth):
            goodfiles = fnmatch.filter(files, '*.h5')
            results.extend(os.path.join(base, f) for f in goodfiles)
        return results

    def __doRepack(self, filepath, basePath, outDir, compression):
        t0=time.time()
        # call h5repack to repack the file
        if outDir is None:
            repackedFullPath = filepath + '.repacked'
        else:
            repackedFullPath = filepath.replace(basePath, outDir)
        cmd = ['h5repack', '-f', compression, filepath, repackedFullPath]
        ret = subprocess.call(cmd)

        success = (ret == 0)
        if success:
            # repack was successful, replace the old file if we did it in the
            # same directory, otherwise leave it alone
            if outDir is None:
                os.remove(filepath)
                os.rename(repackedFullPath, filepath)
                os.chmod(filepath, stat.S_IWUSR | stat.S_IWGRP | stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
        else:
            # remove failed new file if there was one
            if os.path.exists(repackedFullPath):
                os.remove(repackedFullPath)
            if outDir is not None:
                # repack failed, but they wanted the data in a different
                # directory, so just copy the original data without the repack
                shutil.copy(filepath, repackedFullPath)
        t1=time.time()
        if timeMap.has_key('repack'):
            timeMap['repack']+=t1-t0
        else:
            timeMap['repack']=t1-t0
        return success

    def __doFileAction(self, filepath, basePath, outputDir, fileAction, response, compression='NONE', timestampCheck=None):
        lock = None
        try:
            f, lock = self.__openFile(filepath, 'a')
            proceedWithRepack = True
            if timestampCheck:
                if timestampCheck in f.attrs.keys():
                    lastRepacked = f.attrs[timestampCheck]
                    lastModified = os.stat(filepath).st_mtime
                    if lastRepacked > lastModified:
                        proceedWithRepack = False
            if proceedWithRepack:
                # update repack time even if repack will fail, cause if it fails at
                # this time there's no point in retrying.  put time in the near future
                # cause the modified time will be after the repack and rename
                if timestampCheck:
                    f.attrs[timestampCheck] = time.time() + 30
                f.close()

                success = fileAction(filepath, basePath, outputDir, compression)

                # update response
                if success:
                    getter = response.getSuccessfulFiles
                    setter = response.setSuccessfulFiles
                else:
                    getter = response.getFailedFiles
                    setter = response.setFailedFiles
                responseList = getter()
                if responseList:
                    responseList += [filepath]
                else:
                    responseList = [filepath]
                setter(responseList)
        except Exception, e:
            logger.warn("Error repacking file " + filepath + ": " + str(e))
            failed = response.getFailedFiles()
            if failed:
                failed += [filepath]
            else:
                failed = [filepath]
            response.setFailedFiles(failed)
        finally:
            if lock:
                LockManager.releaseLock(lock)
