package com.raytheon.uf.common.pypies;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.datastorage.DuplicateRecordStorageException;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pypies.request.AbstractRequest;
import com.raytheon.uf.common.pypies.request.CopyRequest;
import com.raytheon.uf.common.pypies.request.CreateDatasetRequest;
import com.raytheon.uf.common.pypies.request.DatasetDataRequest;
import com.raytheon.uf.common.pypies.request.DatasetNamesRequest;
import com.raytheon.uf.common.pypies.request.DeleteFilesRequest;
import com.raytheon.uf.common.pypies.request.DeleteRequest;
import com.raytheon.uf.common.pypies.request.GroupsRequest;
import com.raytheon.uf.common.pypies.request.RepackRequest;
import com.raytheon.uf.common.pypies.request.RetrieveRequest;
import com.raytheon.uf.common.pypies.request.StoreRequest;
import com.raytheon.uf.common.pypies.response.ErrorResponse;
import com.raytheon.uf.common.pypies.response.FileActionResponse;
import com.raytheon.uf.common.pypies.response.RetrieveResponse;
import com.raytheon.uf.common.pypies.response.StoreResponse;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.util.FileUtil;

/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

/**
 * Data Store implementation that communicates with a PyPIES server over http.
 * The requests and responses are all DynamicSerialized.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 27, 2010            njensen     Initial creation
 * Oct 01, 2010            rjpeter     Added logging of requests over 300ms
 * Mon 07, 2013  DR 15294  D. Friedman Stream large requests
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PyPiesDataStore implements IDataStore {

    private static final long SIMPLE_LOG_TIME = 300;

    private static final long HUGE_REQUEST = 1024 * 1024 * 25;

    protected static String address = null;

    protected List<IDataRecord> records = new ArrayList<IDataRecord>();

    protected String filename;

    protected PypiesProperties props;

    public PyPiesDataStore(final File file, final boolean useLocking,
            final PypiesProperties props) {
        this.filename = FileUtil.edexPath(file.getPath()); // Win32
        this.props = props;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#addDataRecord(com.raytheon
     * .uf.common.datastorage.records.IDataRecord,
     * com.raytheon.uf.common.datastorage.StorageProperties)
     */
    @Override
    public void addDataRecord(final IDataRecord dataset,
            final StorageProperties properties) throws StorageException {
        if (dataset.validateDataSet()) {
            dataset.setProperties(properties);
            records.add(dataset);
        } else {
            throw new StorageException("Invalid dataset " + dataset.getName()
                    + " :" + dataset, null);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#addDataRecord(com.raytheon
     * .uf.common.datastorage.records.IDataRecord)
     */
    @Override
    public void addDataRecord(final IDataRecord dataset)
            throws StorageException {
        addDataRecord(dataset, dataset.getProperties());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#createLinks(java.util.Map)
     */
    @Override
    public void createLinks(final Map<String, LinkLocation> links)
            throws StorageException, FileNotFoundException {
        throw new UnsupportedOperationException(
                "pypies does not support this yet!");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#delete(java.lang.String[])
     */
    @Override
    public void delete(final String... location) throws StorageException,
            FileNotFoundException {
        DeleteRequest delete = new DeleteRequest();
        delete.setLocations(location);
        sendRequest(delete);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#getDatasets(java.lang.String
     * )
     */
    @Override
    public String[] getDatasets(final String group) throws StorageException,
            FileNotFoundException {
        DatasetNamesRequest req = new DatasetNamesRequest();
        req.setGroup(group);
        String[] result = (String[]) cachedRequest(req);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieve(java.lang.String)
     */
    @Override
    public IDataRecord[] retrieve(final String group) throws StorageException,
            FileNotFoundException {
        return retrieve(group, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieve(java.lang.String,
     * boolean)
     */
    @Override
    public IDataRecord[] retrieve(final String group,
            final boolean includeInterpolated) throws StorageException,
            FileNotFoundException {
        RetrieveRequest req = new RetrieveRequest();
        req.setGroup(group);
        req.setIncludeInterpolated(includeInterpolated);
        RetrieveResponse resp = (RetrieveResponse) cachedRequest(req);
        return resp.getRecords();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieve(java.lang.String,
     * java.lang.String, com.raytheon.uf.common.datastorage.Request)
     */
    @Override
    public IDataRecord retrieve(final String group, final String dataset,
            final Request request) throws StorageException,
            FileNotFoundException {
        RetrieveRequest req = new RetrieveRequest();
        req.setGroup(group);
        req.setDataset(dataset);
        req.setRequest(request);
        RetrieveResponse resp = (RetrieveResponse) cachedRequest(req);
        return resp.getRecords()[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieveDatasets(java.lang
     * .String[], com.raytheon.uf.common.datastorage.Request)
     */
    @Override
    public IDataRecord[] retrieveDatasets(final String[] datasetGroupPath,
            final Request request) throws StorageException,
            FileNotFoundException {
        DatasetDataRequest req = new DatasetDataRequest();
        req.setDatasetGroupPath(datasetGroupPath);
        req.setRequest(request);
        RetrieveResponse result = (RetrieveResponse) cachedRequest(req);
        return result.getRecords();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#retrieveGroups(java.lang
     * .String[], com.raytheon.uf.common.datastorage.Request)
     */
    @Override
    public IDataRecord[] retrieveGroups(final String[] groups,
            final Request request) throws StorageException,
            FileNotFoundException {
        GroupsRequest req = new GroupsRequest();
        req.setGroups(groups);
        req.setRequest(request);

        RetrieveResponse resp = (RetrieveResponse) cachedRequest(req);
        return resp.getRecords();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datastorage.IDataStore#store()
     */
    @Override
    public StorageStatus store() throws StorageException {
        return store(StoreOp.STORE_ONLY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#store(com.raytheon.uf.common
     * .datastorage.IDataStore.StoreOp)
     */
    @Override
    public StorageStatus store(final StoreOp storeOp) throws StorageException {
        StoreRequest req = new StoreRequest();
        req.setOp(storeOp);
        req.setRecords(records);

        boolean huge = false;
        long totalSize = 0;
        for (IDataRecord rec : records) {
            totalSize += rec.getSizeInBytes();
            if (totalSize >= HUGE_REQUEST) {
                huge = true;
                break;
            }
        }

        StorageStatus ss = null;
        try {
            StoreResponse sr = (StoreResponse) sendRequest(req, huge);
            ss = sr.getStatus();
            String[] exc = sr.getExceptions();
            IDataRecord[] failed = sr.getFailedRecords();

            // need to set the correlation object
            if (failed != null) {
                for (IDataRecord rec : failed) {
                    Iterator<IDataRecord> recordIter = records.iterator();
                    while (recordIter.hasNext()) {
                        IDataRecord oldRec = recordIter.next();
                        if (oldRec.getGroup().equals(rec.getGroup())
                                && oldRec.getName().equals(rec.getName())) {
                            rec.setCorrelationObject(oldRec
                                    .getCorrelationObject());
                            recordIter.remove();
                            break;
                        }
                    }
                }
            }

            records.clear();
            StorageException[] jexc = new StorageException[exc.length];
            for (int i = 0; i < exc.length; i++) {
                // checking for duplicates based on what is in the string...
                if (exc[i].contains("already exists")) {
                    jexc[i] = new DuplicateRecordStorageException(exc[i],
                            failed[i]);
                } else {
                    jexc[i] = new StorageException(exc[i], failed[i]);
                }
            }

            ss.setExceptions(jexc);
        } catch (StorageException e) {
            ss = new StorageStatus();
            ss.setOperationPerformed(storeOp);
            int size = records.size();
            StorageException[] jexc = new StorageException[size];
            for (int i = 0; i < size; i++) {
                jexc[i] = new StorageException(e.getMessage(), records.get(i),
                        e);
            }
            ss.setExceptions(jexc);
        }
        return ss;
    }

    protected Object sendRequest(final AbstractRequest obj) throws StorageException {
        return sendRequest(obj, false);
    }

    protected Object sendRequest(final AbstractRequest obj, boolean huge)
            throws StorageException {
        obj.setFilename(filename);

        initializeProperties();

        byte[] result = null;
        long t0 = System.currentTimeMillis();
        try {
            result = doSendRequest(obj, huge);
        } catch (Exception e) {
            throw new StorageException(
                    "Error communicating with pypies server", null, e);
        }
        long time = System.currentTimeMillis() - t0;

        if (time >= SIMPLE_LOG_TIME) {
            System.out.println("Took " + time + " ms to receive response for "
                    + obj.getClass().getSimpleName() + " on file "
                    + obj.getFilename());
        }

        Object ret = deserializeResponse(result);

        if (ret instanceof ErrorResponse) {
            throw new StorageException(((ErrorResponse) ret).getError(), null);
        }

        return ret;
    }

    protected byte[] doSendRequest(final AbstractRequest obj, boolean huge) throws Exception {
        if (huge) {
            return HttpClient.getInstance().postBinary(address, new HttpClient.OStreamHandler() {
                @Override
                public void writeToStream(OutputStream os) throws CommunicationException {
                    try {
                        DynamicSerializationManager.getManager(SerializationType.Thrift).serialize(obj, os);
                    } catch (SerializationException e) {
                        throw new CommunicationException(e);
                    }
                }
            });
        } else {
            byte[] bytes = serializeRequest(obj);
            return HttpClient.getInstance().postBinary(address, bytes);
        }
    }

    /**
     * By default this method simply passes the request to
     * sendRequest(AbstractRequest). Method exists to be overridden for
     * implementations that cache data responses..
     * 
     * @param obj
     * @return
     * @throws StorageException
     */
    protected Object cachedRequest(final AbstractRequest obj)
            throws StorageException {
        return this.sendRequest(obj);
    }

    protected byte[] serializeRequest(final AbstractRequest request)
            throws StorageException {
        try {
            return SerializationUtil.transformToThrift(request);
        } catch (SerializationException e) {
            throw new StorageException("Error serializing request", null, e);
        }
    }

    protected Object deserializeResponse(final byte[] response)
            throws StorageException {
        try {
            return SerializationUtil.transformFromThrift(response);
        } catch (SerializationException e) {
            throw new StorageException(
                    "Error deserializing response from pypies server", null, e);
        }
    }

    protected void initializeProperties() {
        if (address == null) {
            address = props.getAddress();
        }
    }

    @Override
    public void deleteFiles(final String[] datesToDelete)
            throws StorageException, FileNotFoundException {
        DeleteFilesRequest req = new DeleteFilesRequest();
        req.setDatesToDelete(datesToDelete);
        sendRequest(req);
    }

    @Override
    public void createDataset(final IDataRecord rec) throws StorageException,
            FileNotFoundException {
        CreateDatasetRequest req = new CreateDatasetRequest();
        req.setRecord(rec);
        sendRequest(req);
    }

    @Override
    public void repack(final Compression compression) throws StorageException {
        RepackRequest req = new RepackRequest();
        req.setFilename(this.filename);
        req.setCompression(compression);
        FileActionResponse resp = (FileActionResponse) sendRequest(req);
        // TODO do we really want to make this an exception?
        // reasoning is if the repack fails for some reason, the original file
        // is left as is, just isn't as efficiently packed
        if ((resp != null) && (resp.getFailedFiles() != null)
                && (resp.getFailedFiles().length > 0)) {
            StringBuilder sb = new StringBuilder();
            sb.append("Error repacking the following files: ");
            String[] failed = resp.getFailedFiles();
            for (int i = 0; i < failed.length; i++) {
                sb.append(failed[i]);
                if (i < failed.length - 1) {
                    sb.append(", ");
                }
            }
            throw new StorageException(sb.toString(), null);
        }
    }

    @Override
    public void copy(final String outputDir, final Compression compression,
            final String timestampCheck, final int minMillisSinceLastChange,
            final int maxMillisSinceLastChange) throws StorageException {
        CopyRequest req = new CopyRequest();
        req.setFilename(this.filename);
        if (compression != null) {
            req.setRepack(true);
            req.setRepackCompression(compression);
        } else {
            req.setRepack(false);
        }
        req.setOutputDir(outputDir);
        req.setTimestampCheck(timestampCheck);
        req.setMinMillisSinceLastChange(minMillisSinceLastChange);
        FileActionResponse resp = (FileActionResponse) sendRequest(req);

        if ((resp != null) && (resp.getFailedFiles() != null)
                && (resp.getFailedFiles().length > 0)) {
            StringBuilder sb = new StringBuilder();
            sb.append("Error copying the following files: ");
            String[] failed = resp.getFailedFiles();
            for (int i = 0; i < failed.length; i++) {
                sb.append(failed[i]);
                if (i < failed.length - 1) {
                    sb.append(", ");
                }
            }
            throw new StorageException(sb.toString(), null);
        }
    }
}
