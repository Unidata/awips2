package com.raytheon.uf.common.pypies;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.comm.HttpClient;
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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PyPiesDataStore implements IDataStore {

    protected static String address = null;

    protected List<IDataRecord> records = new ArrayList<IDataRecord>();

    protected String filename;

    protected PypiesProperties props;

    public PyPiesDataStore(File file, boolean useLocking, PypiesProperties props) {
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
    public void addDataRecord(IDataRecord dataset, StorageProperties properties)
            throws StorageException {
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
    public void addDataRecord(IDataRecord dataset) throws StorageException {
        addDataRecord(dataset, dataset.getProperties());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStore#createLinks(java.util.Map)
     */
    @Override
    public void createLinks(Map<String, LinkLocation> links)
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
    public void delete(String... location) throws StorageException,
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
    public String[] getDatasets(String group) throws StorageException,
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
    public IDataRecord[] retrieve(String group) throws StorageException,
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
    public IDataRecord[] retrieve(String group, boolean includeInterpolated)
            throws StorageException, FileNotFoundException {
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
    public IDataRecord retrieve(String group, String dataset, Request request)
            throws StorageException, FileNotFoundException {
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
    public IDataRecord[] retrieveDatasets(String[] datasetGroupPath,
            Request request) throws StorageException, FileNotFoundException {
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
    public IDataRecord[] retrieveGroups(String[] groups, Request request)
            throws StorageException, FileNotFoundException {
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
    public StorageStatus store(StoreOp storeOp) throws StorageException {
        StoreRequest req = new StoreRequest();
        req.setOp(storeOp);
        req.setRecords(records);

        StorageStatus ss = null;
        try {
            StoreResponse sr = (StoreResponse) sendRequest(req);
            records.clear();
            ss = sr.getStatus();
            String[] exc = sr.getExceptions();
            IDataRecord[] failed = sr.getFailedRecords();
            StorageException[] jexc = new StorageException[exc.length];
            for (int i = 0; i < exc.length; i++) {
                jexc[i] = new StorageException(exc[i], failed[i]);
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

    protected Object sendRequest(AbstractRequest obj) throws StorageException {
        obj.setFilename(filename);
        byte[] bytes = serializeRequest(obj);

        initializeProperties();

        byte[] result = null;
        try {
            result = HttpClient.getInstance().postBinary(address, bytes);
        } catch (Exception e) {
            throw new StorageException(
                    "Error communicating with pypies server", null, e);
        }

        Object ret = deserializeResponse(result);

        if (ret instanceof ErrorResponse) {
            throw new StorageException(((ErrorResponse) ret).getError(), null);
        }

        return ret;
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
    protected Object cachedRequest(AbstractRequest obj) throws StorageException {
        return this.sendRequest(obj);
    }

    protected byte[] serializeRequest(AbstractRequest request)
            throws StorageException {
        try {
            return SerializationUtil.transformToThrift(request);
        } catch (SerializationException e) {
            throw new StorageException("Error serializing request", null, e);
        }
    }

    protected Object deserializeResponse(byte[] response)
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
            int maxConnections = props.getMaxConnections();
            HttpClient.getInstance().setMaxConnectionsPerHost(maxConnections);
            int socketTimeout = props.getSocketTimeout();
            HttpClient.getInstance().setSocketTimeout(socketTimeout);
        }
    }

    @Override
    public void deleteFiles(String[] datesToDelete) throws StorageException,
            FileNotFoundException {
        DeleteFilesRequest req = new DeleteFilesRequest();
        req.setDatesToDelete(datesToDelete);
        sendRequest(req);
    }

    @Override
    public void createDataset(IDataRecord rec) throws StorageException,
            FileNotFoundException {
        CreateDatasetRequest req = new CreateDatasetRequest();
        req.setRecord(rec);
        sendRequest(req);
    }

    @Override
    public void repack(Compression compression) throws StorageException {
        RepackRequest req = new RepackRequest();
        req.setFilename(this.filename);
        req.setCompression(compression);
        FileActionResponse resp = (FileActionResponse) sendRequest(req);
        // TODO do we really want to make this an exception?
        // reasoning is if the repack fails for some reason, the original file
        // is left as is, just isn't as efficiently packed
        if (resp != null && resp.getFailedFiles() != null
                && resp.getFailedFiles().length > 0) {
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
    public void copy(String outputDir, Compression compression,
            String timestampCheck, int minMillisSinceLastChange,
            int maxMillisSinceLastChange) throws StorageException {
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

        if (resp != null && resp.getFailedFiles() != null
                && resp.getFailedFiles().length > 0) {
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
