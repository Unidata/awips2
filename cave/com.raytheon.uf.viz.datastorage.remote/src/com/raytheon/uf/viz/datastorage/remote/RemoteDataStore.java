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
package com.raytheon.uf.viz.datastorage.remote;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Map;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.remote.requests.ThriftDataRequest;
import com.raytheon.uf.common.datastorage.remote.requests.ThriftDataResponse;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Creates RetrieveDataRequests and sends them to the edex server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RemoteDataStore implements IDataStore {

    private File file;

    private boolean useLocking;

    public RemoteDataStore(File file, boolean useLocking) {
        this.file = file;
        this.useLocking = useLocking;
    }

    @Override
    public String[] getDatasets(String group) throws StorageException,
            FileNotFoundException {
        ThriftDataRequest request = new ThriftDataRequest();
        request.setFile(file.getAbsolutePath());
        request.setUseLocking(useLocking);
        request.setMethod("getDatasets");
        request.setParameterTypes(new String[] { String.class.getName() });
        request.setParameters(new Object[] { group });
        try {
            ThriftDataResponse response = (ThriftDataResponse) ThriftClient
                    .sendRequest(request);
            if (response.getDatasets() == null) {
                throw new VizException("response object was null");
            }
            return response.getDatasets();
        } catch (VizException e) {
            throw new StorageException("unable to retrieve data over http",
                    null, e);
        }
    }

    @Override
    public IDataRecord[] retrieve(String group) throws StorageException,
            FileNotFoundException {
        ThriftDataRequest request = new ThriftDataRequest();
        request.setFile(file.getAbsolutePath());
        request.setUseLocking(useLocking);
        request.setMethod("retrieve");
        request.setParameterTypes(new String[] { String.class.getName() });
        request.setParameters(new Object[] { group });
        try {
            ThriftDataResponse response = (ThriftDataResponse) ThriftClient
                    .sendRequest(request);
            if (response.getRecords() == null) {
                throw new VizException("response object was null");
            }
            return response.getRecords();
        } catch (VizException e) {
            throw new StorageException("unable to retrieve data over http",
                    null, e);
        }
    }

    @Override
    public IDataRecord[] retrieve(String group, boolean includeInterpolated)
            throws StorageException, FileNotFoundException {
        ThriftDataRequest request = new ThriftDataRequest();
        request.setFile(file.getAbsolutePath());
        request.setUseLocking(useLocking);
        request.setMethod("retrieve");
        request.setParameterTypes(new String[] { String.class.getName(),
                boolean.class.getName() });
        request.setParameters(new Object[] { group, includeInterpolated });
        try {
            ThriftDataResponse response = (ThriftDataResponse) ThriftClient
                    .sendRequest(request);
            if (response.getRecords() == null) {
                throw new VizException("response object was null");
            }
            return response.getRecords();
        } catch (VizException e) {
            throw new StorageException("unable to retrieve data over http",
                    null, e);
        }
    }

    @Override
    public IDataRecord retrieve(String group, String dataset, Request request)
            throws StorageException, FileNotFoundException {
        ThriftDataRequest req = new ThriftDataRequest();
        req.setFile(file.getAbsolutePath());
        req.setUseLocking(useLocking);
        req.setMethod("retrieve");
        req.setParameterTypes(new String[] { String.class.getName(),
                String.class.getName(), Request.class.getName() });
        req.setParameters(new Object[] { group, dataset, request });
        try {
            ThriftDataResponse response = (ThriftDataResponse) ThriftClient
                    .sendRequest(req);
            if (response.getRecord() == null) {
                throw new VizException("response object was null");
            }
            return response.getRecord();
        } catch (VizException e) {
            throw new StorageException("unable to retrieve data over http",
                    null, e);
        }
    }

    @Override
    public IDataRecord[] retrieveDatasets(String[] datasetGroupPath,
            Request request) throws StorageException, FileNotFoundException {
        ThriftDataRequest req = new ThriftDataRequest();
        req.setFile(file.getAbsolutePath());
        req.setUseLocking(useLocking);
        req.setMethod("retrieveDatasets");
        req.setParameterTypes(new String[] { String[].class.getName(),
                Request.class.getName() });
        req.setParameters(new Object[] { datasetGroupPath, request });
        try {
            ThriftDataResponse response = (ThriftDataResponse) ThriftClient
                    .sendRequest(req);
            if (response.getRecords() == null) {
                throw new VizException("response object was null");
            }
            return response.getRecords();
        } catch (VizException e) {
            throw new StorageException("unable to retrieve data over http",
                    null, e);
        }
    }

    @Override
    public IDataRecord[] retrieveGroups(String[] groups, Request request)
            throws StorageException, FileNotFoundException {
        ThriftDataRequest req = new ThriftDataRequest();
        req.setFile(file.getAbsolutePath());
        req.setUseLocking(useLocking);
        req.setMethod("retrieveGroups");
        req.setParameterTypes(new String[] { String[].class.getName(),
                Request.class.getName() });
        req.setParameters(new Object[] { groups, request });
        try {
            ThriftDataResponse response = (ThriftDataResponse) ThriftClient
                    .sendRequest(req);
            if (response.getRecords() == null) {
                throw new VizException("response object was null");
            }
            return response.getRecords();
        } catch (VizException e) {
            throw new StorageException("unable to retrieve data over http",
                    null, e);
        }
    }

    @Override
    public void addDataRecord(IDataRecord dataset, StorageProperties properties)
            throws StorageException {
        throw new StorageException("Operation not supported", dataset);
    }

    @Override
    public void addDataRecord(IDataRecord dataset) throws StorageException {
        throw new StorageException("Operation not supported", dataset);
    }

    @Override
    public void delete(String... location) throws StorageException,
            FileNotFoundException {
        throw new StorageException("Operation not supported", null);
    }

    @Override
    public StorageStatus store() throws StorageException {
        throw new StorageException("Operation not supported", null);
    }

    @Override
    public StorageStatus store(StoreOp storeOp) throws StorageException {
        throw new StorageException("Operation not supported", null);
    }

    @Override
    public void createLinks(Map<String, LinkLocation> links)
            throws StorageException, FileNotFoundException {
        throw new StorageException("Operation not supported", null);

    }

    @Override
    public void deleteFiles(String[] datesToDelete) throws StorageException,
            FileNotFoundException {
        throw new StorageException("Operation not supported", null);
    }

    @Override
    public void createDataset(IDataRecord rec) throws StorageException,
            FileNotFoundException {
        throw new StorageException("Operation not supported", null);
    }

    @Override
    public void repack(Compression compression) throws StorageException {
        throw new StorageException("Operation not supported", null);
    }

    @Override
    public void copy(String outputDir, Compression compression,
            String timestampCheck, int minMillisSinceLastChange,
            int maxMillisSinceLastChange) throws StorageException {
        throw new StorageException("Operation not supported", null);
    }

}
