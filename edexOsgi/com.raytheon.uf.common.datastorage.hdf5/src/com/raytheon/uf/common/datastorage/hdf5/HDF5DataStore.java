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

package com.raytheon.uf.common.datastorage.hdf5;

import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferFloat;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferShort;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import javax.media.jai.BorderExtender;
import javax.media.jai.Interpolation;
import javax.media.jai.JAI;
import javax.media.jai.ParameterBlockJAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.media.jai.TiledImage;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.hdf5lib.HDFNativeData;
import ncsa.hdf.hdf5lib.exceptions.HDF5Exception;
import ncsa.hdf.hdf5lib.exceptions.HDF5LibraryException;
import ncsa.hdf.hdf5lib.exceptions.HDF5SymbolTableException;

import com.raytheon.uf.common.datastorage.DuplicateRecordStorageException;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.locking.ClusteredLockManager;
import com.raytheon.uf.common.datastorage.locking.LockException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Interfaces to the HDF5 data store
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *       
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 9, 2007              chammack    Initial Creation.
 * 20070914            379  jkorman     Numerous changes to refactor DataType and
 *                                      to add support of a long data type.
 * Sep 25, 2007             chammack    Added replace record functionality
 * Apr 01, 2008 1041        chammack    Added delete functionality
 * Jun 30, 2008     2538    jsanchez    Update readProperties for Strings.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class HDF5DataStore implements IDataStore {

    protected static final int DOWNSCALE_THRESHOLD = 512;

    protected static final int RETRIEVAL_RETRIES = 10;

    public static final int DEFAULT_CHUNK_SIZE = 256;

    private static final Pattern PURGE_PATTERN = Pattern
            .compile("-[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{2}-");

    private static final H5Filter HDF5_FILTER = new H5Filter();

    protected ClusteredLockManager lockManager;

    protected File file;

    protected Map<IDataRecord, StorageProperties> records;

    protected boolean useLocking;

    /**
     * Connect to an HDF5 Datastore instance
     * 
     * Do not call directly, use DataStoreFactory.getDataStore();
     * 
     * @param file
     */
    public HDF5DataStore(File file, boolean useLocking) {
        records = new LinkedHashMap<IDataRecord, StorageProperties>();
        this.file = file;
        this.useLocking = useLocking;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.storage.IDataStore#addDataRecord(com.raytheon.edex.
     * storage.records.IDataRecord, com.raytheon.edex.storage.StorageProperties)
     */
    @Override
    public void addDataRecord(IDataRecord record, StorageProperties properties)
            throws StorageException {

        if (record.validateDataSet()) {
            records.put(record, properties);
        } else {
            throw new StorageException("Invalid dataset " + record.getName()
                    + " :" + record, null);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.storage.IDataStore#addDataRecord(com.raytheon.edex.
     * storage.records.IDataRecord)
     */
    @Override
    public void addDataRecord(IDataRecord record) throws StorageException {

        addDataRecord(record, record.getProperties());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.IDataStore#store()
     */
    @Override
    public StorageStatus store() throws StorageException {
        return store(StoreOp.STORE_ONLY);
    }

    protected void setVerboseErrorMessages() {
        H5.H5error_off();
    }

    protected int openFile(boolean forWrite) throws HDF5LibraryException,
            StorageException, FileNotFoundException {

        int file_id;
        // Setup the HDF5 file - Create if it doesn't exist (0 length file
        // indicates it was created for the purpose of the lock)

        H5.H5open();
        setVerboseErrorMessages();
        if (file.length() == 0) {
            file_id = H5.H5Fcreate(file.toString(),
                    HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);

        } else {
            file_id = H5.H5Fopen(file.toString(), HDF5Constants.H5F_ACC_RDWR,
                    HDF5Constants.H5P_DEFAULT);

        }

        return file_id;
    }

    protected void closeFileAndCleanUp(int file_id) throws StorageException {
        try {
            if (file_id > 0) {
                H5.H5Fflush(file_id, HDF5Constants.H5F_SCOPE_LOCAL);
                H5.H5Fclose(file_id);
            }
        } catch (HDF5LibraryException e) {
            // ignore
        }

        try {
            H5.H5close();
        } catch (HDF5LibraryException e) {
            // ignore
        }

        unlockFile();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.storage.IDataStore#store(com.raytheon.edex.storage.
     * IDataStore.StoreOp)
     */
    @Override
    public StorageStatus store(StoreOp storeOp) throws StorageException {

        List<StorageException> exceptions = new ArrayList<StorageException>();

        // To avoid holding file locks for long periods of time,
        // sort out the interpolated datasets and store them one
        // transaction at a time.

        // Store all non-interpolated datasets in one large transaction.

        Map<IDataRecord, StorageProperties> interpolatedRecords = new LinkedHashMap<IDataRecord, StorageProperties>();
        Map<IDataRecord, StorageProperties> nonInterpolatedRecords = new LinkedHashMap<IDataRecord, StorageProperties>();

        for (Entry<IDataRecord, StorageProperties> entry : this.records
                .entrySet()) {
            IDataRecord rec = entry.getKey();
            StorageProperties props = entry.getValue();

            if (props != null && props.isDownscaled()) {
                interpolatedRecords.put(rec, props);
            } else {
                nonInterpolatedRecords.put(rec, props);
            }
        }

        records.clear();

        StorageStatus ss = null;

        if (nonInterpolatedRecords.size() > 0) {
            ss = storeProducts(storeOp, nonInterpolatedRecords);
            if (ss.getExceptions() != null) {
                exceptions.addAll(Arrays.asList(ss.getExceptions()));
            }
        }

        if (interpolatedRecords.size() > 0) {
            for (Entry<IDataRecord, StorageProperties> entry : interpolatedRecords
                    .entrySet()) {
                IDataRecord record = entry.getKey();
                StorageProperties props = entry.getValue();

                ss = storeInterpolatedProduct(storeOp, record, props);
                if (ss.getExceptions() != null) {
                    exceptions.addAll(Arrays.asList(ss.getExceptions()));
                }
            }
        }

        if (ss == null) {
            ss = new StorageStatus();
        }

        ss.setExceptions(exceptions.toArray(new StorageException[exceptions
                .size()]));

        return ss;
    }

    private StorageStatus storeInterpolatedProduct(StoreOp storeOp,
            IDataRecord record, StorageProperties properties)
            throws StorageException {

        List<StorageException> exceptions = new ArrayList<StorageException>();

        StorageStatus status = new StorageStatus();
        if (storeOp != StoreOp.REPLACE && storeOp != StoreOp.STORE_ONLY) {
            throw new StorageException(
                    "Only replace and store modes are supported with interpolation enabled",
                    record);
        }

        // Store the base product
        Map<IDataRecord, StorageProperties> baseProduct = new HashMap<IDataRecord, StorageProperties>();
        baseProduct.put(record, properties);
        status = storeProducts(storeOp, baseProduct);
        if (status.getExceptions() != null) {
            exceptions.addAll(Arrays.asList(status.getExceptions()));
        }

        // Store the interpolated products
        int newSzX = (int) record.getSizes()[0];
        int newSzY = (int) record.getSizes()[1];
        String originalName = record.getName();
        String originalGroup = record.getGroup();
        int level = 0;

        // avoid recursive links by turning off downscaling (note the
        // clone to not corrupt the caller's copy)
        properties = properties.clone();
        properties.setDownscaled(false);

        while ((newSzX > DOWNSCALE_THRESHOLD || newSzY > DOWNSCALE_THRESHOLD)) {
            level++;
            resize(record, originalName, originalGroup, 0.5f, level);
            newSzX /= 2;
            newSzY /= 2;

            synchronized (HDF5DataStore.class) {
                try {
                    lockFile(true);
                } catch (FileNotFoundException e1) {
                    throw new StorageException("Unable to create lock file",
                            null, e1);
                }

                setVerboseErrorMessages();
                int file_id = 0;
                try {
                    try {
                        file_id = openFile(true);
                    } catch (Exception e) {
                        throw new StorageException("Error opening file: "
                                + this.file + ":: ", null, e);
                    }

                    try {
                        this.writeHDF(file_id, properties, record, storeOp,
                                status);
                    } catch (StorageException e) {
                        exceptions.add(e);
                    }
                    if (status.getExceptions() != null) {
                        exceptions
                                .addAll(Arrays.asList(status.getExceptions()));
                    }

                } finally {
                    closeFileAndCleanUp(file_id);
                }
            }
        }

        status.setExceptions(exceptions.toArray(new StorageException[exceptions
                .size()]));
        return status;

    }

    private StorageStatus storeProducts(StoreOp storeOp,
            Map<IDataRecord, StorageProperties> recordsToStore)
            throws StorageException {
        StorageStatus status = new StorageStatus();

        synchronized (HDF5DataStore.class) {
            try {
                lockFile(true);
            } catch (FileNotFoundException e1) {
                throw new StorageException("Unable to create lock file", null,
                        e1);
            }

            int file_id = 0;
            try {
                try {
                    file_id = openFile(true);
                } catch (Exception e) {
                    throw new StorageException("Error opening file: "
                            + this.file + ":: ", null, e);
                }

                Iterator<IDataRecord> recordIterator = recordsToStore.keySet()
                        .iterator();
                List<StorageException> exceptions = new ArrayList<StorageException>();

                while (recordIterator.hasNext()) {
                    IDataRecord record = recordIterator.next();
                    try {
                        this.writeHDF(file_id, recordsToStore.get(record),
                                record, storeOp, status);
                    } catch (StorageException e) {
                        exceptions.add(e);
                    }
                    if (status.getExceptions() != null) {
                        exceptions
                                .addAll(Arrays.asList(status.getExceptions()));
                    }
                }

                status.setExceptions(exceptions
                        .toArray(new StorageException[exceptions.size()]));
                return status;
            } finally {
                closeFileAndCleanUp(file_id);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.IDataStore#retrieve(java.lang.String)
     */
    @Override
    public IDataRecord[] retrieve(String group) throws StorageException,
            FileNotFoundException {
        return this.retrieve(group, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.IDataStore#retrieve(java.lang.String,
     * boolean)
     */
    @Override
    public IDataRecord[] retrieve(String group, boolean includeInterpolated)
            throws StorageException, FileNotFoundException {

        List<IDataRecord> records = new ArrayList<IDataRecord>();

        String[] datasets = getDatasets(group);
        for (String ds : datasets) {
            if (includeInterpolated && ds.endsWith("-interpolated")) {
                IDataRecord[] subresults;
                subresults = this.retrieve(group + "/" + ds, false);

                if (subresults != null && subresults.length > 0) {
                    for (IDataRecord result : subresults) {
                        records.add(result);
                    }
                }
            } else if (!ds.endsWith("-interpolated")) {
                IDataRecord record = this.retrieve(group, ds, Request.ALL);
                records.add(record);
            }

        }

        return records.toArray(new IDataRecord[records.size()]);

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
        StorageException se = null;
        for (int retry = 0; retry < RETRIEVAL_RETRIES; retry++) {
            synchronized (HDF5DataStore.class) {

                int file_id = -1;
                int plist = -1;
                lockFile(false);
                try {
                    try {
                        H5.H5open();
                    } catch (HDF5LibraryException e2) {
                        throw new StorageException("Couldn't initialize hdf5",
                                null, e2);
                    }
                    setVerboseErrorMessages();

                    plist = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);

                    int[] nElements = new int[1];
                    int[] rElements = new int[1];
                    int[] rBytes = new int[1];
                    double[] w0 = new double[1];

                    H5.H5Pget_cache(plist, nElements, rElements, rBytes, w0);
                    rBytes[0] = 0;
                    H5.H5Pset_cache(plist, nElements[0], rElements[0],
                            rBytes[0], w0[0]);

                    // Open an existing file
                    file_id = H5.H5Fopen(file.toString(),
                            HDF5Constants.H5F_ACC_RDONLY, plist);

                    return HDF5OpManager.read(request, file_id, group, dataset,
                            1.0f);

                } catch (HDF5Exception e) {
                    try {
                        H5.H5Eclear();
                    } catch (HDF5LibraryException e1) {
                        // ignore
                    }
                    se = new StorageException(
                            "Error occurred during retrieve ", null, e);
                } catch (Exception e) {
                    throw new StorageException(
                            "Error occurred during retrieve from file: "
                                    + file.getAbsolutePath() + " group: "
                                    + group + " dataset: " + dataset
                                    + " request: " + request, null, e);
                } finally {
                    try {
                        if (plist >= 0) {
                            H5.H5Pclose(plist);
                        }
                    } catch (HDF5LibraryException e) {
                        // ignore
                    }

                    try {
                        if (file_id >= 0) {
                            H5.H5Fclose(file_id);
                        }
                    } catch (HDF5LibraryException e) {
                        // ignore
                    }

                    try {
                        H5.H5Eclear();
                        H5.H5close();
                    } catch (HDF5LibraryException e) {
                        // ignore
                    }

                    unlockFile();
                }

            }
        }
        if (se != null) {
            throw se;
        } else {
            throw new StorageException(
                    "Read failed, unknown state after multiple retries", null);
        }
    }

    @Override
    public IDataRecord[] retrieveDatasets(String[] datasetGroupPath,
            Request request) throws StorageException, FileNotFoundException {
        StorageException se = null;
        for (int retry = 0; retry < RETRIEVAL_RETRIES; retry++) {
            synchronized (HDF5DataStore.class) {
                int file_id = -1;
                int plist = -1;
                lockFile(false);
                try {
                    setVerboseErrorMessages();

                    try {
                        H5.H5open();
                    } catch (HDF5LibraryException e2) {
                        throw new StorageException("Couldn't initialize hdf5",
                                null, e2);
                    }

                    plist = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);

                    int[] nElements = new int[1];
                    int[] rElements = new int[1];
                    int[] rBytes = new int[1];
                    double[] w0 = new double[1];

                    H5.H5Pget_cache(plist, nElements, rElements, rBytes, w0);
                    rElements[0] = 0; // 2048 * 1024;
                    rBytes[0] = 0; // 1024 * 1024 * 16;

                    H5.H5Pset_cache(plist, nElements[0], rElements[0],
                            rBytes[0], w0[0]);

                    // Open an existing file
                    file_id = H5.H5Fopen(file.toString(),
                            HDF5Constants.H5F_ACC_RDONLY, plist);
                    List<IDataRecord> recList = new ArrayList<IDataRecord>();
                    for (String d : datasetGroupPath) {
                        IDataRecord retVal = HDF5OpManager.read(request,
                                file_id, "", d, 1.0f);
                        if (retVal != null) {
                            recList.add(retVal);
                        }
                    }
                    return recList.toArray(new IDataRecord[recList.size()]);
                } catch (HDF5Exception e) {
                    try {
                        H5.H5Eclear();
                    } catch (HDF5LibraryException e1) {
                        // ignore
                    }
                    se = new StorageException(
                            "Error occurred during retrieve ", null, e);
                } catch (Exception e) {
                    throw new StorageException(
                            "Error occurred during retrieve ", null, e);
                } finally {
                    try {
                        if (plist >= 0) {
                            H5.H5Pclose(plist);
                        }
                    } catch (HDF5LibraryException e) {
                        // ignore
                    }

                    try {
                        if (file_id >= 0) {
                            H5.H5Fclose(file_id);
                        }
                    } catch (HDF5LibraryException e) {
                        // ignore
                    }

                    try {
                        H5.H5close();
                    } catch (HDF5LibraryException e) {
                        // ignore
                    }

                    unlockFile();
                }

            }
        }
        if (se != null) {
            throw se;
        } else {
            throw new StorageException(
                    "Read failed, unknown state after multiple retries", null);
        }
    }

    private IDataRecord[] retrieveGroupsInternal(String[] groups,
            Request request, float scaleFactor) throws StorageException,
            FileNotFoundException {
        synchronized (HDF5DataStore.class) {
            int file_id = -1;
            int plist = -1;

            lockFile(false);
            try {

                try {
                    H5.H5open();
                } catch (HDF5LibraryException e2) {
                    throw new StorageException("Couldn't initialize hdf5",
                            null, e2);
                }

                setVerboseErrorMessages();

                plist = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);

                int[] nElements = new int[1];
                int[] rElements = new int[1];
                int[] rBytes = new int[1];
                double[] w0 = new double[1];

                H5.H5Pget_cache(plist, nElements, rElements, rBytes, w0);
                rBytes[0] = 0;
                H5.H5Pset_cache(plist, nElements[0], rElements[0], rBytes[0],
                        w0[0]);

                // Open an existing file
                file_id = H5.H5Fopen(file.toString(),
                        HDF5Constants.H5F_ACC_RDONLY, plist);

                List<IDataRecord> records = new ArrayList<IDataRecord>();
                for (String group : groups) {
                    // Get the datasets
                    String[] datasets = getDatasetsFromFile(group, file_id);

                    for (String dataset : datasets) {
                        IDataRecord rec = HDF5OpManager.read(request, file_id,
                                group, dataset, scaleFactor);
                        records.add(rec);
                    }

                }
                return records.toArray(new IDataRecord[records.size()]);

            } catch (Exception e) {
                try {
                    H5.H5Eclear();
                } catch (HDF5LibraryException e1) {
                    // ignore
                }
                throw new StorageException("Error occurred during retrieve ",
                        null, e);
            } finally {
                try {
                    if (plist >= 0) {
                        H5.H5Pclose(plist);
                    }
                } catch (HDF5LibraryException e) {
                    // ignore
                }

                try {
                    if (file_id >= 0) {
                        H5.H5Fclose(file_id);
                    }
                } catch (HDF5LibraryException e) {
                    // ignore
                }

                try {
                    H5.H5close();
                } catch (HDF5LibraryException e) {
                    // ignore
                }

                unlockFile();
            }

        }
    }

    @Override
    public String[] getDatasets(String group) throws StorageException,
            FileNotFoundException {
        String[] retVal = null;

        synchronized (HDF5DataStore.class) {
            lockFile(false);

            setVerboseErrorMessages();
            int file_id = -1;
            // int group_id = -1;

            try {
                H5.H5open();
            } catch (HDF5LibraryException e2) {
                throw new StorageException("Couldn't initialize hdf5", null, e2);
            }

            try {
                file_id = H5
                        .H5Fopen(file.toString(), HDF5Constants.H5F_ACC_RDONLY,
                                HDF5Constants.H5P_DEFAULT);

                retVal = getDatasetsFromFile(group, file_id);
            } catch (HDF5LibraryException e) {
                try {
                    H5.H5Eclear();
                } catch (HDF5LibraryException e1) {
                    // ignore
                }

                throw new StorageException("Unable to catalog hdf5 file", null,
                        e);
            } finally {

                if (file_id >= 0) {
                    try {
                        H5.H5Fclose(file_id);
                    } catch (HDF5LibraryException e) {
                        // ignore
                    }
                }

                try {
                    H5.H5close();
                } catch (HDF5LibraryException e) {
                    // ignore
                }
                unlockFile();

            }
        }

        return retVal;

    }

    private String[] getDatasetsFromFile(String group, int file_id)
            throws StorageException {
        String[] retVal;
        int group_id = -1;
        try {
            group_id = H5.H5Gopen(file_id, group);

            long[] numObjs = new long[1];
            H5.H5Gget_num_objs(group_id, numObjs);

            retVal = new String[(int) numObjs[0]];

            for (int i = 0; i < retVal.length; i++) {
                String[] tmp = new String[1];
                H5.H5Gget_objname_by_idx(group_id, i, tmp, 255);
                retVal[i] = tmp[0];
            }

            return retVal;
        } catch (HDF5LibraryException e) {
            throw new StorageException("Unable to catalog hdf5 file", null, e);
        } finally {

            if (group_id >= 0) {
                try {
                    H5.H5Gclose(group_id);
                } catch (HDF5LibraryException e) {
                    // Ignore
                }
            }
        }
    }

    /**
     * Write to the HDF5 file
     * 
     * @param file_id
     *            file id
     * @param record
     *            the record to write
     * @param properties
     *            the storage properties
     * @param storeOp
     *            the store operation
     * @param status
     *            the storage status
     * @throws StorageException
     */
    private void writeHDF(int file_id, StorageProperties properties,
            IDataRecord record, StoreOp storeOp, StorageStatus status)
            throws StorageException {

        LocalHDFDataType dataType = null;
        Object data = null;

        if (properties != null
                && !properties.isChunked()
                && (properties.getCompression() != null && properties
                        .getCompression() != Compression.NONE)) {
            throw new StorageException("Data must be chunked to be compressed",
                    record);
        }

        data = record.getDataObject();
        dataType = LocalHDFDataType.getCorrespondingEnum(record);

        this.createGroup(file_id, record.getGroup(), record);

        if (record.getMinIndex() != null && record.getMinIndex().length > 0) {
            this.writePartialHDFDataset(file_id, data, record.getDimension(),
                    record.getSizes(), record.getName(), record.getGroup(),
                    properties, dataType, record.getMinIndex(), record);

        } else {
            // Store the base product
            this.writeHDFDataset(file_id, data, record.getDimension(),
                    record.getSizes(), record.getName(), record.getGroup(),
                    properties, dataType, storeOp, record, status);

            // Create the symlink if interpolation is set
            if (properties != null && properties.isDownscaled() == true) {

                this.createGroup(file_id,
                        record.getGroup() + "/" + record.getName()
                                + "-interpolated", record);
                this.createSymlink(file_id,
                        record.getGroup() + "/" + record.getName(),
                        record.getGroup() + "/" + record.getName()
                                + "-interpolated/0", record);
            }

        }

    }

    /**
     * Create a group inside the HDF5
     * 
     * @param file_id
     *            the file id
     * @param dataset
     *            the dataset to create
     * @param whether
     *            to replace existing records
     * @param rec
     *            the correlation record
     */
    protected int createGroup(int file_id, String dataset, IDataRecord rec)
            throws StorageException {
        int grp = 0;
        String[] parts = dataset.split("/");
        StringBuffer path = new StringBuffer();
        for (int i = 0; i < parts.length; i++) {
            String p = parts[i];
            path.append("/" + p);
            String curPath = path.toString();

            try {
                setVerboseErrorMessages();
                grp = H5.H5Gcreate(file_id, curPath, 0);

                H5.H5Gclose(grp);
            } catch (HDF5LibraryException e) {
                try {
                    H5.H5Eclear();
                } catch (HDF5LibraryException e1) {
                    // ignore
                }
            }

        }

        return grp;
    }

    protected void unlockFile() throws StorageException {
        if (useLocking) {
            try {
                lockManager.releaseLock(file);
            } catch (LockException e) {
                throw new StorageException("Lock Manager unlock failed", null,
                        e);
            }
        }
    }

    protected void lockFile(boolean writeLock) throws StorageException,
            FileNotFoundException {
        if (useLocking) {
            boolean gotLock = false;
            try {
                if (lockManager == null) {
                    lockManager = ClusteredLockManager.getInstance();
                }

                int retries = 0;
                while (gotLock == false && retries < 6000) {
                    if (retries > 0) {
                        Thread.sleep(10);
                    }
                    gotLock = lockManager.getLock(file, writeLock);
                    retries++;
                }
            } catch (LockException e1) {
                throw new StorageException("Unable to get lock", null, e1);
            } catch (InterruptedException e1) {
                throw new StorageException("Thread interrupted", null, e1);
            }

            if (gotLock == false) {
                throw new StorageException(
                        "Unable to get lock on file after 60 seconds", null);
            }
        }
    }

    /**
     * Create a symbolic link between two datasets
     * 
     * @param originalData
     *            the link target
     * @param newData
     *            the new symbolic link
     * @param the
     *            record
     */
    private void createSymlink(int file_id, String originalData,
            String newData, IDataRecord rec) throws StorageException {

        try {
            setVerboseErrorMessages();
            H5.H5Glink(file_id, HDF5Constants.H5G_LINK_SOFT, originalData,
                    newData);

        } catch (HDF5LibraryException e) {
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e1) {
                // ignore
            }

            throw new StorageException("Error creating symbolic link", rec, e);
        }
    }

    /**
     * Write the partial HDF5 dataset
     * 
     * @param file_id
     * @param mydata
     * @param dims
     * @param szDims
     * @param dataset
     * @param group
     * @param properties
     * @param dataType
     * @param minIndex
     * @param rec
     * @throws StorageException
     */
    private void writePartialHDFDataset(int file_id, Object mydata, int dims,
            long[] szDims, String dataset, String group,
            StorageProperties properties, LocalHDFDataType dataType,
            long[] minIndex, IDataRecord rec) throws StorageException {
        int dset = 0;
        int plist = 0;
        int gid = 0;

        try {
            dset = H5.H5Dopen(file_id, group + "/" + dataset);

            // Reverse the dimensions for hdf5
            long[] szDims1 = new long[szDims.length];
            for (int i = 0; i < szDims.length; i++) {
                szDims1[i] = szDims[szDims.length - i - 1];
            }

            long[] offset = new long[minIndex.length];
            for (int i = 0; i < minIndex.length; i++) {
                offset[i] = minIndex[minIndex.length - i - 1];
            }
            int dataspace = H5.H5Dget_space(dset);
            int sz = H5.H5Sget_simple_extent_ndims(dataspace);
            int memspace = H5.H5Screate_simple(sz,
                    HDFNativeData.longToByte(0, szDims1.length, szDims1), null);

            plist = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);

            if ((dataType != LocalHDFDataType.STRING)) {
                long[] chunk = new long[szDims.length];
                for (int i = 0; i < szDims.length; i++) {
                    chunk[i] = DEFAULT_CHUNK_SIZE;
                }

                H5.H5Pset_chunk(plist, szDims.length,
                        HDFNativeData.longToByte(0, chunk.length, chunk));

                processCompressionFlags(properties, plist);

            } else {
                long[] chunk = new long[szDims.length];
                for (int i = 0; i < szDims.length; i++) {
                    chunk[i] = 1;
                }
                H5.H5Pset_chunk(plist, szDims.length,
                        HDFNativeData.longToByte(0, chunk.length, chunk));

            }

            int nativeType = dataType.getHDFNativeType();

            H5.H5Sselect_hyperslab(dataspace, HDF5Constants.H5S_SELECT_SET,
                    HDFNativeData.longToByte(0, offset.length, offset), null,
                    HDFNativeData.longToByte(0, szDims1.length, szDims1), null);

            H5.H5Dwrite(dset, nativeType, memspace, dataspace,
                    HDF5Constants.H5P_DEFAULT, mydata);

        } catch (HDF5LibraryException e) {
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e1) {
                // ignore
            }
            throw new StorageException("Error occurred on storage", rec, e);
        } catch (HDF5Exception e) {
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e2) {
                // ignore
            }
            throw new StorageException("Error occurred on storage", rec, e);
        } finally {

            try {
                if (plist > 0) {
                    H5.H5Pclose(plist);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }

            try {

                if (dset > 0) {
                    H5.H5Dclose(dset);
                }
            } catch (HDF5LibraryException e) {
                // Ignore
            }

            try {
                if (gid > 0) {
                    H5.H5Gclose(gid);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }
        }

    }

    private void writeProperties(IDataRecord dr, int dataSet)
            throws StorageException {
        if (dr.getDataAttributes() == null) {
            return;
        }

        for (String key : dr.getDataAttributes().keySet()) {
            Object val = dr.getDataAttributes().get(key);
            int type = 0;
            boolean typeNeedsDispose = false;

            try {
                Object storeData = null;
                if (val instanceof Integer) {
                    type = LocalHDFDataType.INT.getHDFNativeType();
                    storeData = new int[] { (Integer) val };
                } else if (val instanceof Float) {
                    type = LocalHDFDataType.FLOAT.getHDFNativeType();
                    storeData = new float[] { (Float) val };
                } else if (val instanceof Double) {
                    type = LocalHDFDataType.DOUBLE.getHDFNativeType();
                    storeData = new double[] { (Double) val };
                } else if (val instanceof Byte) {
                    type = LocalHDFDataType.BYTE.getHDFNativeType();
                    storeData = new byte[] { (Byte) val };
                } else if (val instanceof Short) {
                    type = LocalHDFDataType.SHORT.getHDFNativeType();
                    storeData = new short[] { (Short) val };
                } else if (val instanceof Long) {
                    type = LocalHDFDataType.LONG.getHDFNativeType();
                    storeData = new long[] { (Long) val };
                } else if (val instanceof String) {
                    type = LocalHDFDataType.STRING.getHDFNativeType();
                    typeNeedsDispose = true;
                    try {
                        type = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
                        H5.H5Tset_size(type, ((String) val).length() + 1);
                        H5.H5Tset_strpad(type, HDF5Constants.H5T_STR_NULLTERM);
                    } catch (HDF5LibraryException e) {
                        try {
                            H5.H5Eclear();
                        } catch (HDF5LibraryException e1) {
                        }
                        throw new StorageException("Error setting up string",
                                dr, e);
                    }

                }

                if (type > 0) {
                    int s = 0;
                    int attr = 0;
                    try {
                        // s = H5.H5Screate(HDF5Constants.H5S_SCALAR);
                        s = H5.H5Screate_simple(1, new long[] { 1, 1 },
                                new long[] { 1, 1 });
                        attr = H5.H5Acreate(dataSet, key, type, s,
                                HDF5Constants.H5P_DEFAULT);
                        if (val instanceof String) {
                            byte[] d = ((String) val).getBytes();
                            byte[] padded = new byte[d.length + 1];
                            System.arraycopy(d, 0, padded, 0, d.length);
                            padded[padded.length - 1] = '\0';
                            H5.H5Awrite(attr, type, padded);
                        } else {
                            H5.H5Awrite(attr, type, storeData);
                        }
                    } catch (Exception e) {
                        try {
                            H5.H5Eclear();
                        } catch (HDF5LibraryException e1) {
                            // ignore
                        }
                        throw new StorageException("Error writing attribute",
                                dr, e);
                    } finally {
                        if (attr > 0) {
                            try {
                                H5.H5Aclose(attr);
                            } catch (HDF5LibraryException e) {
                                // ignore
                            }
                        }

                        try {
                            if (s > 0) {
                                H5.H5Sclose(s);
                            }
                        } catch (HDF5LibraryException e) {
                            // ignore
                        }

                    }
                }
            } finally {
                if (typeNeedsDispose && type > 0) {
                    try {
                        H5.H5Tclose(type);
                    } catch (HDF5LibraryException e) {
                        // ignore
                    }
                }
            }
        }

    }

    /**
     * Write the dataset
     * 
     * @param file_id
     * @param mydata
     * @param dims
     * @param szDims
     * @param dataset
     * @param group
     * @param properties
     * @param dataType
     * @param update
     *            optional flag that indicates updating is acceptable
     * @param ss
     *            storage status
     * @param rec
     * @throws StorageException
     */
    private void writeHDFDataset(int file_id, Object mydata, int dims,
            long[] szDims, String dataset, String group,
            StorageProperties properties, LocalHDFDataType dataType,
            StoreOp storeOp, IDataRecord rec, StorageStatus ss)
            throws StorageException {
        int dset = 0;
        int plist = 0;
        int dataspace = 0;

        try {

            // Reverse the dimensions for hdf5
            long[] szDims1 = new long[szDims.length];

            // the hdf5 maximum dimensions
            long[] maxDims = new long[szDims.length];

            // the max dimensions from the dataset
            long[] recMaxDims = rec.getMaxSizes();

            for (int i = 0; i < szDims.length; i++) {
                szDims1[i] = szDims[szDims.length - i - 1];
                if (recMaxDims == null || recMaxDims[i] == 0) {
                    maxDims[i] = HDF5Constants.H5S_UNLIMITED;
                } else {
                    maxDims[i] = recMaxDims[i];
                }
            }

            plist = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            int maxChunkSize = DEFAULT_CHUNK_SIZE;
            if (rec.getMaxChunkSize() > 0) {
                maxChunkSize = rec.getMaxChunkSize();
            }

            long[] chunk = new long[szDims.length];
            for (int i = 0; i < szDims.length; i++) {
                if (maxDims[i] == HDF5Constants.H5S_UNLIMITED) {
                    chunk[i] = maxChunkSize;
                } else {
                    chunk[i] = Math.min(maxChunkSize, (int) maxDims[i]);
                }
            }

            H5.H5Pset_chunk(plist, szDims.length,
                    HDFNativeData.longToByte(0, chunk.length, chunk));

            processCompressionFlags(properties, plist);

            String loc = group + "/" + dataset;

            int nativeType = dataType.getHDFNativeType();
            boolean datasetExists = false;

            try {
                // Check to see if data exists
                int tempDsId = H5.H5Dopen(file_id, loc);
                H5.H5Dclose(tempDsId);
                datasetExists = true;
            } catch (HDF5Exception e) {
                H5.H5Eclear();
            }

            if (!datasetExists) {

                // Write path
                boolean typeNeedsDisposal = false;
                try {
                    if (nativeType == HDF5Constants.H5T_STR_NULLTERM) {
                        nativeType = createStringType(rec);
                        typeNeedsDisposal = true;

                        if (rec instanceof StringDataRecord) {
                            mydata = ((StringDataRecord) rec)
                                    .getStorageObject();
                        }
                    }

                    dataspace = H5.H5Screate_simple(dims, HDFNativeData
                            .longToByte(0, szDims1.length, szDims1),
                            HDFNativeData
                                    .longToByte(0, maxDims.length, maxDims));

                    dset = H5.H5Dcreate(file_id, loc, nativeType, dataspace,
                            plist);

                    H5.H5Dwrite(dset, nativeType, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT,
                            mydata);
                    ss.setOperationPerformed(StoreOp.STORE_ONLY);

                    writeProperties(rec, dset);
                } finally {
                    if (typeNeedsDisposal == true) {
                        if (nativeType > 0) {
                            H5.H5Tclose(nativeType);
                        }
                    }
                }
            } else {
                if (storeOp == StoreOp.STORE_ONLY) {
                    throw new DuplicateRecordStorageException(
                            "Record already exists: " + loc, rec);
                }
                // Update or replace path

                else if (storeOp == StoreOp.APPEND) {
                    dset = H5.H5Dopen(file_id, loc);
                    dataspace = H5.H5Dget_space(dset);

                    long dimsOld[] = new long[szDims.length];
                    long memSpaceDims[] = new long[szDims.length];
                    for (int i = 0; i < memSpaceDims.length; i++) {
                        memSpaceDims[i] = szDims1[i];
                    }
                    H5.H5Sget_simple_extent_dims(dataspace, dimsOld, null);

                    szDims1[0] += dimsOld[0];

                    H5.H5Dextend(dset, HDFNativeData.longToByte(0,
                            szDims1.length, szDims1));
                    H5.H5Sclose(dataspace);
                    dataspace = H5.H5Dget_space(dset);
                    if (dimsOld.length > 1) {
                        dimsOld[dimsOld.length - 1] = 0;
                    }

                    H5.H5Sselect_hyperslab(dataspace,
                            HDF5Constants.H5S_SELECT_SET, HDFNativeData
                                    .longToByte(0, dimsOld.length, dimsOld),
                            null, HDFNativeData.longToByte(0,
                                    memSpaceDims.length, memSpaceDims), null);

                    int memSpace = 0;
                    try {
                        memSpace = H5.H5Screate_simple(memSpaceDims.length,
                                HDFNativeData.longToByte(0,
                                        memSpaceDims.length, memSpaceDims),
                                null);

                        // Write path
                        boolean typeNeedsDisposal = false;
                        try {
                            if (nativeType == HDF5Constants.H5T_STR_NULLTERM) {
                                nativeType = createStringType(rec);
                                typeNeedsDisposal = true;

                                if (rec instanceof StringDataRecord) {
                                    mydata = ((StringDataRecord) rec)
                                            .getStorageObject();
                                }
                            }

                            H5.H5Dwrite(dset, nativeType, memSpace, dataspace,
                                    HDF5Constants.H5P_DEFAULT, mydata);
                            ss.setOperationPerformed(StoreOp.APPEND);
                            ss.setIndexOfAppend(dimsOld);
                        } finally {
                            if (typeNeedsDisposal == true) {
                                if (nativeType > 0) {
                                    H5.H5Tclose(nativeType);
                                }
                            }
                        }
                    } finally {
                        if (memSpace > 0) {
                            H5.H5Sclose(memSpace);
                        }
                    }
                } else if (storeOp == StoreOp.REPLACE) {
                    H5.H5Gunlink(file_id, loc);
                    dataspace = H5.H5Screate_simple(dims, HDFNativeData
                            .longToByte(0, szDims1.length, szDims1),
                            HDFNativeData
                                    .longToByte(0, maxDims.length, maxDims));

                    boolean typeNeedsDisposal = false;
                    try {
                        if (nativeType == HDF5Constants.H5T_STR_NULLTERM) {
                            nativeType = createStringType(rec);
                            typeNeedsDisposal = true;
                        }
                        dset = H5.H5Dcreate(file_id, loc, nativeType,
                                dataspace, plist);

                        H5.H5Dwrite(dset, nativeType, HDF5Constants.H5S_ALL,
                                dataspace, HDF5Constants.H5P_DEFAULT, mydata);
                        ss.setOperationPerformed(StoreOp.REPLACE);
                    } finally {
                        if (typeNeedsDisposal == true) {
                            if (nativeType > 0) {
                                H5.H5Tclose(nativeType);
                            }
                        }
                    }
                } else if (storeOp == StoreOp.OVERWRITE) {
                    dset = H5.H5Dopen(file_id, loc);
                    dataspace = H5.H5Dget_space(dset);
                    boolean typeNeedsDisposal = false;
                    try {
                        if (nativeType == HDF5Constants.H5T_STR_NULLTERM) {
                            nativeType = createStringType(rec);
                            typeNeedsDisposal = true;
                        }

                        H5.H5Dwrite(dset, nativeType, HDF5Constants.H5S_ALL,
                                dataspace, HDF5Constants.H5P_DEFAULT, mydata);
                        ss.setOperationPerformed(StoreOp.REPLACE);
                        writeProperties(rec, dset);
                    } finally {
                        if (typeNeedsDisposal == true) {
                            if (nativeType > 0) {
                                H5.H5Tclose(nativeType);
                            }
                        }
                    }
                }

                else {
                    throw new UnsupportedOperationException(
                            "Unknown operation: " + storeOp);
                }

            }

        } catch (HDF5LibraryException e) {
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e1) {
                // ignore
            }
            throw new StorageException("Error occurred on storage", rec, e);
        } catch (HDF5Exception e) {
            e.printStackTrace();
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e2) {
                // ignore
            }
            throw new StorageException("Error occurred on storage", rec, e);
        } finally {

            try {
                if (plist > 0) {
                    H5.H5Pclose(plist);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }

            try {
                if (dataspace > 0) {
                    H5.H5Sclose(dataspace);
                }
            } catch (HDF5LibraryException e) {
                // Ignore
            }

            try {

                if (dset > 0) {
                    H5.H5Dclose(dset);
                }
            } catch (HDF5LibraryException e) {
                // Ignore
            }

        }
    }

    private int createStringType(IDataRecord rec) throws HDF5LibraryException {
        if (!(rec instanceof StringDataRecord)) {
            throw new IllegalArgumentException(
                    "Expected StringDataRecord, got "
                            + rec.getClass().getName());
        }

        int atype = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
        StringDataRecord sdr = (StringDataRecord) rec;
        if (sdr.getMaxLength() > 0) {
            H5.H5Tset_size(atype, sdr.getMaxLength());
        } else {
            H5.H5Tset_size(atype, HDF5Constants.H5T_VARIABLE);
        }
        H5.H5Tset_strpad(atype, HDF5Constants.H5T_STR_NULLTERM);

        return atype;
    }

    private void processCompressionFlags(StorageProperties properties, int plist)
            throws HDF5LibraryException {
        if (properties != null && properties.getCompression() != null) {
            switch (properties.getCompression()) {
            case ZLIB:
                H5.H5Pset_shuffle(plist);
                H5.H5Pset_deflate(plist, 1);
                break;
            case LZF:
                H5.H5Pset_shuffle(plist);
                H5.H5Pset_filter(plist, 32000, HDF5Constants.H5Z_FLAG_OPTIONAL,
                        0, null);
                break;
            case NONE:
                break;
            default:
                throw new IllegalArgumentException(
                        "Unsupported compression scheme: "
                                + properties.getCompression());
            }
        }
    }

    protected static void resize(IDataRecord rec, String originalDatasetName,
            String originalGroup, float scaleFactor, int level) {
        int w = (int) rec.getSizes()[0];
        int h = (int) rec.getSizes()[1];

        int len = w * h;
        java.awt.Point origin = new Point(0, 0);

        // create a sample model
        DataBuffer dataBuffer = null;
        int type = 0;

        Object in = rec.getDataObject();

        if (rec instanceof ByteDataRecord) {
            dataBuffer = new java.awt.image.DataBufferByte((byte[]) in, len);
            type = DataBuffer.TYPE_BYTE;
        } else if (rec instanceof ShortDataRecord) {
            dataBuffer = new java.awt.image.DataBufferShort((short[]) in, len);
            type = DataBuffer.TYPE_SHORT;
        } else if (rec instanceof IntegerDataRecord) {
            dataBuffer = new java.awt.image.DataBufferInt((int[]) in, len);
            type = DataBuffer.TYPE_INT;
        } else if (rec instanceof FloatDataRecord) {
            dataBuffer = new java.awt.image.DataBufferFloat((float[]) in, len);
            type = DataBuffer.TYPE_FLOAT;
        } else {
            throw new UnsupportedOperationException("["
                    + rec.getClass().getName() + "]"
                    + " not supported by resize.");
        }

        SampleModel sampleModel = RasterFactory.createBandedSampleModel(type,
                w, h, 1);

        // create a TiledImage using the float SampleModel
        TiledImage tiledImage = new TiledImage(0, 0, w, h, 0, 0, sampleModel,
                null);

        // create a Raster
        Raster raster = RasterFactory.createWritableRaster(sampleModel,
                dataBuffer, origin);

        // set the TiledImage data to that of the Raster
        tiledImage.setData(raster);

        PlanarImage scaledImg;

        // Interpolate the image using a scale factor and
        // the copy border extender
        ParameterBlockJAI param = new ParameterBlockJAI("Scale");
        param.addSource(tiledImage);
        param.setParameter("xScale", scaleFactor);
        param.setParameter("yScale", scaleFactor);
        Interpolation interpol = Interpolation
                .getInstance(Interpolation.INTERP_BICUBIC);
        param.setParameter("interpolation", interpol);
        RenderingHints hint = new RenderingHints(JAI.KEY_BORDER_EXTENDER,
                BorderExtender.createInstance(BorderExtender.BORDER_COPY));
        scaledImg = JAI.create("Scale", param, hint);

        // Get the floats back out
        DataBuffer newDb = scaledImg.getData().getDataBuffer();

        if (rec instanceof ByteDataRecord) {
            ((ByteDataRecord) rec).setByteData(((DataBufferByte) newDb)
                    .getData());
        } else if (rec instanceof ShortDataRecord) {
            ((ShortDataRecord) rec).setShortData(((DataBufferShort) newDb)
                    .getData());
        } else if (rec instanceof IntegerDataRecord) {
            ((IntegerDataRecord) rec).setIntData(((DataBufferInt) newDb)
                    .getData());
        } else if (rec instanceof FloatDataRecord) {
            ((FloatDataRecord) rec).setFloatData(((DataBufferFloat) newDb)
                    .getData());
        }

        rec.setName("" + level);
        rec.setGroup(originalGroup + "/" + originalDatasetName
                + "-interpolated");

        rec.setSizes(new long[] { w / 2, h / 2 });

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.IDataStore#delete(java.lang.String)
     */
    @Override
    public void delete(String... location) throws StorageException,
            FileNotFoundException {
        synchronized (HDF5DataStore.class) {
            setVerboseErrorMessages();
            int file_id = -1;
            try {
                lockFile(true);

                file_id = H5.H5Fopen(file.toString(),
                        HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5P_DEFAULT);
                for (String s : location) {
                    try {
                        H5.H5Gunlink(file_id, s);
                    } catch (HDF5SymbolTableException e) {
                        H5.H5Eclear();
                        // Don't throw exception in cases where group doesn't
                        // exist
                    }
                }

            } catch (HDF5LibraryException e) {
                try {
                    H5.H5Eclear();
                } catch (HDF5LibraryException e1) {
                    // ignore
                }
                throw new StorageException("Failure deleting location", null, e);
            } finally {
                try {
                    if (file_id >= 0) {
                        H5.H5Fclose(file_id);
                    }
                } catch (HDF5LibraryException e) {
                    // ignore
                }

                try {
                    if (file_id >= 0) {
                        H5.H5close();
                    }
                } catch (HDF5LibraryException e) {
                    // ignore
                }

                unlockFile();
            }
        }
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
        StorageException se = null;
        for (int i = 0; i < RETRIEVAL_RETRIES; i++) {
            try {
                return retrieveGroupsInternal(groups, request, 1.0f);
            } catch (StorageException e) {
                se = e;
            }
        }
        if (se != null) {
            throw se;
        }

        throw new StorageException("Unknown state after multiple retries", null);
    }

    @Override
    public void createDataset(IDataRecord rec) throws StorageException,
            FileNotFoundException {

        synchronized (HDF5DataStore.class) {
            setVerboseErrorMessages();
            int file_id = 0;
            try {
                try {
                    lockFile(true);
                    file_id = openFile(true);
                } catch (HDF5LibraryException e) {
                    throw new StorageException("Error opening file: "
                            + this.file + ":: ", null, e);
                }

                createHDFDataset(file_id, rec);

            } finally {
                closeFileAndCleanUp(file_id);
            }

        }

    }

    /**
     * Write the dataset
     * 
     * @param file_id
     * @param mydata
     * @param dims
     * @param szDims
     * @param dataset
     * @param group
     * @param properties
     * @param dataType
     * @param update
     *            optional flag that indicates updating is acceptable
     * @param rec
     * @throws StorageException
     */
    private void createHDFDataset(int file_id, IDataRecord rec)
            throws StorageException {

        int dims = rec.getDimension();
        StorageProperties properties = rec.getProperties();

        if (properties != null && !properties.isChunked()
                && properties.getCompression() != Compression.NONE) {
            throw new StorageException("Data must be chunked to be compressed",
                    null);
        }

        String group = rec.getGroup();
        this.createGroup(file_id, group, rec);

        int dset = 0;
        int plist = 0;

        long[] szDims = rec.getSizes();
        try {

            // Reverse the dimensions for hdf5
            long[] szDims1 = new long[szDims.length];
            for (int i = 0; i < szDims.length; i++) {
                szDims1[i] = szDims[szDims.length - i - 1];
            }

            int dataspace = H5.H5Screate_simple(dims,
                    HDFNativeData.longToByte(0, szDims1.length, szDims1),
                    (byte[]) null);

            plist = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            if (properties != null && properties.isChunked()) {
                long[] chunk = new long[2];
                chunk[0] = DEFAULT_CHUNK_SIZE;
                chunk[1] = DEFAULT_CHUNK_SIZE;
                H5.H5Pset_chunk(plist, 2, chunk);
            }

            processCompressionFlags(properties, plist);

            int nativeType = LocalHDFDataType.getCorrespondingEnum(rec)
                    .getHDFNativeType();
            Number fillValue = rec.getFillValue();
            if (fillValue != null) {
                Object array = Array.newInstance(fillValue.getClass(), 1);
                Array.set(array, 0, fillValue);
                H5.H5Pset_fill_value(plist, nativeType, array);
            }

            String loc = group + "/" + rec.getName();

            try {
                dset = H5.H5Dcreate(file_id, loc, nativeType, dataspace, plist);
                writeProperties(rec, dset);
            } catch (HDF5SymbolTableException e1) {
                H5.H5Eclear();
                throw new DuplicateRecordStorageException(
                        "Record already exists: " + loc, rec);
            }

        } catch (HDF5LibraryException e) {
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e1) {
                // ignore
            }
            throw new StorageException("Error occurred on storage", rec, e);
        } catch (HDF5Exception e) {
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e2) {
                // ignore
            }
            throw new StorageException("Error occurred on storage", rec, e);
        } finally {
            try {

                if (dset > 0) {
                    H5.H5Dclose(dset);
                }
            } catch (HDF5LibraryException e) {
                // Ignore
            }

            try {
                if (plist > 0) {
                    H5.H5Pclose(plist);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }
        }
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

        synchronized (HDF5DataStore.class) {
            setVerboseErrorMessages();
            int file_id = -1;
            try {
                lockFile(true);

                file_id = H5.H5Fopen(file.toString(),
                        HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5P_DEFAULT);

                for (Map.Entry<String, LinkLocation> entry : links.entrySet()) {
                    LinkLocation ll = entry.getValue();
                    String start = entry.getKey();

                    if (ll.fileName != null) {

                        String group = start.substring(0,
                                start.lastIndexOf("/"));
                        try {
                            // Check to see if data exists
                            int tempDsId = H5.H5Gopen(file_id, group);
                            H5.H5Gclose(tempDsId);
                            H5.H5Gunlink(file_id, start);
                            this.createGroup(file_id, group, null);
                        } catch (HDF5Exception e) {
                            H5.H5Eclear();
                            this.createGroup(file_id, group, null);
                        }

                        H5.H5Lcreate_external(ll.fileName, ll.linkTarget,
                                file_id, start, HDF5Constants.H5P_DEFAULT,
                                HDF5Constants.H5P_DEFAULT);
                    } else {
                        H5.H5Glink(file_id, HDF5Constants.H5G_LINK_SOFT, start,
                                ll.linkTarget);
                    }
                }

            } catch (HDF5LibraryException e) {
                try {
                    H5.H5Eclear();
                } catch (HDF5LibraryException e1) {
                    // ignore
                }
                throw new StorageException("Failure creating links!!", null, e);
            } finally {
                try {
                    if (file_id >= 0) {
                        H5.H5Fclose(file_id);
                    }
                } catch (HDF5LibraryException e) {
                    // ignore
                }

                try {
                    if (file_id >= 0) {
                        H5.H5close();
                    }
                } catch (HDF5LibraryException e) {
                    // ignore
                }

                unlockFile();
            }
        }
    }

    @Override
    public void deleteFiles(String[] datesToDelete) throws StorageException,
            FileNotFoundException {
        if (file.exists()) {
            if (file.isDirectory()) {
                ArrayList<File> files = FileUtil.listFiles(file, HDF5_FILTER,
                        true);
                for (String date : datesToDelete) {
                    for (File file : files) {
                        if (file.getName().contains(date)) {
                            file.delete();
                        }
                    }
                }
            } else {
                file.delete();
            }
        }
    }

    private static class H5Filter implements FilenameFilter {

        @Override
        public boolean accept(File dir, String name) {
            if (name.endsWith(".h5")) {
                return true;
            } else {
                return false;
            }
        }
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
