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
package com.raytheon.uf.edex.archive;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * This class handles moving processed data to the archiver directory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            rjpeter     Initial creation
 * Jan 18, 2013 1469       bkowal      Removed the hdf5 data directory.
 * Oct 23, 2013 2478       rferrel     Make date format thread safe.
 *                                     Add debug information.
 * Nov 05, 2013 2499       rjpeter     Repackaged, removed config files, always compresses hdf5.
 * Nov 11, 2013 2478       rjpeter     Updated data store copy to always copy hdf5.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class DatabaseArchiver implements IPluginArchiver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatabaseArchiver.class);

    /** Thread safe date format. */
    private static final ThreadLocal<SimpleDateFormat> TL_DATE_FORMAT = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat df = new SimpleDateFormat(
                    "yyyy-MM-dd HH:mm:ss.SSS");
            df.setTimeZone(TimeZone.getTimeZone("GMT"));
            return df;
        }
    };

    /** Minimum time increment to archive, note based off of insertTime. */
    private static final int MIN_DURATION_MILLIS = 1000 * 60 * 30;

    /** Maximum time increment to archive, note based off of insertTime. */
    private static final int MAX_DURATION_MILLIS = 1000 * 60 * 60;

    /** Job's name. */
    private static final String TASK_NAME = "DB Archiver";

    /** Cluster time out on lock. */
    private static final int CLUSTER_LOCK_TIMEOUT = 60000;

    /** Chunk size for I/O Buffering and Compression */
    private static final int CHUNK_SIZE = 8192;

    /** Mapping for plug-in formatters. */
    private final Map<String, IPluginArchiveFileNameFormatter> pluginArchiveFormatters;

    /** When true dump the pdos. */
    private final boolean debugArchiver;

    private final boolean compressDatabaseFiles;

    /**
     * The constructor.
     */
    public DatabaseArchiver() {
        pluginArchiveFormatters = new HashMap<String, IPluginArchiveFileNameFormatter>();
        pluginArchiveFormatters.put("default",
                new DefaultPluginArchiveFileNameFormatter());
        debugArchiver = Boolean.getBoolean("archive.debug.enable");
        compressDatabaseFiles = Boolean
                .getBoolean("archive.compression.enable");
    }

    @Override
    public void archivePlugin(String pluginName, String archivePath) {
        PluginProperties props = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName);
        if ((props != null) && (props.getRecord() != null)
                && (props.getDao() != null)) {
            Class<?> recordClass = props.getRecord();
            if (recordClass != null) {
                try {
                    recordClass.asSubclass(PluginDataObject.class);
                    archivePluginData(pluginName, archivePath);
                } catch (ClassCastException e) {
                    // not an error, using asSubClass to filter non
                    // PluginDataObjects
                }
            }
        }
    }

    @SuppressWarnings("rawtypes")
    public boolean archivePluginData(String pluginName, String archivePath) {
        SimpleDateFormat dateFormat = TL_DATE_FORMAT.get();
        // set archive time
        Calendar runTime = Calendar.getInstance();
        runTime.setTimeZone(TimeZone.getTimeZone("GMT"));
        runTime.add(Calendar.MINUTE, -30);

        // cluster lock, grabbing time of last successful archive
        CurrentTimeClusterLockHandler lockHandler = new CurrentTimeClusterLockHandler(
                CLUSTER_LOCK_TIMEOUT, dateFormat.format(runTime.getTime()),
                false);
        ClusterTask ct = ClusterLockUtils.lock(TASK_NAME, pluginName,
                lockHandler, false);
        if (!LockState.SUCCESSFUL.equals(ct.getLockState())) {
            return true;
        }

        Calendar startTime = null;
        long timimgStartMillis = System.currentTimeMillis();
        int recordCount = 0;
        statusHandler.info(pluginName + ": Archiving plugin");

        try {
            // lookup dao
            PluginDao dao = null;
            try {
                dao = PluginFactory.getInstance().getPluginDao(pluginName);
            } catch (PluginException e) {
                statusHandler
                        .error(pluginName
                                + ": Error getting data access object!  Unable to archive data!",
                                e);
                return false;
            }

            Set<String> datastoreFilesToArchive = new HashSet<String>();

            startTime = determineStartTime(pluginName, ct.getExtraInfo(),
                    runTime, dao);
            Calendar endTime = determineEndTime(startTime, runTime);
            Map<String, List<PersistableDataObject>> pdoMap = new HashMap<String, List<PersistableDataObject>>();

            IPluginArchiveFileNameFormatter archiveFormatter = pluginArchiveFormatters
                    .get(pluginName);
            if (archiveFormatter == null) {
                archiveFormatter = pluginArchiveFormatters.get("default");
            }

            while ((startTime != null) && (endTime != null)) {
                Map<String, List<PersistableDataObject>> pdosToSave = archiveFormatter
                        .getPdosByFile(pluginName, dao, pdoMap, startTime,
                                endTime);

                if ((pdosToSave != null) && !pdosToSave.isEmpty()) {
                    recordCount += savePdoMap(pluginName, archivePath,
                            pdosToSave);
                    for (Map.Entry<String, List<PersistableDataObject>> entry : pdosToSave
                            .entrySet()) {
                        List<PersistableDataObject> pdoList = entry.getValue();
                        if ((pdoList != null) && !pdoList.isEmpty()
                                && (pdoList.get(0) instanceof IPersistable)) {
                            datastoreFilesToArchive.add(entry.getKey());
                        }
                    }
                }

                startTime = endTime;
                endTime = determineEndTime(startTime, runTime);
            }

            if ((pdoMap != null) && !pdoMap.isEmpty()) {
                recordCount += savePdoMap(pluginName, archivePath, pdoMap);
                // don't forget to archive the HDF5 for the records that weren't
                // saved off by the prior while block
                for (Map.Entry<String, List<PersistableDataObject>> entry : pdoMap
                        .entrySet()) {
                    List<PersistableDataObject> pdoList = entry.getValue();
                    if ((pdoList != null) && !pdoList.isEmpty()
                            && (pdoList.get(0) instanceof IPersistable)) {
                        datastoreFilesToArchive.add(entry.getKey());
                    }
                }
            }

            if (!datastoreFilesToArchive.isEmpty()) {
                Compression compRequired = Compression.LZF;

                PluginProperties props = PluginRegistry.getInstance()
                        .getRegisteredObject(pluginName);

                if ((props != null) && (props.getCompression() != null)) {
                    if (compRequired.equals(Compression.valueOf(props
                            .getCompression()))) {
                        // if plugin is already compressed to the correct level,
                        // no additional compression required
                        compRequired = null;
                    }
                }

                for (String dataStoreFile : datastoreFilesToArchive) {
                    IDataStore ds = DataStoreFactory.getDataStore(new File(
                            FileUtil.join(pluginName, dataStoreFile)));
                    int pathSep = dataStoreFile.lastIndexOf(File.separatorChar);
                    String outputDir = (pathSep > 0 ? FileUtil.join(
                            archivePath, pluginName,
                            dataStoreFile.substring(0, pathSep)) : FileUtil
                            .join(archivePath, pluginName, dataStoreFile));

                    try {
                        // copy the changed hdf5 file, does repack if
                        // compRequired, otherwise pure file copy
                        ds.copy(outputDir, compRequired, null, 0, 0);
                    } catch (StorageException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage());
                    }
                }
            }

            // set last archive time to startTime
            if (startTime != null) {
                lockHandler
                        .setExtraInfo(dateFormat.format(startTime.getTime()));
            }

            if (recordCount > 0) {
                statusHandler.info(pluginName
                        + ": successfully archived "
                        + recordCount
                        + " records in "
                        + TimeUtil.prettyDuration(System.currentTimeMillis()
                                - timimgStartMillis));
            } else {
                statusHandler
                        .info(pluginName + ": Found no records to archive");
            }
        } catch (Throwable e) {
            // previous run time needs to be reset
            if (startTime != null) {
                lockHandler
                        .setExtraInfo(dateFormat.format(startTime.getTime()));
            }

            statusHandler.error(pluginName + ": Error occurred archiving data",
                    e);
        } finally {
            if (ct != null) {
                // release lock setting archive time in cluster lock
                ClusterLockUtils.unlock(ct, false);
            }
        }

        return true;
    }

    @SuppressWarnings("rawtypes")
    protected int savePdoMap(String pluginName, String archivePath,
            Map<String, List<PersistableDataObject>> pdoMap)
            throws SerializationException, IOException {
        int recordsSaved = 0;

        StringBuilder path = new StringBuilder();
        for (Map.Entry<String, List<PersistableDataObject>> entry : pdoMap
                .entrySet()) {
            path.setLength(0);
            path.append(archivePath).append(File.separator).append(pluginName)
                    .append(File.separator).append(entry.getKey());
            // remove .h5
            if (path.lastIndexOf(".h5") == (path.length() - 3)) {
                path.setLength(path.length() - 3);
            }
            int pathDebugLength = path.length();
            if (compressDatabaseFiles) {
                path.append(".bin.gz");
            } else {
                path.append(".bin");
            }

            File file = new File(path.toString());
            List<PersistableDataObject> pdosToSerialize = entry.getValue();
            recordsSaved += pdosToSerialize.size();

            if (file.exists()) {
                // read previous list in from disk (in gz format)
                InputStream is = null;

                try {

                    // created gzip'd stream
                    if (compressDatabaseFiles) {
                        is = new GZIPInputStream(new FileInputStream(file),
                                CHUNK_SIZE);
                    } else {
                        is = new BufferedInputStream(new FileInputStream(file),
                                CHUNK_SIZE);
                    }

                    // transform back for list append
                    @SuppressWarnings("unchecked")
                    List<PersistableDataObject<Object>> prev = SerializationUtil
                            .transformFromThrift(List.class, is);

                    statusHandler.info(pluginName + ": Read in " + prev.size()
                            + " records from file " + file.getAbsolutePath());

                    List<PersistableDataObject> newList = new ArrayList<PersistableDataObject>(
                            prev.size() + pdosToSerialize.size());

                    // get set of new identifiers
                    Set<Object> identifierSet = new HashSet<Object>(
                            pdosToSerialize.size(), 1);
                    for (PersistableDataObject pdo : pdosToSerialize) {
                        identifierSet.add(pdo.getIdentifier());
                    }

                    // merge records by Identifier, to remove old duplicate
                    for (PersistableDataObject pdo : prev) {
                        if (!identifierSet.contains(pdo.getIdentifier())) {
                            newList.add(pdo);
                        }
                    }

                    // release prev
                    prev = null;

                    newList.addAll(pdosToSerialize);
                    pdosToSerialize = newList;
                } finally {
                    if (is != null) {
                        try {
                            is.close();
                        } catch (IOException e) {
                            statusHandler.error(pluginName
                                    + ": Error occurred closing input stream",
                                    e);
                        }
                    }
                }
            }

            statusHandler.info(pluginName + ": Serializing "
                    + pdosToSerialize.size() + " records to file "
                    + file.getAbsolutePath());

            OutputStream os = null;

            try {
                if (!file.getParentFile().exists()) {
                    file.getParentFile().mkdirs();
                }

                if (debugArchiver) {
                    String debugRootName = path.substring(0, pathDebugLength);
                    dumpPdos(pluginName, pdosToSerialize, debugRootName);
                }

                // created gzip'd stream
                if (compressDatabaseFiles) {
                    os = new GZIPOutputStream(new FileOutputStream(file), CHUNK_SIZE);
                } else {
                    os = new BufferedOutputStream(new FileOutputStream(file),
                            CHUNK_SIZE);
                }

                // Thrift serialize pdo list
                SerializationUtil.transformToThriftUsingStream(pdosToSerialize,
                        os);
            } finally {
                if (os != null) {
                    try {
                        os.close();
                    } catch (IOException e) {
                        statusHandler.error(pluginName
                                + ": Error occurred closing output stream", e);
                    }
                }
            }
        }

        return recordsSaved;
    }

    /**
     * Dump the record information being archived to a file.
     */
    @SuppressWarnings("rawtypes")
    private void dumpPdos(String pluginName,
            List<PersistableDataObject> pdosToSerialize, String debugRootName) {
        StringBuilder sb = new StringBuilder(debugRootName);
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        sb.append("_").append(sdf.format(Calendar.getInstance().getTime()))
                .append(".txt");
        File file = new File(sb.toString());
        Writer writer = null;
        try {
            PersistableDataObject<?>[] pdoArray = pdosToSerialize
                    .toArray(new PersistableDataObject<?>[0]);
            writer = new BufferedWriter(new FileWriter(file));
            statusHandler.info(String.format("Dumping %s records to: %s",
                    pdoArray.length, file.getAbsolutePath()));
            for (int i = 0; i < pdosToSerialize.size(); ++i) {
                if (pdoArray[i] instanceof PluginDataObject) {
                    PluginDataObject pdo = (PluginDataObject) pdoArray[i];
                    if (pdo.getId() != 0) {
                        // otherwise was read from file
                        writer.write("" + pdo.getId() + ":");
                        writer.write(pdo.getDataURI());
                        writer.write("\n");
                    }
                } else {
                    writer.write(pdoArray[i].toString());
                    writer.write("\n");
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (Exception e) {
                    // Ignore
                }
                writer = null;
            }
        }
    }

    /**
     * Get the plug-in's start time for a query.
     * 
     * @param pluginName
     * @param extraInfo
     * @param runTime
     * @param dao
     * @return startTime
     * @throws DataAccessLayerException
     */
    protected Calendar determineStartTime(String pluginName, String extraInfo,
            Calendar runTime, PluginDao dao) throws DataAccessLayerException {
        Calendar startTime = null;
        SimpleDateFormat dateFormat = TL_DATE_FORMAT.get();

        // get previous run time
        if ((extraInfo != null) && !extraInfo.isEmpty()) {
            try {
                Date prevDate = dateFormat.parse(extraInfo);

                // cloning runTime as it already has the correct time zone
                startTime = (Calendar) runTime.clone();
                startTime.setTimeInMillis(prevDate.getTime());
            } catch (ParseException e) {
                statusHandler.error(pluginName
                        + ": Unable to parse last run time [" + extraInfo
                        + "], will archive all data up to current time", e);
                startTime = null;
            }
        }

        // protect against time failure where startTime is more than
        // MIN_DURATION in the future
        if (startTime != null) {
            if ((startTime.getTimeInMillis() - runTime.getTimeInMillis()) > MIN_DURATION_MILLIS) {
                statusHandler
                        .warn(pluginName
                                + ": Previous run time is a future time, reseting to current time.  Check server times");
                startTime = (Calendar) runTime.clone();
            }
        } else {
            // startTime has never been set lookup earliest start time
            Date minInsert = dao.getMinInsertTime(null);
            if (minInsert != null) {
                startTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
                startTime.setTimeInMillis(minInsert.getTime());
            } else {
                // if no data for plugin in db, set startTime to runTime
                startTime = (Calendar) runTime.clone();
            }
        }

        return startTime;
    }

    /**
     * Determines the endTime to bound the next query by. Uses
     * MAX_DURATION_MILLIS and MIN_DURATION_MILLIS to determine endTime based on
     * startTime and runTime. If starTime + MIN_DURATION_MILLIS < runTime, will
     * return null.
     * 
     * @param startTime
     *            Starting time bound for query.
     * @param runTime
     *            Time of current archive run.
     * @return
     */
    protected Calendar determineEndTime(Calendar startTime, Calendar runTime) {
        Calendar endTime = null;
        long timeDiff = runTime.getTimeInMillis() - startTime.getTimeInMillis();

        if (timeDiff > MAX_DURATION_MILLIS) {
            endTime = (Calendar) startTime.clone();
            endTime.setTimeInMillis(endTime.getTimeInMillis()
                    + MAX_DURATION_MILLIS);
        } else if (timeDiff > MIN_DURATION_MILLIS) {
            endTime = (Calendar) runTime.clone();
        }

        return endTime;
    }

    /**
     * Register archive formatter for a plug-in; and issue a warning if plug-in
     * is already registered.
     * 
     * @param pluginName
     * @param archiveFormatter
     * @return databaseArchiver
     */
    public Object registerPluginArchiveFormatter(String pluginName,
            IPluginArchiveFileNameFormatter archiveFormatter) {
        if (!pluginArchiveFormatters.containsKey(pluginName)) {
            pluginArchiveFormatters.put(pluginName, archiveFormatter);
        } else {
            statusHandler
                    .warn("Plugin archive formatter already registered for: "
                            + pluginName);
        }

        return this;
    }
}
