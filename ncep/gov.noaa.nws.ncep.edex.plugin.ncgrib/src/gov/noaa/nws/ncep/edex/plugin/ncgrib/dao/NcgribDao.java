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

package gov.noaa.nws.ncep.edex.plugin.ncgrib.dao;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribPathProvider;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.edex.common.dao.NcepDefaultPluginDao;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.hdf5.HDF5PluginFilenameFilter;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Data access object for accessing Grib records from the database
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 12/16/10				    mli			extend NcepDefaultPluginDao to enable purge
 * 01/14/13     1469        bkowal      Removed the hdf5 data directory.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class NcgribDao extends NcepDefaultPluginDao {

    // FIXME: Add rules for purging
    /** Temporary value of how many models to keep */
    private static final int MODELCOUNT = 2;

    private static final String MODEL_QUERY = "select distinct modelname from awips.ncgrib_models";

    private static final String REFTIME_QUERY = "select distinct reftime from awips.ncgrib where modelinfo_id in (select id from awips.ncgrib_models where modelname='?') order by reftime desc";

    private static final String PURGE_SQL = "delete from awips.ncgrib where refTime='?' and modelinfo_id in(select id from awips.ncgrib_models where modelname='?')";

    private static final String LOCAL_SECTION = "localSection";

    private static final String HYBRID_LEVELS = "hybridLevels";

    private static final String THINNED_PTS = "thinnedPts";

    private static final HDF5PluginFilenameFilter fileFilter = new HDF5PluginFilenameFilter(
            "ncgrib");

    /**
     * Creates a new NcgribPyDao object
     * 
     * @param pluginName
     *            The name of the plugin. In this case, ncgrib.
     * @throws PluginException
     *             If problems occur instantiating dao
     */
    public NcgribDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    public NcgribDao() throws PluginException {
        this("ncgrib");
    }

    // public void purgeExpiredData() {
    // QueryResult models = null;
    // try {
    // models = (QueryResult) executeNativeSql(MODEL_QUERY);
    // } catch (DataAccessLayerException e) {
    // logger.error("Error purging ncgrib data.  Unable to get models", e);
    // }
    //
    // String currentModel = null;
    // for (int i = 0; i < models.getResultCount(); i++) {
    // currentModel = (String) models.getRowColumnValue(i, 0);
    // QueryResult refTimes = null;
    // try {
    // refTimes = (QueryResult) executeNativeSql(REFTIME_QUERY
    // .replace("?", currentModel));
    // } catch (DataAccessLayerException e) {
    // logger
    // .error("Error purging ncgrib data. Unable to get reference times for model ["
    // + currentModel + "]");
    // continue;
    // }
    //
    // // FIXME: Add rules for purging here instead of just keeping 2
    // // runs
    // List<String> filesKept = new ArrayList<String>();
    // File modelDirectory = new File(PLUGIN_HDF5_DIR + File.separator
    // + currentModel);
    //
    // for (int j = 0; j < refTimes.getResultCount(); j++) {
    // Date time = (Date) refTimes.getRowColumnValue(j, 0);
    // File hdf5File = new File(modelDirectory.getAbsolutePath()
    // + File.separator
    // + ((NcgribPathProvider) pathProvider).formatTime(time)
    // + ".h5");
    //
    // if (j < MODELCOUNT) {
    // filesKept.add(hdf5File.getAbsolutePath());
    // continue;
    // }
    //
    // try {
    // purgeDb(time, currentModel);
    // } catch (DataAccessLayerException e) {
    // logger.error("Error purging database for ncgrib model ["
    // + currentModel + "]");
    // }
    // }
    //
    // List<File> files = FileUtil.listFiles(modelDirectory, fileFilter,
    // false);
    //
    // for (File file : files) {
    // if (!filesKept.contains(file.getAbsolutePath())) {
    // if (!file.delete()) {
    // logger
    // .error("Error purging HDF5 files for ncgrib model ["
    // + currentModel + "]");
    // }
    // }
    // }
    //
    // }
    //
    // }

    private int purgeDb(final Date date, String modelName)
            throws DataAccessLayerException {
        String sql = PURGE_SQL.replaceFirst("\\?", TimeUtil.formatDate(date))
                .replaceFirst("\\?", modelName);
        Integer results = null;
        results = (Integer) this.executeNativeSql(sql);
        return results;
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        NcgribRecord gribRec = (NcgribRecord) obj;

        if (gribRec.getMessageData() != null
                && !gribRec.getModelInfo().getParameterName().equals("Missing")) {
            AbstractStorageRecord storageRecord = null;
            AbstractStorageRecord localSection = null;
            AbstractStorageRecord hybridLevels = null;
            AbstractStorageRecord thinnedPts = null;

            // System.out.println (" good data to be populated, rec datauri=" +
            // gribRec.getDataURI());
            // System.out.println
            // (" good data to be populated, rec messagedata=" +
            // gribRec.getMessageData());

            /*
             * Stores the binary data to the HDF5 data store
             */
            if (gribRec.getMessageData() instanceof float[]) {
                if (gribRec.getSpatialObject() != null
                        && gribRec.getMessageData() != null) {
                    long[] sizes = new long[] {
                            (gribRec.getSpatialObject()).getNx(),
                            (gribRec.getSpatialObject()).getNy() };
                    storageRecord = new FloatDataRecord("Data",
                            gribRec.getDataURI(),
                            (float[]) gribRec.getMessageData(), 2, sizes);
                } else
                    throw new Exception(
                            "Cannot create data record, spatialData = "
                                    + gribRec.getSpatialObject()
                                    + " and messageData = "
                                    + gribRec.getMessageData());
            } else if (gribRec.getMessageData() instanceof byte[]) {
                storageRecord = new ByteDataRecord("Data",
                        gribRec.getDataURI(), (byte[]) gribRec.getMessageData());
            } else {
                throw new PluginException("Invalid message data type: "
                        + gribRec.getMessageData().getClass());
            }

            /*
             * Stores any data from the local section if present
             */
            if (gribRec.isLocalSectionUsed()) {
                localSection = new IntegerDataRecord(LOCAL_SECTION,
                        gribRec.getDataURI(), gribRec.getLocalSection());
                localSection.setCorrelationObject(gribRec);
                dataStore.addDataRecord(localSection);
            }

            /*
             * Stores any hybrid coordinate data if present
             */
            if (gribRec.isHybridGrid()) {
                hybridLevels = new FloatDataRecord(HYBRID_LEVELS,
                        gribRec.getDataURI(), gribRec.getHybridCoordList());
                hybridLevels.setCorrelationObject(gribRec);
                dataStore.addDataRecord(hybridLevels);
            }

            /*
             * Stores any thinned point data for quasi-regular grids if present
             */
            if (gribRec.isThinnedGrid()) {
                thinnedPts = new IntegerDataRecord(THINNED_PTS,
                        gribRec.getDataURI(), gribRec.getThinnedPts());
                thinnedPts.setCorrelationObject(gribRec);
                dataStore.addDataRecord(thinnedPts);
            }

            storageRecord.setCorrelationObject(gribRec);
            StorageProperties sp = new StorageProperties();
            sp.setCompression(Compression.LZF);
            sp.setChunked(true);
            dataStore.addDataRecord(storageRecord, sp);
        }
        return dataStore;
    }

    @Override
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {

        List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>();

        for (PluginDataObject rec : objects) {
            IDataRecord[] record = null;

            if (rec instanceof NcgribRecord) {
                NcgribRecord obj = (NcgribRecord) rec;
                /* connect to the data store and retrieve the data */
                IDataStore dataStore = getDataStore(obj);
                record = new IDataRecord[4];
                try {

                    record[0] = dataStore.retrieve(obj.getDataURI(), "Data",
                            Request.ALL);

                    if (obj.isLocalSectionUsed()) {
                        record[1] = dataStore.retrieve(obj.getDataURI(),
                                LOCAL_SECTION, Request.ALL);
                    }
                    if (obj.isHybridGrid()) {
                        record[2] = dataStore.retrieve(obj.getDataURI(),
                                HYBRID_LEVELS, Request.ALL);
                    }
                    if (obj.isThinnedGrid()) {
                        record[3] = dataStore.retrieve(obj.getDataURI(),
                                THINNED_PTS, Request.ALL);
                    }
                } catch (StorageException e) {
                    throw new PluginException("Error getting HDF5 data", e);
                } catch (FileNotFoundException e) {
                    throw new PluginException("Error getting HDF5 data", e);
                }
                retVal.add(record);
            }
        }

        return retVal;
    }

    @Override
    public void persistRecords(PluginDataObject... records)
            throws PluginException {
        List<PluginDataObject> toPersist = new ArrayList<PluginDataObject>();
        for (PluginDataObject record : records) {
            NcgribRecord rec = (NcgribRecord) record;
            NcgribModel model = rec.getModelInfo();
            if (model.getParameterName() == null
                    || model.getParameterName().equals("Missing")) {
                // System.out.println (" persist missing or null, rec datauri="
                // + rec.getDataURI());

                logger.info("Discarding record due to missing or unknown parameter mapping: "
                        + record);
            } else {
                boolean validLevel = false;
                Level level = model.getLevel();

                if (level != null) {
                    MasterLevel ml = level.getMasterLevel();

                    if (ml != null
                            && !LevelFactory.UNKNOWN_LEVEL.equals(ml.getName())) {
                        validLevel = true;
                    }
                }

                if (validLevel) {
                    toPersist.add(rec);
                } else {
                    logger.info("Discarding record due to missing or unknown level mapping: "
                            + record);
                }
            }
        }

        super.persistRecords(toPersist.toArray(new PluginDataObject[0]));
    }

    @Override
    public PluginDataObject[] persistToDatabase(PluginDataObject... records) {
        return super.persistToDatabase(verifyRecords(records));
    }

    @Override
    public StorageStatus persistToHDF5(PluginDataObject... records)
            throws PluginException {
        return super.persistToHDF5(verifyRecords(records));
    }

    private PluginDataObject[] verifyRecords(PluginDataObject... records) {
        List<PluginDataObject> toPersist = new ArrayList<PluginDataObject>();
        for (PluginDataObject record : records) {
            NcgribRecord rec = (NcgribRecord) record;
            NcgribModel model = rec.getModelInfo();
            if (model.getParameterName() == null
                    || model.getParameterName().equals("Missing")) {
                // System.out.println (" verify missing or null, rec datauri=" +
                // rec.getDataURI());

                logger.info("Discarding record due to missing or unknown parameter mapping: "
                        + record);
            } else {
                boolean validLevel = false;
                Level level = model.getLevel();

                if (level != null) {
                    MasterLevel ml = level.getMasterLevel();

                    if (ml != null
                            && !LevelFactory.UNKNOWN_LEVEL.equals(ml.getName())) {
                        validLevel = true;
                    }
                }

                if (validLevel) {
                    toPersist.add(rec);
                } else {
                    logger.info("Discarding record due to missing or unknown level mapping: "
                            + record);
                }
            }
        }
        return toPersist.toArray(new NcgribRecord[0]);
    }

    public List<StorageException> replaceRecord(NcgribRecord pdo)
            throws PluginException {
        List<StorageException> exceptions = new ArrayList<StorageException>();
        IPersistable persistable = (IPersistable) pdo;
        persistable.setHdfFileId(EDEXUtil.getServerId());

        // get the directory
        String directory = pdo.getPluginName() + File.separator
                + pathProvider.getHDFPath(this.pluginName, persistable);
        File dataStoreFile = new File(directory + File.separator
                + pathProvider.getHDFFileName(pdo.getPluginName(), persistable));

        IDataStore dataStore = DataStoreFactory.getDataStore(dataStoreFile);

        try {
            populateDataStore(dataStore, persistable);
        } catch (Exception e) {
            throw new PluginException("Error populating data store", e);
        }

        StorageStatus s = null;
        try {
            s = dataStore.store(StoreOp.REPLACE);

            // add exceptions to a list for aggregation
            exceptions.addAll(Arrays.asList(s.getExceptions()));
        } catch (StorageException e) {
            logger.error("Error persisting to HDF5", e);
        }
        return exceptions;
    }

}
