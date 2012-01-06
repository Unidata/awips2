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

package com.raytheon.edex.plugin.grib.dao;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.orm.hibernate3.HibernateTemplate;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
import com.raytheon.uf.common.dataplugin.grib.GribPathProvider;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;

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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class GribDao extends PluginDao {

    private static final String LOCAL_SECTION = "localSection";

    private static final String HYBRID_LEVELS = "hybridLevels";

    private static final String THINNED_PTS = "thinnedPts";

    private static final String REBUILD_CACHE_TOPIC = "jms-generic:topic:rebuildD2DCache";

    private static final String PURGE_MODEL_CACHE_TOPIC = "jms-generic:topic:purgeGribModelCache";

    private static final String ORPHAN_MODEL_QUERY = "select id from awips.grib_models where id not in(select distinct(modelinfo_id) from awips.grib)";

    /**
     * Creates a new GribPyDao object
     * 
     * @param pluginName
     *            The name of the plugin. In this case, grib.
     * @throws PluginException
     *             If problems occur instantiating dao
     */
    public GribDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    public GribDao() throws PluginException {
        this("grib");
    }

    public void purgeExpiredData() throws PluginException {
        super.purgeExpiredData();
        try {
            List<Integer> orphanedIds = purgeGribModelOrphans();
            EDEXUtil.getMessageProducer().sendAsyncUri(PURGE_MODEL_CACHE_TOPIC,
                    orphanedIds);
        } catch (DataAccessLayerException e1) {
            statusHandler
                    .error("Error purging orphaned grib model entries", e1);
        } catch (EdexException e) {
            statusHandler.error(
                    "Error sending message to purge grib model topic", e);
        }
        try {
            EDEXUtil.getMessageProducer().sendAsyncUri(REBUILD_CACHE_TOPIC,
                    null);
        } catch (EdexException e) {
            statusHandler.error(
                    "Error sending message to rebuild D2D Cache topic", e);
        }
    }

    private List<Integer> purgeGribModelOrphans()
            throws DataAccessLayerException {
        QueryResult result = (QueryResult) this
                .executeNativeSql(ORPHAN_MODEL_QUERY);
        List<Integer> orphanedIds = new ArrayList<Integer>();
        for (int i = 0; i < result.getResultCount(); i++) {
            orphanedIds.add((Integer) result.getRowColumnValue(0, 0));
        }
        if (!orphanedIds.isEmpty()) {
            this.executeNativeSql(ORPHAN_MODEL_QUERY.replace("select id",
                    "delete"));
        }
        return orphanedIds;
    }

    public void purgeAllData() throws PluginException {
        super.purgeAllData();
        try {
            List<Integer> orphanedIds = purgeGribModelOrphans();
            EDEXUtil.getMessageProducer().sendAsyncUri(PURGE_MODEL_CACHE_TOPIC,
                    orphanedIds);
        } catch (DataAccessLayerException e1) {
            statusHandler
                    .error("Error purging orphaned grib model entries", e1);
        } catch (EdexException e) {
            statusHandler.error(
                    "Error sending message to purge grib model topic", e);
        }
        try {
            EDEXUtil.getMessageProducer().sendAsyncUri(REBUILD_CACHE_TOPIC,
                    null);
        } catch (EdexException e) {
            throw new PluginException(
                    "Error sending message to rebuild D2D Cache topic", e);
        }
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        GribRecord gribRec = (GribRecord) obj;

        if (gribRec.getMessageData() != null
                && !gribRec.getModelInfo().getParameterName().equals("Missing")) {
            AbstractStorageRecord storageRecord = null;
            AbstractStorageRecord localSection = null;
            AbstractStorageRecord hybridLevels = null;
            AbstractStorageRecord thinnedPts = null;

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
            String compression = PluginRegistry.getInstance()
                    .getRegisteredObject(pluginName).getCompression();
            if (compression != null) {
                sp.setCompression(StorageProperties.Compression
                        .valueOf(compression));
            }
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

            if (rec instanceof GribRecord) {
                GribRecord obj = (GribRecord) rec;

                try {
                    IDataStore dataStore = getDataStore(obj);
                    String abbrev = obj.getModelInfo()
                            .getParameterAbbreviation();
                    if (GribPathProvider.STATIC_PARAMETERS.contains(abbrev)) {
                        record = new IDataRecord[4];
                        record[0] = dataStore
                                .retrieve("/", abbrev, Request.ALL);
                        retVal.add(record);
                    } else {
                        /* connect to the data store and retrieve the data */

                        record = new IDataRecord[4];

                        record[0] = dataStore.retrieve(obj.getDataURI(),
                                "Data", Request.ALL);

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

                        retVal.add(record);
                    }
                } catch (StorageException e) {
                    throw new PluginException("Error getting HDF5 data", e);
                } catch (FileNotFoundException e) {
                    throw new PluginException("Error getting HDF5 data", e);
                }
            }
        }

        return retVal;
    }

    @Override
    public void persistRecords(PluginDataObject... records)
            throws PluginException {
        List<PluginDataObject> toPersist = new ArrayList<PluginDataObject>();
        for (PluginDataObject record : records) {
            GribRecord rec = (GribRecord) record;
            GribModel model = rec.getModelInfo();
            if (model.getParameterName() == null
                    || model.getParameterName().equals("Missing")) {
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

        super.persistRecords(toPersist.toArray(new PluginDataObject[toPersist
                .size()]));
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
            GribRecord rec = (GribRecord) record;
            GribModel model = rec.getModelInfo();
            if (model.getParameterName() == null
                    || model.getParameterName().equals("Missing")) {
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
        return toPersist.toArray(new GribRecord[toPersist.size()]);
    }

    public boolean isSTopoInDb(GribRecord record) {
        Session s = getHibernateTemplate().getSessionFactory().openSession();
        try {
            Criteria baseCriteria = s.createCriteria(GribRecord.class);
            Criterion baseCritia = Restrictions.and(
                    Restrictions.eq("modelInfo", record.getModelInfo()),
                    Restrictions.eq("dataTime", record.getDataTime()));
            baseCriteria.add(baseCritia);
            List<?> results = baseCriteria.list();
            if (results.isEmpty()) {
                return false;
            } else {
                return true;
            }
        } finally {
            if (s != null) {
                s.flush();
                s.close();
            }
        }
    }

    public List<StorageException> replaceRecord(GribRecord pdo)
            throws PluginException {
        List<StorageException> exceptions = new ArrayList<StorageException>();
        IPersistable persistable = (IPersistable) pdo;
        persistable.setHdfFileId(EDEXUtil.getServerId());

        // get the directory
        String directory = HDF5_DIR
                + File.separator
                + pdo.getPluginName()
                + File.separator
                + pathProvider.getHDFPath(pdo.getPluginName(),
                        (IPersistable) pdo);
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

    public int purgeModelData(final String modelName) {
        return (Integer) txTemplate.execute(new TransactionCallback() {
            @Override
            public Object doInTransaction(TransactionStatus status) {
                int rval = 0;
                HibernateTemplate ht = getHibernateTemplate();
                Session sess = ht.getSessionFactory().getCurrentSession();
                Query modelIdQuery = sess
                        .createQuery("SELECT distinct id from GribModel where modelName = :modelName");
                modelIdQuery.setString("modelName", modelName);
                List<Integer> mIds = (List<Integer>) modelIdQuery.list();
                for (Integer mId : mIds) {
                    Query query = sess
                            .createQuery("DELETE from GribRecord where modelInfo.id = :mId");
                    query.setInteger("mId", mId);
                    rval += query.executeUpdate();
                }

                return rval;
            }
        });
    }

    public void purgeHdf5ModelData(final String modelName)
            throws FileNotFoundException, StorageException {
        IDataStore dataStore = DataStoreFactory.getDataStore(new File(
                getHDF5Path("modelInfo.modelName=" + modelName)));
        dataStore.deleteFiles(null);
    }
}
