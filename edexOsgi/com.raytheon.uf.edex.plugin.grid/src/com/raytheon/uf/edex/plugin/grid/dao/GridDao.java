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

package com.raytheon.uf.edex.plugin.grid.dao;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.UnitConverter;

import org.hibernate.Query;
import org.hibernate.Session;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.grid.GridPathProvider;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.DataUriMetadataIdentifier;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IMetadataIdentifier.MetadataSpecificity;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.plugin.grid.PartialGrid;

import tec.uom.se.AbstractConverter;

/**
 * Data access object for accessing Grid records from the database
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 07, 2009  1994     bphillip    Initial Creation
 * Mar 14, 2013  1587     bsteffen    Fix static data persisting to datastore.
 * Mar 27, 2013  1821     bsteffen    Speed up GridInfoCache.
 * Mar 20, 2013  2910     bsteffen    Clear dataURI after loading cached info.
 * Jul 09, 2015  4500     rjpeter     Fix SQL Injection concern.
 * Sep 15, 2015  4819     rferrel     Made {@link #validateDataset(GridRecord)} public.
 * Nov 24, 2015  5154     nabowle     Handle id=0 when validating the level.
 * Mar 26, 2019  21070    dhaines     Piggybacking purging of orphaned grid coverages onto
 *                                    the existing grid info purge job.
 * Nov 11, 2020  8278     rjpeter     Updated orphaned grid_info and gridcoverage logic
 *                                    and logging.
 * Sep 23, 2021  8608     mapeters    Add metadata id handling
 * Jun 22, 2022  8865     mapeters    Update populateDataStore to return boolean
 * </pre>
 *
 * @author bphillip
 */
public class GridDao extends PluginDao {

    private static String purgeInfoCacheTopic = null;

    private static String purgeCoverageCacheTopic = null;

    public static String setPurgeModelCacheTopics(String purgeInfoCacheTopic,
            String purgeCoverageCacheTopic) {
        GridDao.purgeInfoCacheTopic = purgeInfoCacheTopic;
        GridDao.purgeCoverageCacheTopic = purgeCoverageCacheTopic;
        return purgeInfoCacheTopic + " & " + purgeCoverageCacheTopic;
    }

    public GridDao() throws PluginException {
        super(GridConstants.GRID);
    }

    public GridDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected boolean populateDataStore(IDataStore dataStore, IPersistable obj)
            throws Exception {
        GridRecord gridRec = (GridRecord) obj;
        Object messageData = gridRec.getMessageData();
        GridCoverage location = gridRec.getLocation();
        if (location != null && messageData instanceof float[]) {
            long[] sizes = new long[] { location.getNx(), location.getNy() };
            String abbrev = gridRec.getParameter().getAbbreviation();
            String group = gridRec.getDataURI();
            String datasetName = "Data";
            DataUriMetadataIdentifier metaId = new DataUriMetadataIdentifier(
                    gridRec);
            if (GridPathProvider.STATIC_PARAMETERS.contains(abbrev)) {
                group = "/" + location.getId();
                datasetName = abbrev;
                metaId.setSpecificity(MetadataSpecificity.DATASET);
            }
            AbstractStorageRecord storageRecord = null;
            Object partialGrid = gridRec.getExtraAttribute(PartialGrid.KEY);
            if ((partialGrid != null) && (partialGrid instanceof PartialGrid)) {
                /* Check if dataset needs to be created */
                PartialGrid pGrid = (PartialGrid) partialGrid;
                long[] pGridSize = new long[] { pGrid.getNx(), pGrid.getNy() };
                long[] pGridOffset = new long[] { pGrid.getxOffset(),
                        pGrid.getyOffset() };
                storageRecord = new FloatDataRecord(datasetName, group,
                        (float[]) messageData, 2, pGridSize);
                storageRecord.setMinIndex(pGridOffset);
                storageRecord.setMaxSizes(sizes);
                storageRecord.setFillValue(GridUtil.GRID_FILL_VALUE);
            } else {
                storageRecord = new FloatDataRecord(datasetName, group,
                        (float[]) messageData, 2, sizes);
            }

            storageRecord.setCorrelationObject(gridRec);
            StorageProperties sp = new StorageProperties();
            String compression = PluginRegistry.getInstance()
                    .getRegisteredObject(pluginName).getCompression();
            if (compression != null) {
                sp.setCompression(
                        StorageProperties.Compression.valueOf(compression));
            }
            sp.setChunked(true);
            dataStore.addDataRecord(storageRecord, metaId, sp);
            return true;
        } else {
            throw new Exception("Cannot create data record, spatialData = "
                    + location + " and messageData = " + messageData);
        }
    }

    @Override
    public void persistRecords(PluginDataObject... records)
            throws PluginException {
        List<PluginDataObject> toPersist = new ArrayList<>(records.length);
        for (PluginDataObject record : records) {
            GridRecord rec = (GridRecord) record;
            if ((rec.getParameter() == null)
                    || (rec.getParameter().getName() == null)
                    || "Missing".equals(rec.getParameter().getName())) {
                logger.info(
                        "Discarding record due to missing or unknown parameter mapping: "
                                + record);
            } else {
                boolean validLevel = false;
                Level level = rec.getLevel();

                if (level != null) {
                    MasterLevel ml = level.getMasterLevel();

                    if ((ml != null) && !LevelFactory.UNKNOWN_LEVEL
                            .equals(ml.getName())) {
                        validLevel = true;
                    }
                }

                if (validLevel) {
                    toPersist.add(rec);
                } else {
                    logger.info(
                            "Discarding record due to missing or unknown level mapping: "
                                    + record);
                }
            }
        }

        super.persistRecords(
                toPersist.toArray(new PluginDataObject[toPersist.size()]));
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

    @Override
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {

        List<IDataRecord[]> retVal = new ArrayList<>(objects.size());

        for (PluginDataObject rec : objects) {
            if (rec instanceof GridRecord) {
                try {
                    GridRecord obj = (GridRecord) rec;
                    IDataStore dataStore = getDataStore(obj);
                    String abbrev = obj.getParameter().getAbbreviation();
                    if (GridPathProvider.STATIC_PARAMETERS.contains(abbrev)) {
                        IDataRecord[] record = new IDataRecord[4];
                        record[0] = dataStore.retrieve(
                                "/" + obj.getLocation().getId(), abbrev,
                                Request.ALL);
                        retVal.add(record);
                    } else {
                        /* connect to the data store and retrieve the data */
                        IDataRecord[] record = new IDataRecord[4];
                        record[0] = dataStore.retrieve(obj.getDataURI(), "Data",
                                Request.ALL);

                        retVal.add(record);
                    }
                } catch (StorageException | FileNotFoundException e) {
                    throw new PluginException("Error getting HDF5 data", e);
                }
            }
        }

        return retVal;
    }

    private PluginDataObject[] verifyRecords(PluginDataObject... records) {
        List<PluginDataObject> toPersist = new ArrayList<>(records.length);
        for (PluginDataObject record : records) {
            GridRecord rec = (GridRecord) record;
            if (validateDataset(rec)) {
                toPersist.add(rec);
            }
        }
        return toPersist.toArray(new GridRecord[toPersist.size()]);
    }

    public boolean validateDataset(GridRecord record) {
        if (!validateParameter(record)) {
            return false;
        }
        if (!validateLevel(record)) {
            return false;
        }
        if (!validateCoverage(record)) {
            return false;
        }
        try {
            record.setInfo(
                    GridInfoCache.getInstance().getGridInfo(record.getInfo()));
        } catch (DataAccessLayerException e) {
            logger.handle(Priority.PROBLEM,
                    "Cannot load GridInfoRecord from DB for: "
                            + record.getDataURI(),
                    e);
            return false;
        }
        /* Clear the dataURI just in case something changed. */
        record.setDataURI(null);
        return true;

    }

    private boolean validateParameter(GridRecord record) {
        Parameter parameter = record.getParameter();
        boolean result = true;
        if (parameter == null || parameter.getName() == null
                || "Missing".equals(parameter.getName())) {
            result = false;
        } else {
            Parameter dbParameter = ParameterLookup.getInstance()
                    .getParameter(parameter, true);
            if (!parameter.equals(dbParameter)) {
                // This check is for debugging purposes
                // if (!parameter.getName().equals(dbParameter.getName())) {
                // logger.info("Record parameter name(" + parameter.getName()
                // + ") does not match database("
                // + dbParameter.getName() + ") "
                // + record.getDataURI());
                // }
                UnitConverter converter = Parameter.compareUnits(parameter,
                        dbParameter);
                // if (converter == null) {
                // logger.info("Record parameter unit("
                // + parameter.getUnitString()
                // + ") does not match database("
                // + dbParameter.getUnitString() + ") "
                // + record.getDataURI());
                // // For absolute accuracy we should abort if units don't
                // // match, but currently we will persist it anyway.
                // // result = false;
                // } else
                if ((converter != null)
                        && (converter != AbstractConverter.IDENTITY)) {
                    Object messageData = record.getMessageData();
                    if (messageData instanceof float[]) {
                        float[] data = (float[]) messageData;
                        for (int i = 0; i < data.length; i++) {
                            data[i] = (float) converter.convert(data[i]);
                        }
                    } else {
                        logger.info(
                                "Unable to convert grid data to correct units: "
                                        + record.getDataURI());
                    }
                }
            }
            record.setParameter(dbParameter);
        }
        if (!result) {
            logger.info(
                    "Discarding record due to missing or unknown parameter mapping: "
                            + record);
        }
        return result;
    }

    private boolean validateCoverage(GridRecord record) {
        GridCoverage coverage = record.getLocation();
        if (coverage == null) {
            logger.info("Discarding record due to missing location: " + record);
            return false;
        }
        GridCoverage dbCoverage = GridCoverageLookup.getInstance()
                .getCoverage(coverage, false);
        if (coverage == dbCoverage) {
            return true;
        }
        if (dbCoverage == null) {
            dbCoverage = GridCoverageLookup.getInstance().getCoverage(coverage,
                    true);
            logger.error("Unable to persist " + record
                    + " because storing the location failed.");
            if (dbCoverage == null) {
                return false;
            }
        }
        record.setLocation(dbCoverage);
        return true;
    }

    private boolean validateLevel(GridRecord record) {
        boolean result = false;
        Level level = record.getLevel();

        if (level != null) {
            MasterLevel ml = level.getMasterLevel();

            if ((ml != null)
                    && !LevelFactory.UNKNOWN_LEVEL.equals(ml.getName())) {
                result = true;
                if (level.getId() == 0) {
                    level = LevelFactory.getInstance().getLevel(ml.getName(),
                            level.getLevelonevalue(), level.getLeveltwovalue(),
                            ml.getUnitString());
                    if (level != null) {
                        record.setLevel(level);
                    }
                }
            }
        }

        if (!result) {
            logger.info(
                    "Discarding record due to missing or unknown level mapping: "
                            + record);
        }
        return result;
    }

    /**
     * Overridden to clean up orphan GridInfoRecords.
     */
    @Override
    public void delete(final List<PluginDataObject> objs) {
        super.delete(objs);

        try {
            txTemplate.execute(status -> {
                Map<Integer, GridInfoRecord> orphanedInfos = new HashMap<>(
                        objs.size());
                for (PluginDataObject pdo : objs) {
                    if (pdo instanceof GridRecord) {
                        GridInfoRecord info = ((GridRecord) pdo).getInfo();
                        orphanedInfos.put(info.getId(), info);
                    }
                }

                int rowsDeleted = 0;
                if (!orphanedInfos.isEmpty()) {
                    /**
                     * get list of all unreferenced grid_info records and purge
                     * them
                     */
                    Session sess = getCurrentSession();
                    Query query = sess.createQuery(
                            "select distinct rec.info.id from GridRecord rec where rec.info.id in (:ids)");
                    Set<Integer> orphanedInfoIds = orphanedInfos.keySet();
                    query.setParameterList("ids", orphanedInfoIds);
                    List<?> idsToKeep = query.list();
                    if (idsToKeep != null) {
                        orphanedInfoIds.removeAll(idsToKeep);
                    }

                    /*
                     * TODO: This only handles purge use case, doesn't handle
                     * manual use case
                     */
                    if (!orphanedInfos.isEmpty()) {
                        if (purgeInfoCacheTopic != null) {
                            Map<Integer, GridCoverage> orphanedCoverages = new HashMap<>(
                                    objs.size());
                            StringBuilder msg = new StringBuilder(160);
                            msg.append("Purging grid_info entries for ");

                            /*
                             * build log message and track possible orphan
                             * coverages
                             */
                            for (GridInfoRecord val : orphanedInfos.values()) {
                                msg.append("[Id: ").append(val.getId())
                                        .append(", info:")
                                        .append(val.toString()).append("], ");
                                GridCoverage coverage = val.getLocation();
                                orphanedCoverages.put(coverage.getId(),
                                        coverage);
                            }

                            msg.setLength(msg.length() - 2);
                            logger.info(msg.toString());

                            query = sess.createQuery(
                                    "delete from GridInfoRecord where id in (:ids)");
                            query.setParameterList("ids", orphanedInfoIds);
                            rowsDeleted = query.executeUpdate();

                            try {
                                EDEXUtil.getMessageProducer().sendAsyncUri(
                                        purgeInfoCacheTopic,
                                        new ArrayList<>(orphanedInfoIds));
                            } catch (EdexException e) {
                                logger.error(
                                        "Error sending message to purge grid info topic",
                                        e);
                            }

                            /**
                             * Now get list of all unreferenced gridcoverage
                             * records and purge them also, ideally we wouldn't
                             * purge static entries, but no good way to tell
                             * that. Loading GribSpatialCache would create
                             * dependency. Alternative of forcing delete from
                             * GribSpatialCache.
                             */
                            query = sess.createQuery(
                                    "select distinct info.location.id from GridInfoRecord info where info.location.id"
                                            + " in (:ids)");
                            Set<Integer> orphanedCoveragesIds = orphanedCoverages
                                    .keySet();
                            query.setParameterList("ids", orphanedCoveragesIds);
                            List<?> idsInUse = query.list();
                            if (idsInUse != null) {
                                orphanedCoveragesIds.removeAll(idsInUse);
                            }
                            if (!orphanedCoverages.isEmpty()) {
                                if (purgeCoverageCacheTopic != null) {
                                    msg.setLength(0);
                                    msg.append(
                                            "Purging gridcoverage entries for ");

                                    for (GridCoverage val : orphanedCoverages
                                            .values()) {
                                        msg.append("[Id: ").append(val.getId())
                                                .append(", Name: ")
                                                .append(val.getName())
                                                .append("], ");
                                    }

                                    msg.setLength(msg.length() - 2);
                                    logger.info(msg.toString());

                                    query = sess.createQuery(
                                            "delete from GridCoverage where id in (:ids)");
                                    query.setParameterList("ids",
                                            orphanedCoveragesIds);
                                    rowsDeleted += query.executeUpdate();

                                    try {
                                        EDEXUtil.getMessageProducer()
                                                .sendAsyncUri(
                                                        purgeCoverageCacheTopic,
                                                        new ArrayList<>(
                                                                orphanedCoveragesIds));
                                    } catch (EdexException e) {
                                        logger.error(
                                                "Error sending message to purge grid coverage topic",
                                                e);
                                    }
                                } else {
                                    GridCoverageLookup.getInstance()
                                            .purgeCaches(orphanedCoveragesIds);
                                    logger.warn(
                                            "Unable to purge grid coverage caches of clustered edices");
                                }
                            }
                        }

                    } else {
                        GridInfoCache.getInstance()
                                .purgeCache(new ArrayList<>(orphanedInfoIds));
                        logger.warn(
                                "Unable to purge grid info cache of clustered edices");
                    }
                }

                return rowsDeleted;
            });
        } catch (Exception e) {
            logger.error(
                    "Error purging orphaned grid info and grid coverage entries",
                    e);
        }
    }
}
