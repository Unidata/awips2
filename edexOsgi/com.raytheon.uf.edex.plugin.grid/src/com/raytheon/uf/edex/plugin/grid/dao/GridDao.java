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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.measure.converter.UnitConverter;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridPathProvider;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Data access object for accessing Grid records from the database
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
public class GridDao extends PluginDao {

    private static String purgeModelCacheTopic = null;

    public static String setPurgeModelCacheTopic(String purgeModelCacheTopic) {
        GridDao.purgeModelCacheTopic = purgeModelCacheTopic;
        return purgeModelCacheTopic;
    }

    public GridDao() throws PluginException {
        super(GridConstants.GRID);
    }

    public GridDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        GridRecord gridRec = (GridRecord) obj;
        Object messageData = gridRec.getMessageData();
        GridCoverage location = gridRec.getLocation();
        if (location != null && messageData instanceof float[]) {
            long[] sizes = new long[] { location.getNx(), location.getNy() };
            String abbrev = gridRec.getParameter().getAbbreviation();
            String group = gridRec.getDataURI();
            if (GridPathProvider.STATIC_PARAMETERS.contains(abbrev)) {
                group = "/" + location.getId();
            }
            AbstractStorageRecord storageRecord = new FloatDataRecord("Data",
                    group, (float[]) messageData, 2, sizes);

            storageRecord.setCorrelationObject(gridRec);
            StorageProperties sp = new StorageProperties();
            String compression = PluginRegistry.getInstance()
                    .getRegisteredObject(pluginName).getCompression();
            if (compression != null) {
                sp.setCompression(StorageProperties.Compression
                        .valueOf(compression));
            }
            sp.setChunked(true);
            dataStore.addDataRecord(storageRecord, sp);
        } else {
            throw new Exception("Cannot create data record, spatialData = "
                    + location + " and messageData = " + messageData);
        }
        return dataStore;
    }

    @Override
    public void persistRecords(PluginDataObject... records)
            throws PluginException {
        List<PluginDataObject> toPersist = new ArrayList<PluginDataObject>(
                records.length);
        for (PluginDataObject record : records) {
            GridRecord rec = (GridRecord) record;
            if (rec.getParameter() == null
                    || rec.getParameter().getName() == null
                    || rec.getParameter().getName().equals("Missing")) {
                logger.info("Discarding record due to missing or unknown parameter mapping: "
                        + record);
            } else {
                boolean validLevel = false;
                Level level = rec.getLevel();

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

    @Override
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {

        List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>(
                objects.size());

        for (PluginDataObject rec : objects) {
            if (rec instanceof GridRecord) {
                try {
                    GridRecord obj = (GridRecord) rec;
                    IDataStore dataStore = getDataStore(obj);
                    String abbrev = obj.getParameter().getAbbreviation();
                    if (GridPathProvider.STATIC_PARAMETERS.contains(abbrev)) {
                        IDataRecord[] record = new IDataRecord[4];
                        record[0] = dataStore.retrieve("/"
                                + obj.getLocation().getId(), abbrev,
                                Request.ALL);
                        retVal.add(record);
                    } else {
                        /* connect to the data store and retrieve the data */
                        IDataRecord[] record = new IDataRecord[4];
                        record[0] = dataStore.retrieve(obj.getDataURI(),
                                "Data", Request.ALL);

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

    private PluginDataObject[] verifyRecords(PluginDataObject... records) {
        List<PluginDataObject> toPersist = new ArrayList<PluginDataObject>(
                records.length);
        for (PluginDataObject record : records) {
            GridRecord rec = (GridRecord) record;
            if (validateDataset(rec)) {
                toPersist.add(rec);
            }
        }
        return toPersist.toArray(new GridRecord[toPersist.size()]);
    }

    private boolean validateDataset(GridRecord record) {
        if (!validateParameter(record)) {
            return false;
        }
        if (!validateLevel(record)) {
            return false;
        }
        if (!validateCoverage(record)) {
            return false;
        }
        record.setInfo(GridInfoCache.getInstance()
                .getGridInfo(record.getInfo()));
        return true;

    }

    private boolean validateParameter(GridRecord record) {
        Parameter parameter = record.getParameter();
        boolean result = true;
        if (parameter == null) {
            result = false;
        } else if (parameter.getName() == null) {
            result = false;
        } else if (parameter.getName().equals("Missing")) {
            result = false;
        } else {
            Parameter dbParameter = ParameterLookup.getInstance().getParameter(
                    parameter, true);
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
                if (converter != null && converter != UnitConverter.IDENTITY) {
                    Object messageData = record.getMessageData();
                    if (messageData instanceof float[]) {
                        float[] data = (float[]) messageData;
                        for (int i = 0; i < data.length; i++) {
                            data[i] = (float) converter.convert(data[i]);
                        }
                    } else {
                        logger.info("Unable to convert grid data to correct units: "
                                + record.getDataURI());
                    }
                }
            }
            record.setParameter(dbParameter);
        }
        if (!result) {
            logger.info("Discarding record due to missing or unknown parameter mapping: "
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
        GridCoverage dbCoverage = GridCoverageLookup.getInstance().getCoverage(
                coverage, false);
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
        if (!coverage.getId().equals(dbCoverage.getId())) {
            record.setDataURI(null);
            try {
                record.constructDataURI();
            } catch (PluginException e) {
                logger.info("Error constructing dataURI: " + record);
                return false;
            }
        }
        return true;
    }

    private boolean validateLevel(GridRecord record) {
        boolean result = false;
        Level level = record.getLevel();

        if (level != null) {
            MasterLevel ml = level.getMasterLevel();

            if (ml != null && !LevelFactory.UNKNOWN_LEVEL.equals(ml.getName())) {
                result = true;
            }
        }

        if (!result) {
            logger.info("Discarding record due to missing or unknown level mapping: "
                    + record);
        }
        return result;
    }

    /**
     * Overridden to clean up orphan GridInfoRecords.
     */
    @Override
    public void delete(List<PluginDataObject> objs) {
        super.delete(objs);
        Set<Integer> orphanedIds = new HashSet<Integer>(objs.size());
        StringBuilder sqlString = new StringBuilder(objs.size() * 15 + 80);
        sqlString
                .append("select distinct info_id from awips.grid where info_id in (");
        for (PluginDataObject pdo : objs) {
            if (pdo instanceof GridRecord) {
                Integer id = ((GridRecord) pdo).getInfo().getId();
                if (orphanedIds.add(id)) {
                    if (orphanedIds.size() > 1) {
                        sqlString.append(", ");
                    }
                    sqlString.append(id);
                }
            }
        }
        sqlString.append(");");
        try {
            QueryResult result = (QueryResult) this.executeNativeSql(sqlString
                    .toString());
            for (int i = 0; i < result.getResultCount(); i++) {
                orphanedIds.remove((Integer) result.getRowColumnValue(i, 0));
            }
            if (!orphanedIds.isEmpty()) {
                sqlString = new StringBuilder(orphanedIds.size() * 15 + 60);
                sqlString.append("delete from awips.grid_info where id in (");
                boolean first = true;
                for (Integer id : orphanedIds) {
                    if (!first) {
                        sqlString.append(", ");
                    } else {
                        first = false;
                    }
                    sqlString.append(id);
                }
                sqlString.append(");");
                if (purgeModelCacheTopic != null) {
                    this.executeNativeSql(sqlString.toString());
                    EDEXUtil.getMessageProducer().sendAsyncUri(
                            purgeModelCacheTopic, orphanedIds);
                } else {
                    GridInfoCache.getInstance().purgeCache(
                            new ArrayList<Integer>(orphanedIds));
                    statusHandler
                            .warn("Unable to purge model cache of clustered edices");
                }
            }
        } catch (DataAccessLayerException e1) {
            statusHandler.error("Error purging orphaned grid info entries", e1);
        } catch (EdexException e) {
            statusHandler.error(
                    "Error sending message to purge grid info topic", e);
        }
    }

}
