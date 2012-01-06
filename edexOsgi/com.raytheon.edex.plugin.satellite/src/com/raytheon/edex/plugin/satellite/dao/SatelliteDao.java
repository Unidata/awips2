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
package com.raytheon.edex.plugin.satellite.dao;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.edex.util.satellite.SatelliteCreatingEntity;
import com.raytheon.edex.util.satellite.SatellitePhysicalElement;
import com.raytheon.edex.util.satellite.SatellitePosition;
import com.raytheon.edex.util.satellite.SatelliteSectorId;
import com.raytheon.edex.util.satellite.SatelliteSource;
import com.raytheon.edex.util.satellite.SatelliteUnit;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data access object used for retrieving satellite data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2009            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SatelliteDao extends PluginDao {

    /**
     * Database query used for retrieving the inventory of data based on the
     * source, creating entity, sector id, and physical element
     */
    private static final String INVENTORY_QUERY = "select reftime from awips.satellite where source='%s' and creatingentity='%s' and sectorid='%s' and physicalElement='%s' order by reftime asc";

    /** The creating entity data access object */
    private SatelliteCreatingEntityDao creatingEntityDao = new SatelliteCreatingEntityDao();

    /** The physical element data access object */
    private SatellitePhysicalElementDao physicalElementDao = new SatellitePhysicalElementDao();

    /** The sector ID data access object */
    private SatelliteSectorIdDao sectorIdDao = new SatelliteSectorIdDao();

    /** The source data access object */
    private SatelliteSourceDao sourceDao = new SatelliteSourceDao();

    /** The satellite unit data access object */
    private SatelliteUnitDao unitDao = new SatelliteUnitDao();

    /** The satellite position data access object */
    private SatellitePositionDao positionDao = new SatellitePositionDao();

    /**
     * Creates a new satellite data access object
     * 
     * @param pluginName
     *            The plugin name. Should be "satellite"
     * @throws PluginException
     *             If a data access object cannot be created
     */
    public SatelliteDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable record) throws StorageException {
        SatelliteRecord satRecord = (SatelliteRecord) record;
        AbstractStorageRecord storageRecord = null;

        long nx = satRecord.getCoverage().getNx();
        long ny = satRecord.getCoverage().getNy();

        long[] sizes = new long[] { nx, ny };
        storageRecord = new ByteDataRecord("Data", satRecord.getDataURI(),
                (byte[]) satRecord.getMessageData(), 2, sizes);

        StorageProperties props = new StorageProperties();
        String compression = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName).getCompression();
        if (compression != null) {
            props.setCompression(StorageProperties.Compression
                    .valueOf(compression));
        }
        props.setDownscaled(true);

        storageRecord.setProperties(props);
        storageRecord.setCorrelationObject(satRecord);
        dataStore.addDataRecord(storageRecord);
        return dataStore;
    }

    /**
     * Retrieves fully populated SatelliteRecords using the provided criteria
     * 
     * @param source
     *            The source of the satellite data
     * @param creatingEntity
     *            The creating entity of the satellite data
     * @param sectorID
     *            The sector ID of the satellite data
     * @param physicalElement
     *            The physical element of the satellite data
     * @param dates
     *            The dates to retrieve data for
     * @return A list of SatelliteRecords corresponding to the provided criteria
     * @throws DataAccessLayerException
     *             If errors occur while retrieving the data
     */
    public List<SatelliteRecord> getSatelliteData(String source,
            String creatingEntity, String sectorID, String physicalElement,
            List<Date> dates) throws DataAccessLayerException {

        List<SatelliteRecord> satRecords = new ArrayList<SatelliteRecord>();

        List<Date> inventory = getSatelliteInventory(source, creatingEntity,
                sectorID, physicalElement);

        for (Date theDate : dates) {
            if (!inventory.contains(theDate)) {
                System.out.println("Not Found: " + theDate);
                continue;
            }
            DatabaseQuery query = new DatabaseQuery(SatelliteRecord.class);
            query.addQueryParam("source", source);
            query.addQueryParam("creatingEntity", creatingEntity);
            query.addQueryParam("sectorID", sectorID);
            query.addQueryParam("physicalElement", physicalElement);
            query.addQueryParam("dataTime.refTime", theDate);
            query.addOrder("dataTime.refTime", true);
            try {
                PluginDataObject[] pdos = this.getFullRecord(query, -1);
                for (int i = 0; i < pdos.length; i++) {
                    satRecords.add((SatelliteRecord) pdos[i]);
                    satRecords.get(i).setMessageData(
                            (ByteDataRecord) ((IDataRecord[]) satRecords.get(i)
                                    .getMessageData())[0]);
                }
            } catch (Exception e) {
                throw new DataAccessLayerException(
                        "Error retrieving satellite data!", e);
            }
        }
        return satRecords;
    }

    /**
     * Gets the inventory of satellite data contained in the data repository for
     * the given criteria
     * 
     * @param source
     *            The source of the satellite data
     * @param creatingEntity
     *            The creating entity of the satellite data
     * @param sectorID
     *            The sector ID of the satellite data
     * @param physicalElement
     *            The physical element of the satellite data
     * @return A List of Dates desribing the inventory
     * @throws DataAccessLayerException
     *             If errors occur while querying the data repository
     */
    public List<Date> getSatelliteInventory(String source,
            String creatingEntity, String sectorID, String physicalElement)
            throws DataAccessLayerException {
        QueryResult result = (QueryResult) this.executeNativeSql(String.format(
                INVENTORY_QUERY, source, creatingEntity, sectorID,
                physicalElement));
        List<Date> inventory = new ArrayList<Date>();
        if (result.getResultCount() > 0) {
            for (int i = 0; i < result.getResultCount(); i++) {
                inventory.add((Date) result.getRowColumnValue(i, 0));
            }
        }
        return inventory;
    }

    /**
     * Gets a SatelliteCreatingEntity with the given id
     * 
     * @param entityId
     *            The entity id
     * @return The SatelliteCreatingEntity with the given id
     */
    public SatelliteCreatingEntity getCreatingEntity(int entityId) {
        return creatingEntityDao.queryById(entityId);
    }

    /**
     * Gets a SatellitePhysicalElement with the given id
     * 
     * @param elementId
     *            The physical element id
     * @return The SatellitePhysicalElement with the given id
     */
    public SatellitePhysicalElement getPhysicalElement(int elementId) {
        return physicalElementDao.queryById(elementId);
    }

    /**
     * Gets a SatelliteSectorId with the given id
     * 
     * @param sectorId
     *            The sector id
     * @return The SatelliteSectorId with the given id
     */
    public SatelliteSectorId getSectorId(int sectorId) {
        return sectorIdDao.queryById(sectorId);
    }

    /**
     * Gest a SatelliteSource with the given id
     * 
     * @param sourceId
     *            The source id
     * @return The SatelliteSource with the given id
     */
    public SatelliteSource getSource(int sourceId) {
        return sourceDao.queryById(sourceId);
    }

    /**
     * Gets a SatelliteUnit with the given id
     * 
     * @param unitId
     *            The unit id
     * @return The SatelliteUnit with the given id
     */
    public SatelliteUnit getUnit(int unitId) {
        return unitDao.queryById(unitId);
    }

    /**
     * Gets a SatellitePosition object for the given satellite name
     * 
     * @param satelliteName
     *            The satellite name to get the SatellitePosition for
     * @return The SatellitePosition for this given satellite name
     */
    public SatellitePosition getSatellitePosition(String satelliteName) {
        return positionDao.queryById(satelliteName);
    }

    public SatelliteCreatingEntityDao getCreatingEntityDao() {
        return creatingEntityDao;
    }

    public void setCreatingEntityDao(
            SatelliteCreatingEntityDao creatingEntityDao) {
        this.creatingEntityDao = creatingEntityDao;
    }

    public SatellitePhysicalElementDao getPhysicalElementDao() {
        return physicalElementDao;
    }

    public void setPhysicalElementDao(
            SatellitePhysicalElementDao physicalElementDao) {
        this.physicalElementDao = physicalElementDao;
    }

    public SatelliteSectorIdDao getSectorIdDao() {
        return sectorIdDao;
    }

    public void setSectorIdDao(SatelliteSectorIdDao sectorIdDao) {
        this.sectorIdDao = sectorIdDao;
    }

    public SatelliteSourceDao getSourceDao() {
        return sourceDao;
    }

    public void setSourceDao(SatelliteSourceDao sourceDao) {
        this.sourceDao = sourceDao;
    }

    public SatelliteUnitDao getUnitDao() {
        return unitDao;
    }

    public void setUnitDao(SatelliteUnitDao unitDao) {
        this.unitDao = unitDao;
    }

    public SatellitePositionDao getPositionDao() {
        return positionDao;
    }

    public void setPositionDao(SatellitePositionDao positionDao) {
        this.positionDao = positionDao;
    }

}
