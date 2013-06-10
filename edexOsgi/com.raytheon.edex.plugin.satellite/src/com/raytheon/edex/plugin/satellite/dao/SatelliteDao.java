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

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.opengis.referencing.operation.TransformException;

import com.raytheon.edex.util.satellite.SatelliteCreatingEntity;
import com.raytheon.edex.util.satellite.SatellitePhysicalElement;
import com.raytheon.edex.util.satellite.SatellitePosition;
import com.raytheon.edex.util.satellite.SatelliteSectorId;
import com.raytheon.edex.util.satellite.SatelliteSource;
import com.raytheon.edex.util.satellite.SatelliteUnit;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteMessageData;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.interpolation.GridDownscaler;
import com.raytheon.uf.common.geospatial.interpolation.data.AbstractDataWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.ByteArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.DataDestination;
import com.raytheon.uf.common.geospatial.interpolation.data.ShortArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.UnsignedByteArrayWrapper;
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
 * - AWIPS2 Baseline Repository --------
 * 07/09/2012    798        jkorman     Modified datastore population.
 * 03/25/2013    1823       dgilling    Modified getSatelliteData() and 
 *                                      getSatelliteInventory() to allow optional
 *                                      input arguments.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SatelliteDao extends PluginDao {

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

    /**
     * Populated a given IDataStore object with the data record to be persisted.
     * 
     * @param dataStore
     *            Storage object to be populated.
     * @param record
     *            The persistable record containing the data to be persisted.
     * @return The populated data storage object.
     */
    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable record) throws StorageException {
        SatelliteRecord satRecord = (SatelliteRecord) record;

        IDataRecord storageRecord = (IDataRecord) satRecord.getMessageData();
        if (storageRecord != null) {
            StorageProperties props = new StorageProperties();
            String compression = PluginRegistry.getInstance()
                    .getRegisteredObject(pluginName).getCompression();
            if (compression != null) {
                props.setCompression(StorageProperties.Compression
                        .valueOf(compression));
            }
            props.setDownscaled(false);
            storageRecord.setProperties(props);
            storageRecord.setCorrelationObject(satRecord);
            // Store the base record.
            dataStore.addDataRecord(storageRecord);

            Map<String, Object> attributes = storageRecord.getDataAttributes();

            Float fillValue = getAttribute(attributes,
                    SatelliteRecord.SAT_FILL_VALUE, 0.0f);

            SatMapCoverage coverage = satRecord.getCoverage();
            AbstractDataWrapper dataSource = getSource(storageRecord,
                    coverage.getNx(), coverage.getNy());
            dataSource.setFillValue(fillValue);
            GridDownscaler downScaler = new GridDownscaler(
                    MapUtil.getGridGeometry(coverage));

            // How many interpolation levels do we need for this data?
            int levels = downScaler.getNumberOfDownscaleLevels();
            // set the number of levels in the 'parent' satellite data.
            // Subtract one for the base level data.
            satRecord.setInterpolationLevels(levels - 1);

            // How many interpolation levels do we need for this data? Includes
            // the base level!
            // Subtract one for the base level data.
            int downScaleLevels = downScaler.getNumberOfDownscaleLevels() - 1;
            // set the number of downscale levels in the satellite metadata.
            satRecord.setInterpolationLevels(downScaleLevels);
            if (DataStoreFactory.isInterpolated(levels)) {
                for (int level = 0; level < downScaleLevels; level++) {
                    int downScaleLevel = level + 1;
                    Rectangle size = downScaler
                            .getDownscaleSize(downScaleLevel);

                    AbstractDataWrapper dest = getDestination(storageRecord,
                            size);
                    dest.setFillValue(fillValue);
                    try {
                        // Downscale from previous level
                        downScaler.downscale(downScaleLevel - 1,
                                downScaleLevel, dataSource, dest);

                        IDataRecord dr = createDataRecord(satRecord, dest,
                                downScaleLevel, size);
                        // Set the attributes and properties from the parent
                        // data.
                        dr.setDataAttributes(attributes);
                        dr.setProperties(props);
                        dataStore.addDataRecord(dr);

                        // Set source to current level
                        dataSource = dest;
                    } catch (TransformException e) {
                        throw new StorageException(
                                "Error creating downscaled data",
                                storageRecord, e);
                    }
                }
            }

        }
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
            if (source != null) {
                query.addQueryParam("source", source);
            }
            if (creatingEntity != null) {
                query.addQueryParam("creatingEntity", creatingEntity);
            }
            if (sectorID != null) {
                query.addQueryParam("sectorID", sectorID);
            }
            if (physicalElement != null) {
                query.addQueryParam("physicalElement", physicalElement);
            }
            query.addQueryParam("dataTime.refTime", theDate);
            query.addOrder("dataTime.refTime", true);
            try {
                PluginDataObject[] pdos = this.getFullRecord(query, 0);
                for (int i = 0; i < pdos.length; i++) {
                    satRecords.add((SatelliteRecord) pdos[i]);
                    satRecords.get(i)
                            .setMessageData(
                                    ((IDataRecord[]) satRecords.get(i)
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
        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        if (source != null) {
            query.addQueryParam("source", source);
        }
        if (creatingEntity != null) {
            query.addQueryParam("creatingEntity", creatingEntity);
        }
        if (sectorID != null) {
            query.addQueryParam("sectorID", sectorID);
        }
        if (physicalElement != null) {
            query.addQueryParam("physicalElement", physicalElement);
        }
        query.addReturnedField("dataTime.refTime");
        query.addOrder("dataTime.refTime", true);

        @SuppressWarnings("unchecked")
        List<Date> times = (List<Date>) this.queryByCriteria(query);
        return new ArrayList<Date>(times);
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

    /**
     * Create an {@link AbstractDataWrapper} destination from the supplied
     * {@link IDataRecord} with given dimensions.
     * 
     * @param rec
     *            The record containing data to be wrapped.
     * @param size
     *            A {@link Rectangle} containing the size of the input data.
     * @return The wrapped data.
     */
    private AbstractDataWrapper getDestination(IDataRecord rec, Rectangle size) {
        AbstractDataWrapper dest = null;

        if (rec instanceof ByteDataRecord) {
            dest = new UnsignedByteArrayWrapper(size.width, size.height);
        } else if (rec instanceof ShortDataRecord) {
            dest = new ShortArrayWrapper(size.width, size.height);
        }
        return dest;
    }

    /**
     * Create an {@link AbstractDataWrapper} source from the supplied
     * {@link IDataRecord} with given dimensions.
     * 
     * @param rec
     *            The record containing data to be wrapped.
     * @param nx
     *            Number of items on the x axis.
     * @param ny
     *            Number of items on the y axis.
     * @return The wrapped data.
     */
    private AbstractDataWrapper getSource(IDataRecord rec, int nx, int ny) {
        AbstractDataWrapper source = null;

        if (rec instanceof ByteDataRecord) {
            byte[] b = ((ByteDataRecord) rec).getByteData();
            source = new UnsignedByteArrayWrapper(b, nx, ny);
        } else if (rec instanceof ShortDataRecord) {
            short[] s = ((ShortDataRecord) rec).getShortData();
            source = new ShortArrayWrapper(s, nx, ny);
        }
        return source;
    }

    /**
     * Create the {@link IDataRecord} from the {@link DataDestination} using the
     * original satellite data, size and
     * 
     * @param satRec
     *            The original satellite data record.
     * @param data
     *            The down-scaled data.
     * @param downscaledLevel
     *            The level identifier for this data.
     * @param size
     *            Size of the down-scaled data.
     * @return The created data record to be stored.
     */
    private IDataRecord createDataRecord(SatelliteRecord satRec,
            DataDestination data, int downscaleLevel, Rectangle size) {
        SatelliteMessageData msgData = null;
        Object o = null;
        if (data instanceof ByteArrayWrapper) {
            o = ((ByteArrayWrapper) data).getArray();
        } else if (data instanceof ShortArrayWrapper) {
            o = ((ShortArrayWrapper) data).getArray();
        }
        if (o != null) {
            msgData = new SatelliteMessageData(o, size.width, size.height);
        }
        IDataRecord rec = msgData.getStorageRecord(satRec,
                String.valueOf(downscaleLevel));
        rec.setCorrelationObject(satRec);
        rec.setGroup(DataStoreFactory.createGroupName(satRec.getDataURI(),
                SatelliteRecord.SAT_DATASET_NAME, true));

        return rec;
    }

    /**
     * Get the value of an named attribute.
     * 
     * @param attrs
     *            Attributes that contain the value.
     * @param attrName
     *            Name of the attribute.
     * @param defValue
     *            A default value.
     * @return
     */
    public static Float getAttribute(Map<String, Object> attrs,
            String attrName, Float defValue) {
        Float retValue = defValue;
        if ((attrs != null) && (attrName != null)) {
            retValue = (Float) attrs.get(attrName);
        }
        return retValue;
    }

}
