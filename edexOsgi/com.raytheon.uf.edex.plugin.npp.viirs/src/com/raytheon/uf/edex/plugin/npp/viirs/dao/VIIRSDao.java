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
package com.raytheon.uf.edex.plugin.npp.viirs.dao;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSSpatialCoverage;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.interpolation.BilinearInterpolation;
import com.raytheon.uf.common.geospatial.interpolation.GridDownscaler;
import com.raytheon.uf.common.geospatial.interpolation.data.AbstractDataWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.UnsignedShortArrayWrapper;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.npp.viirs.VIIRSMessageData;

/**
 * VIIRSDao, creates storage records from PDOs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2011             mschenke     Initial creation
 * Feb 21, 2012 #30        mschenke     Updated code to account for missingValue 
 *                                      in messageData being float now
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSDao extends PluginDao {

    /**
     * @param pluginName
     * @throws PluginException
     */
    public VIIRSDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * Looks up an existing {@link VIIRSSpatialCoverage} that matches the time
     * and resolution, will return null if none found
     * 
     * @param dataTime
     * @param resolution
     * @return
     */
    public VIIRSSpatialCoverage lookupCoverage(DataTime dataTime,
            float resolution) {
        VIIRSSpatialCoverage coverage = null;
        DatabaseQuery query = new DatabaseQuery(VIIRSSpatialCoverage.class);
        query.addQueryParam("dataTime.refTime", dataTime.getRefTime());
        query.addQueryParam("dx", resolution);
        query.addQueryParam("dy", resolution);
        try {
            List<?> results = queryByCriteria(query);
            if (results != null) {
                for (Object obj : results) {
                    if (obj instanceof VIIRSSpatialCoverage) {
                        coverage = (VIIRSSpatialCoverage) obj;
                        break;
                    } else {
                        statusHandler.handle(Priority.WARN,
                                "VIIRSSpatialCoverage lookup returned type of: "
                                        + obj != null ? obj.getClass()
                                        .getSimpleName() : null);
                    }
                }
            }
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error querying for viirs spatial object", e);
        }
        return coverage;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.database.plugin.PluginDao#populateDataStore(com.
     * raytheon.uf.common.datastorage.IDataStore,
     * com.raytheon.uf.common.dataplugin.persist.IPersistable)
     */
    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        VIIRSDataRecord record = (VIIRSDataRecord) obj;
        VIIRSSpatialCoverage spatialRecord = record.getCoverage();
        int nx = spatialRecord.getNx();
        int ny = spatialRecord.getNy();

        StorageProperties props = new StorageProperties();
        String compression = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName).getCompression();
        if (compression != null) {
            props.setCompression(StorageProperties.Compression
                    .valueOf(compression));
        }

        VIIRSMessageData messageData = (VIIRSMessageData) record
                .getMessageData();
        float[] missingValues = messageData.getMissingValues();
        float fillValue = missingValues[0];
        Object rawData = messageData.getRawData();
        AbstractDataWrapper ds = null;
        // Data sources are create anonymously here to avoid having the
        // fillValue/validMin/validMax even checked when getting values but
        // still getting the getDataValueInternal functionality
        if (rawData instanceof short[]) {
            ds = new UnsignedShortArrayWrapper((short[]) rawData, nx, ny);
        } else if (rawData instanceof float[]) {
            ds = new FloatArrayWrapper((float[]) rawData, nx, ny);
        }

        if (ds != null) {
            ds.setFillValue(fillValue);

            // Wrap the source and replace set each value which will replace
            // anything in missingValues with fillValue
            DataSource source = new VIIRSDataSourceWrapper(ds, missingValues);
            for (int y = 0; y < ny; ++y) {
                for (int x = 0; x < nx; ++x) {
                    ds.setDataValue(source.getDataValue(x, y), x, y);
                }
            }

            GridDownscaler downscaler = new GridDownscaler(
                    spatialRecord.getGridGeometry(),
                    new BilinearInterpolation());

            int levels = downscaler.getNumberOfDownscaleLevels();
            for (int i = 0; i < levels; ++i) {
                Rectangle bounds = downscaler.getDownscaleSize(i);
                AbstractDataWrapper dd = null;

                if (i == 0) {
                    // No interpolation needed for level 0
                    dd = ds;
                } else {
                    if (ds instanceof UnsignedShortArrayWrapper) {
                        dd = new UnsignedShortArrayWrapper(bounds.width,
                                bounds.height);
                    } else if (ds instanceof FloatArrayWrapper) {
                        dd = new FloatArrayWrapper(bounds.width, bounds.height);
                    }

                    dd.setFillValue(fillValue);
                    downscaler.downscale(i - 1, i, ds, dd);
                    ds = dd;
                }

                IDataRecord idr = null;
                if (dd instanceof UnsignedShortArrayWrapper) {
                    idr = new ShortDataRecord(VIIRSDataRecord.getDataSet(i),
                            record.getDataURI(),
                            ((UnsignedShortArrayWrapper) dd).getArray(), 2,
                            new long[] { bounds.width, bounds.height });
                } else if (dd instanceof FloatArrayWrapper) {
                    idr = new FloatDataRecord(VIIRSDataRecord.getDataSet(i),
                            record.getDataURI(),
                            ((FloatArrayWrapper) dd).getArray(), 2, new long[] {
                                    bounds.width, bounds.height });
                }

                Map<String, Object> attributes = new HashMap<String, Object>();
                attributes.put(VIIRSDataRecord.MISSING_VALUE_ID, fillValue);
                attributes.put(VIIRSDataRecord.OFFSET_ID,
                        messageData.getOffset());
                attributes
                        .put(VIIRSDataRecord.SCALE_ID, messageData.getScale());
                if (messageData.getUnitString() != null) {
                    attributes.put(VIIRSDataRecord.UNIT_ID,
                            messageData.getUnitString());
                }
                idr.setDataAttributes(attributes);
                idr.setProperties(props);
                idr.setCorrelationObject(record);
                dataStore.addDataRecord(idr);
                dataStore.store();
            }
        } else {
            throw new Exception("Unrecognized type for rawData: "
                    + (rawData != null ? rawData.getClass() : null));
        }
        return dataStore;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.database.plugin.PluginDao#purgeAllData()
     */
    @Override
    public void purgeAllData() throws PluginException {
        super.purgeAllData();
        purgeSpatialData(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.database.plugin.PluginDao#purgeExpiredData()
     */
    @Override
    public void purgeExpiredData() throws PluginException {
        super.purgeExpiredData();
        purgeSpatialData(false);
    }

    /**
     * Deletes all viirs_spatial metadata which doesn't have spatial data and
     * older than 4 hours if all == false
     */
    private void purgeSpatialData(boolean all) {
        List<Object> args = new ArrayList<Object>(3);
        args.add(VIIRSSpatialCoverage.class.getAnnotation(Table.class).name());
        args.add(VIIRSDataRecord.class.getAnnotation(Table.class).name());
        String formatString = "delete from %s where gid not in (select distinct coverage_gid from %s)";
        if (all == false) {
            formatString += " and reftime < '%s'";
            Calendar cal = Calendar.getInstance();
            cal.add(Calendar.HOUR, -4);
            args.add(new DataTime(cal.getTime()));
        }

        this.executeSQLUpdate(String.format(formatString,
                args.toArray(new Object[args.size()])));
    }

}
