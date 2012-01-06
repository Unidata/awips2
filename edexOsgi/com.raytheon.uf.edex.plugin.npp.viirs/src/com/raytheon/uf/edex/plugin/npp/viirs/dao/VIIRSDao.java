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

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSSpatialRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.plugin.npp.viirs.VIIRSMessageData;
import com.raytheon.uf.edex.plugin.npp.viirs.VIIRSSpatialMessageData;

/**
 * VIIRSDao, creates storage records from PDOs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2011            mschenke     Initial creation
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
        if (obj instanceof VIIRSDataRecord) {
            populateDataStore(dataStore, (VIIRSDataRecord) obj);
        } else if (obj instanceof VIIRSSpatialRecord) {
            populateDataStore(dataStore, (VIIRSSpatialRecord) obj);
        }
        return dataStore;
    }

    private void populateDataStore(IDataStore dataStore, VIIRSDataRecord record)
            throws Exception {
        StorageProperties props = new StorageProperties();
        String compression = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName).getCompression();
        if (compression != null) {
            props.setCompression(StorageProperties.Compression
                    .valueOf(compression));
        }

        int levels = record.getLevels();
        int width = record.getWidth();
        int height = record.getHeight();
        for (int i = 0; i < levels; ++i) {
            VIIRSMessageData messageData = (VIIRSMessageData) record
                    .getMessageData();
            ShortDataRecord sdr = new ShortDataRecord(
                    VIIRSDataRecord.getDataSet(i), record.getDataURI(),
                    messageData.rawData, 2, new long[] { width, height });
            Map<String, Object> attributes = new HashMap<String, Object>();
            attributes.put(VIIRSDataRecord.MISSING_VALUE_ID,
                    messageData.missingValues);
            attributes.put(VIIRSDataRecord.OFFSET_ID, messageData.offset);
            attributes.put(VIIRSDataRecord.SCALE_ID, messageData.scale);
            sdr.setDataAttributes(attributes);
            sdr.setProperties(props);
            sdr.setCorrelationObject(record);
            dataStore.addDataRecord(sdr);
            // TODO: Handle scaling here
            break;
        }
    }

    private void populateDataStore(IDataStore dataStore,
            VIIRSSpatialRecord record) throws Exception {
        StorageProperties props = new StorageProperties();
        String compression = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName).getCompression();
        if (compression != null) {
            props.setCompression(StorageProperties.Compression
                    .valueOf(compression));
        }

        int levels = record.getLevels();
        int width = record.getWidth();
        int height = record.getHeight();
        for (int i = 0; i < levels; ++i) {
            long[] sizes = new long[] { width, height };
            VIIRSSpatialMessageData messageData = (VIIRSSpatialMessageData) record
                    .getMessageData();
            FloatDataRecord lats = new FloatDataRecord(
                    VIIRSSpatialRecord.getLatitudeDataSet(i),
                    record.getDataURI(), messageData.latitudes, sizes.length,
                    sizes);
            FloatDataRecord lons = new FloatDataRecord(
                    VIIRSSpatialRecord.getLongitudeDataSet(i),
                    record.getDataURI(), messageData.longitudes, sizes.length,
                    sizes);
            // Create record attributes
            Map<String, Object> attributes = new HashMap<String, Object>();
            attributes.put(VIIRSDataRecord.MISSING_VALUE_ID,
                    messageData.missingValues);
            attributes.put(VIIRSSpatialRecord.VALID_HEIGHT_ID,
                    messageData.validHeight);

            // Set on records
            lats.setDataAttributes(attributes);
            lats.setProperties(props);
            lats.setCorrelationObject(record);
            lons.setDataAttributes(attributes);
            lons.setProperties(props);
            lons.setCorrelationObject(record);

            // Add records
            dataStore.addDataRecord(lats);
            dataStore.addDataRecord(lons);
            // TODO: Handle scaling here
            break;
        }
    }
}
