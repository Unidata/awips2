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

package com.raytheon.edex.plugin.radar.dao;

/**
 * Data Access Object implementation for accessing radar data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/06/09     1990       bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStoredData;
import com.raytheon.uf.common.dataplugin.radar.level3.GSMBlock.GSMMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

public class RadarDao extends PluginDao {

    /**
     * Creates a new radar dao
     * 
     * @param pluginName
     *            "radar"
     * @throws PluginException
     *             If the dao cannot be initialized
     */
    public RadarDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        RadarRecord radarRec = (RadarRecord) obj;
        StorageProperties sp = null;
        String compression = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName).getCompression();
        if (compression != null) {
            sp = new StorageProperties();
            sp.setCompression(StorageProperties.Compression
                    .valueOf(compression));
        }
        if (radarRec.getRawData() != null) {
            IDataRecord rec = new ByteDataRecord(RadarStoredData.RAW_DATA_ID,
                    radarRec.getDataURI(), radarRec.getRawData(), 2,
                    new long[] { radarRec.getNumRadials(),
                            radarRec.getNumBins() });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, sp);
        }

        if (radarRec.getRawShortData() != null) {
            IDataRecord rec = new ShortDataRecord(
                    RadarStoredData.SHORT_DATA_ID, radarRec.getDataURI(),
                    radarRec.getRawShortData(), 2, new long[] {
                            radarRec.getNumRadials(), radarRec.getNumBins() });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, sp);
        }

        if (radarRec.getAngleData() != null) {
            IDataRecord rec = new FloatDataRecord(
                    RadarStoredData.ANGLE_DATA_ID, radarRec.getDataURI(),
                    radarRec.getAngleData(), 1,
                    new long[] { radarRec.getNumRadials() });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, sp);
        }

        if (radarRec.getThresholds() != null && radarRec.getProductCode() != 2) {
            IDataRecord rec = new ShortDataRecord(
                    RadarStoredData.THRESHOLDS_ID, radarRec.getDataURI(),
                    radarRec.getThresholds(), 1, new long[] { 16 });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, sp);
        }

        if (radarRec.getSymbologyBlock() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getSymbologyBlock());
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.SYM_BLOCK_ID, radarRec.getDataURI(), data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getSymbologyData() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getSymbologyData());
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.SYM_DATA_ID, radarRec.getDataURI(), data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getGraphicBlock() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getGraphicBlock());
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.GRAPHIC_BLOCK_ID, radarRec.getDataURI(),
                    data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getMapProductVals() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getMapProductVals());
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.PRODUCT_VALS_ID, radarRec.getDataURI(),
                    data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getAlphanumericValues() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getAlphanumericValues());
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.ALPHANUMERIC_ID, radarRec.getDataURI(),
                    data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getTabularBlock() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getTabularBlock());
            ByteDataRecord bdr = new ByteDataRecord(RadarStoredData.TABULAR_ID,
                    radarRec.getDataURI(), data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getProductDependentValues() != null) {
            IDataRecord rec = new ShortDataRecord(
                    RadarStoredData.DEPENDENT_VALS_ID, radarRec.getDataURI(),
                    radarRec.getProductDependentValues(), 1,
                    new long[] { radarRec.getProductDependentValues().length });
            rec.setCorrelationObject(radarRec);
            dataStore.addDataRecord(rec, sp);
        }

        if (radarRec.getMapRecordVals() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getMapRecordVals());
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.RECORD_VALS_ID, radarRec.getDataURI(), data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getStormIDs() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(radarRec.getStormIDs());
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.STORM_IDS_ID, radarRec.getDataURI(), data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getGsmMessage() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getGsmMessage());
            ByteDataRecord bdr = new ByteDataRecord(RadarStoredData.GSM_ID,
                    radarRec.getDataURI(), data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }
        if (radarRec.getAlertMessage() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getAlertMessage());
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.ALERT_MESSAGE_ID, radarRec.getDataURI(),
                    data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        if (radarRec.getAapMessage() != null) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(
                    radarRec.getAapMessage());
            ByteDataRecord bdr = new ByteDataRecord(RadarStoredData.AAP_ID,
                    radarRec.getDataURI(), data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        return dataStore;
    }

    @Override
    public List<IDataRecord[]> getHDF5Data(List<PluginDataObject> objects,
            int tileSet) throws PluginException {
        List<IDataRecord[]> retVal = new ArrayList<IDataRecord[]>();

        for (PluginDataObject obj : objects) {
            IDataRecord[] record = null;

            if (obj instanceof IPersistable) {
                /* connect to the data store and retrieve the data */
                try {
                    record = getDataStore((IPersistable) obj).retrieve(
                            obj.getDataURI());
                } catch (Exception e) {
                    throw new PluginException(
                            "Error retrieving radar HDF5 data", e);
                }
                retVal.add(record);
            }
        }

        return retVal;
    }

    @Override
    public PluginDataObject[] getFullRecord(DatabaseQuery query, int tile)
            throws PluginException {
        PluginDataObject[] queryResults = getMetadata(query);
        for (PluginDataObject obj : queryResults) {
            RadarRecord record = (RadarRecord) obj;
            record.setPluginName(pluginName);
            IDataRecord[] hdf5Data = getHDF5Data(record, tile);
            record.setMessageData(hdf5Data[0].getDataObject());
            record.setAngleData((float[]) hdf5Data[1].getDataObject());
            record.setThresholds((short[]) hdf5Data[2].getDataObject());
            record.setProductDependentValues((short[]) hdf5Data[8]
                    .getDataObject());

            record.setProductVals((HashMap<RadarConstants.MapValues, Map<String, Map<RadarConstants.MapValues, String>>>) hdf5Data[5]
                    .getDataObject());
            record.setMapRecordVals((HashMap<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>>) hdf5Data[6]);
            record.setGsmMessage((GSMMessage) hdf5Data[7].getDataObject());
            try {
                record.setSymbologyBlock((SymbologyBlock) SerializationUtil
                        .transformFromThrift((byte[]) hdf5Data[3]
                                .getDataObject()));
                record.setGraphicBlock((GraphicBlock) SerializationUtil
                        .transformFromThrift((byte[]) hdf5Data[4]
                                .getDataObject()));

            } catch (SerializationException e) {
                throw new PluginException(
                        "Error deserializing symbology block", e);
            }
        }
        return queryResults;
    }

    public void populateData(RadarRecord record) throws Exception {
        RadarDataRetriever.populateRadarRecord(getDataStore(record), record);
    }

}
