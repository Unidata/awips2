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
 * Feb 06, 2009 1990       bphillip    Initial creation
 * Mar 18, 2013 1804       bsteffen    Reduce useless data stored in radar hdf5
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Aug 14, 2014 3393       nabowle     Remove broken getFullRecord override.
 * Aug 19, 2014 3393       nabowle     Default constructor.
 *
 * </pre>
 *
 * @author bphillip
 * @version 1.0
 */
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStoredData;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.plugin.PluginDao;

public class RadarDao extends PluginDao {

    /**
     * Creates a new radar dao. Equivalent to RadarDao("radar").
     *
     * @throws PluginException
     *             If the dao cannot be initialized
     */
    public RadarDao() throws PluginException {
        super("radar");
    }

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

        if ((radarRec.getThresholds() != null)
                && (radarRec.getProductCode() != 2)) {
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

        Map<RadarDataKey, RadarDataPoint> symData = radarRec.getSymbologyData();
        if ((symData != null) && !symData.isEmpty()) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(symData);
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

        Map<MapValues, Map<String, Map<MapValues, String>>> mapProdVals = radarRec
                .getMapProductVals();
        if ((mapProdVals != null) && !mapProdVals.isEmpty()) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(mapProdVals);
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.PRODUCT_VALS_ID, radarRec.getDataURI(),
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

        Map<MapValues, Map<MapValues, String>> mapRecVals = radarRec
                .getMapRecordVals();
        if ((mapRecVals != null) && !mapRecVals.isEmpty()) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(mapRecVals);
            ByteDataRecord bdr = new ByteDataRecord(
                    RadarStoredData.RECORD_VALS_ID, radarRec.getDataURI(), data);
            bdr.setCorrelationObject(radarRec);
            dataStore.addDataRecord(bdr, sp);
        }

        Map<String, RadarDataKey> stormIds = radarRec.getStormIDs();
        if ((stormIds != null) && !stormIds.isEmpty()) {
            byte[] data = DynamicSerializationManager.getManager(
                    SerializationType.Thrift).serialize(stormIds);
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


    public void populateData(RadarRecord record) throws Exception {
        RadarDataRetriever.populateRadarRecord(getDataStore(record), record);
    }

}
