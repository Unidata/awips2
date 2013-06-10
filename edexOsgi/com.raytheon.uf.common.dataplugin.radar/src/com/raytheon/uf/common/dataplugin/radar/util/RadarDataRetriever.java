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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStoredData;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertAdaptationParameters;
import com.raytheon.uf.common.dataplugin.radar.level3.AlertMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.GSMBlock.GSMMessage;
import com.raytheon.uf.common.dataplugin.radar.level3.GraphicBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2011            mschenke     Initial creation
 * Mar 18, 2013 1804       bsteffen    Remove AlphanumericValues from radar
 *                                     HDF5.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RadarDataRetriever {

    public static void populateRadarRecord(IDataStore dataStore,
            RadarRecord radarRecord) throws StorageException,
            FileNotFoundException {
        getRadarStoredData(dataStore, radarRecord.getDataURI(),
                radarRecord.getStoredData());
        radarRecord.setThresholds(radarRecord.getThresholds());
    }

    @SuppressWarnings("unchecked")
    public static int getRadarStoredData(IDataStore dataStore, String dataURI,
            RadarStoredData radarData) throws FileNotFoundException,
            StorageException {
        int size = 0;
        for (IDataRecord record : dataStore.retrieve(dataURI)) {
            if (record == null || record.getName() == null) {
                continue;
            }
            size += record.getSizeInBytes();
            try {
                if (record.getName().equals(RadarStoredData.RAW_DATA_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    radarData.setRawData(byteData.getByteData());
                } else if (record.getName().equals(
                        RadarStoredData.SHORT_DATA_ID)) {
                    ShortDataRecord byteData = (ShortDataRecord) record;
                    radarData.setRawShortData(byteData.getShortData());
                } else if (record.getName().equals(
                        RadarStoredData.ANGLE_DATA_ID)) {
                    FloatDataRecord floatData = (FloatDataRecord) record;
                    radarData.setAngleData(floatData.getFloatData());
                } else if (record.getName().equals(
                        RadarStoredData.GRAPHIC_BLOCK_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData.setGraphicBlock((GraphicBlock) o);
                } else if (record.getName()
                        .equals(RadarStoredData.SYM_BLOCK_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData.setSymbologyBlock((SymbologyBlock) o);
                } else if (record.getName().equals(RadarStoredData.SYM_DATA_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData
                            .setSymbologyData((HashMap<RadarDataKey, RadarDataPoint>) o);
                } else if (record.getName().equals(
                        RadarStoredData.PRODUCT_VALS_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData
                            .setProductVals((HashMap<RadarConstants.MapValues, Map<String, Map<RadarConstants.MapValues, String>>>) o);
                } else if (record.getName().equals(
                        RadarStoredData.RECORD_VALS_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData
                            .setMapRecordVals((HashMap<RadarConstants.MapValues, Map<RadarConstants.MapValues, String>>) o);
                } else if (record.getName().equals(RadarStoredData.GSM_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData.setGsmMessage((GSMMessage) o);
                } else if (record.getName()
                        .equals(RadarStoredData.STORM_IDS_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData.setStormIDs((Map<String, RadarDataKey>) o);
                } else if (record.getName().equals(RadarStoredData.AAP_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData.setAapMessage((AlertAdaptationParameters) o);
                } else if (record.getName().equals(
                        RadarStoredData.THRESHOLDS_ID)) {
                    ShortDataRecord shortData = (ShortDataRecord) record;
                    radarData.setThresholds(shortData.getShortData());
                } else if (record.getName().equals(
                        RadarStoredData.DEPENDENT_VALS_ID)) {
                    ShortDataRecord depValData = (ShortDataRecord) record;
                    radarData.setProductDependentValues(depValData
                            .getShortData());
                } else if (record.getName().equals(
                        RadarStoredData.ALERT_MESSAGE_ID)) {
                    ByteDataRecord byteData = (ByteDataRecord) record;
                    ByteArrayInputStream bais = new ByteArrayInputStream(
                            byteData.getByteData());
                    Object o = DynamicSerializationManager.getManager(
                            SerializationType.Thrift).deserialize(bais);
                    radarData.setAlertMessage((AlertMessage) o);
                } else {
                    size -= record.getSizeInBytes();
                }
            } catch (SerializationException e) {
                throw new StorageException("Error Creating Objects", record, e);
            }
        }

        return size;
    }

}
