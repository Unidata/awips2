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
package com.raytheon.viz.radar;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetrieverAndDisposer;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2011            mschenke     Initial creation
 * Mar 18, 2013 1804       bsteffen    Remove AlphanumericValues from radar
 *                                     HDF5.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RadarRecordDataRetriever implements
        IObjectRetrieverAndDisposer<RadarRecordMetadata, RadarRecord> {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.cache.GeneralCacheObject.IObjectRetriever#
     * retrieveObject()
     */
    @Override
    public RadarRecord retrieveObject(RadarRecordMetadata rrm) {
        RadarRecord record = rrm.record;
        File loc = HDF5Util.findHDF5Location(record);
        IDataStore dataStore = DataStoreFactory.getDataStore(loc);
        try {
            RadarDataRetriever.populateRadarRecord(dataStore, record);
        } catch (Exception e) {
            throw new RuntimeException("Error retrieving radar data: "
                    + e.getLocalizedMessage(), e);
        }
        return record;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.cache.GeneralCacheObject.IObjectRetriever#
     * disposeObject(java.lang.Object)
     */
    @Override
    public void disposeObject(RadarRecord object) {
        object.setRawData(null);
        object.setRawShortData(null);
        object.setAngleData(null);
        object.setGraphicBlock(null);
        object.setSymbologyBlock(null);
        object.setSymbologyData(null);
        object.setProductVals(null);
        object.setMapRecordVals(null);
        object.setGsmMessage(null);
        object.setStormIDs(null);
        object.setAapMessage(null);
        object.setThresholds(null);
        object.setProductDependentValues(null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.cache.GeneralCacheObject.IObjectRetriever#getSize
     * (java.lang.Object)
     */
    @Override
    public int getSize(RadarRecord object) {
        // Populate a list of used objects and serialize it for size
        int size = 1;
        List<Object> objs = new ArrayList<Object>(15);
        if (object.getRawData() != null) {
            objs.add(object.getRawData());
        }
        if (object.getRawShortData() != null) {
            objs.add(object.getRawShortData());
        }
        if (object.getAngleData() != null) {
            objs.add(object.getAngleData());
        }
        if (object.getGraphicBlock() != null) {
            objs.add(object.getGraphicBlock());
        }
        if (object.getSymbologyBlock() != null) {
            objs.add(object.getSymbologyBlock());
        }
        if (object.getSymbologyData() != null) {
            objs.add(object.getSymbologyBlock());
        }
        if (object.getMapProductVals() != null) {
            objs.add(object.getMapProductVals());
        }
        if (object.getMapRecordVals() != null) {
            objs.add(object.getMapRecordVals());
        }
        if (object.getGsmMessage() != null) {
            objs.add(object.getGsmMessage());
        }
        if (object.getStormIDs() != null) {
            objs.add(object.getStormIDs());
        }
        if (object.getAapMessage() != null) {
            objs.add(object.getAapMessage());
        }
        if (object.getThresholds() != null) {
            objs.add(object.getThresholds());
        }
        if (object.getProductDependentValues() != null) {
            objs.add(object.getProductDependentValues());
        }

        try {
            byte[] raw = SerializationUtil.transformToThrift(objs);
            size = raw.length;
        } catch (SerializationException e) {
            e.printStackTrace();
        }

        return size;
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        return true;
    }

}
