package com.raytheon.uf.viz.preciprate;

import java.io.File;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.preciprate.PrecipRateRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStoredData;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.DHRValues;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetriever;
import com.raytheon.viz.radar.VizRadarRecord;

public class VizPrecipRateRadarRecord extends VizRadarRecord {

    private static final long serialVersionUID = 3718048640247491371L;

    private static class PrecipRateStoredData extends RadarStoredData {

        private Map<DHRValues, Double> dhrMap;

        public Map<DHRValues, Double> getDhrMap() {
            return dhrMap;
        }

        public void setDhrMap(Map<DHRValues, Double> dhrMap) {
            this.dhrMap = dhrMap;
        }

    }

    protected CacheObject<PrecipRateRecord, PrecipRateStoredData> cacheObject;

    public VizPrecipRateRadarRecord(PrecipRateRecord record) {
        super();
        this.setInsertTime(record.getInsertTime());
        this.setDataTime(record.getDataTime().clone());
        this.setIcao(record.getIcao());
        this.setLatitude(record.getLatitude());
        this.setLongitude(record.getLongitude());
        this.setNumLevels(256);
        this.setGateResolution(record.getGateResolution());
        this.setNumBins(record.getNumBins());
        this.setNumRadials(record.getNumRadials());
        this.setTrueElevationAngle(0.0f);
        this.setPrimaryElevationAngle(0.0);
        this.setElevation(0.0f);
        this.setFormat("Radial");
        this.setCRS(record.getCrs());
        this.setProductCode(32);
        this.setOperationalMode(2);
        this.setUnit(record.getParameterUnit());
        cacheObject = CacheObject.newCacheObject(record,
                new PrecipRateStoredDataRetriever());
    }

    @Override
    public DataTime getDataTime() {
        return cacheObject.getMetadata().getDataTime();
    }

    @Override
    public PrecipRateStoredData getStoredData() {
        return cacheObject.getObjectSync();
    }

    @Override
    public PrecipRateStoredData getStoredDataAsync() {
        return cacheObject.getObjectAsync();
    }

    public double getHailcap() {
        return cacheObject.getMetadata().getHailcap();
    }

    public Map<DHRValues, Double> getDhrMap() {
        return getStoredData().getDhrMap();
    }

    private static class PrecipRateStoredDataRetriever implements
            IObjectRetriever<PrecipRateRecord, PrecipRateStoredData> {

        @Override
        public PrecipRateStoredData retrieveObject(PrecipRateRecord metadata) {
            PrecipRateStoredData data = new PrecipRateStoredData();
            File loc = HDF5Util.findHDF5Location(metadata);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            metadata.retrieveFromDataStore(dataStore);
            data.setRawData(metadata.getRawData());
            data.setAngleData(metadata.getAngleData());
            data.setDhrMap(metadata.getDhrMap());
            metadata.setRawData(null);
            metadata.setAngleData(null);
            metadata.setDhrMap(null);
            return data;
        }

        @Override
        public int getSize(PrecipRateStoredData object) {
            int size = 0;
            if (object.getRawData() != null) {
                size += object.getRawData().length;
            }
            if (object.getAngleData() != null) {
                size += object.getAngleData().length * 4;
            }
            // TODO try to figure out the size of dhr map
            return size;
        }
    };

}
