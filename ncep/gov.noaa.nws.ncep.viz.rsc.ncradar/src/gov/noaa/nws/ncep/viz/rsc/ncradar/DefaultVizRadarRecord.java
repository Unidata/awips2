package gov.noaa.nws.ncep.viz.rsc.ncradar;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Map;
import java.util.WeakHashMap;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStoredData;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetriever;

public class DefaultVizRadarRecord extends VizRadarRecord {

    private static final long serialVersionUID = -6878454982599668417L;

    protected CacheObject<RadarRecord, RadarStoredData> cacheObject;

    public DefaultVizRadarRecord(RadarRecord record) {
        super(record);
        cacheObject = CacheObject.newCacheObject(record,
                new RadarStoredDataRetriever());
    }

    @Override
    public RadarStoredData getStoredData() {
        return cacheObject.getObjectSync();
    }

    @Override
    public RadarStoredData getStoredDataAsync() {
        return cacheObject.getObjectAsync();
    }

    private static class RadarStoredDataRetriever implements
            IObjectRetriever<RadarRecord, RadarStoredData> {

        private Map<RadarStoredData, Integer> sizes = new WeakHashMap<RadarStoredData, Integer>();

        @Override
        public RadarStoredData retrieveObject(RadarRecord metadata) {
            RadarStoredData radarData = new RadarStoredData();
            File loc = HDF5Util.findHDF5Location(metadata);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            int size;
            try {
                size = com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever
                        .getRadarStoredData(dataStore, metadata.getDataURI(),
                                radarData);
            } catch (FileNotFoundException e) {
                throw new RuntimeException(e);
            } catch (StorageException e) {
                throw new RuntimeException(e);
            }
            sizes.put(radarData, size);
            return radarData;
        }

        @Override
        public int getSize(RadarStoredData object) {
            Integer size = sizes.get(object);
            if (size == null) {
                size = 0;
                if (object.getRawData() != null) {
                    size += object.getRawData().length;
                }
                if (object.getRawShortData() != null) {
                    size += object.getRawData().length * 2;
                }
                if (object.getAngleData() != null) {
                    size += object.getAngleData().length * 4;
                }
                if (object.getThresholds() != null) {
                    size += object.getThresholds().length * 2;
                }
                // TODO try to figure out the size of some of the other data.
            }
            return size;
        }

        @Override
        public int hashCode() {
            return RadarStoredDataRetriever.class.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof RadarStoredDataRetriever;
        }

    }

}
