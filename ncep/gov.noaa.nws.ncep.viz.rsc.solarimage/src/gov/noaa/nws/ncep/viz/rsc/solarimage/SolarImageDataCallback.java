package gov.noaa.nws.ncep.viz.rsc.solarimage;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageData;

import java.io.File;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.exception.VizException;

public class SolarImageDataCallback implements IColorMapDataRetrievalCallback {

    protected SolarImageRecord record;
    
    private ImageData imgData;

    /**
     * @param record
     * @throws VizException 
     */
    public SolarImageDataCallback(SolarImageRecord record) throws VizException {
        this.record = record;
        this.imgData = new ImageData(record);
    }

    @Override
    public ColorMapData getColorMapData() throws VizException {
        //System.out.println("Retrieving solarimage data from HDF5...");

        int[] dimensions = new int[] { imgData.getNx(), imgData.getNy() };
        FloatBuffer buffer = FloatBuffer.wrap(imgData.getImageValues());

        return new ColorMapData(buffer, dimensions, ColorMapDataType.FLOAT);
    }

    protected float[] getRawData() { 
    	float[] values = null;
        byte[] rawData = null;
        File loc = HDF5Util.findHDF5Location(record);
        IDataStore dataStore = DataStoreFactory.getDataStore(loc);
        
        try {
            IDataRecord[] dataRecs = dataStore.retrieve(record.getDataURI());
            for (IDataRecord rec : dataRecs) {
            	
                if (rec.getName().equals(SolarImageRecord.RAW_DATA)
                        && rec instanceof ByteDataRecord) {
                	rawData = (((ByteDataRecord) rec).getByteData());
                }
            }

        } catch (Exception se) {
            se.printStackTrace();
        }

        return values;
    }

    public double getOriginalValue(double val) {
        return val;
    }
    
    public ImageData getImageData() {
        return imgData;
    }
    
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime
                * result
                + ((record.getDataURI() == null) ? 0 : record.getDataURI()
                        .hashCode());
        result = prime * result + this.getClass().getCanonicalName().hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SolarImageDataCallback other = (SolarImageDataCallback) obj;
        if (record.getDataURI() == null) {
            if (other.record.getDataURI() != null)
                return false;
        } else if (!record.getDataURI().equals(other.record.getDataURI()))
            return false;
        return true;
    }

}
