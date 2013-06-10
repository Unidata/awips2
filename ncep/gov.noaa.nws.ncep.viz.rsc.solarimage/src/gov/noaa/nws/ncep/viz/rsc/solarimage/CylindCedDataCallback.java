package gov.noaa.nws.ncep.viz.rsc.solarimage;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageData;

import java.nio.FloatBuffer;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Callback Linear for Cylindrical Display
 * 
 * <pre>
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * 04/01/2013    958         qzhou       Initial Creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class CylindCedDataCallback implements IColorMapDataRetrievalCallback {

    protected SolarImageRecord record;

    private final ImageData imgData;

    /**
     * @param record
     * @throws VizException
     */
    public CylindCedDataCallback(SolarImageRecord record, ImageData imgData)
            throws VizException {
        this.record = record;
        this.imgData = imgData;
    }

    @Override
    public ColorMapData getColorMapData() throws VizException {
        // System.out.println("Retrieving solarimage data from HDF5...");

        int[] dimensions = new int[] { 360, 180 }; // imgData.getNx(),
                                                   // imgData.getNy() };//new
                                                   // dimensions
        FloatBuffer buffer = FloatBuffer.wrap(imgData.getImageValues());// new
                                                                        // imageData

        return new ColorMapData(buffer, dimensions, ColorMapDataType.FLOAT);

    }

    public ImageData getImageData() {
        return imgData;
    }

    public double getOriginalValue(double val) {
        return val;
    }

    // @Override
    // public int hashCode() {
    // final int prime = 31;
    // int result = 1;
    // result = prime
    // * result
    // + ((record.getDataURI() == null) ? 0 : record.getDataURI()
    // .hashCode());
    // result = prime * result + this.getClass().getCanonicalName().hashCode();
    // return result;
    // }
    //
    // @Override
    // public boolean equals(Object obj) {
    // if (this == obj)
    // return true;
    // if (obj == null)
    // return false;
    // if (getClass() != obj.getClass())
    // return false;
    // CylindCarrDataCallback other = (CylindCarrDataCallback) obj;
    // if (record.getDataURI() == null) {
    // if (other.record.getDataURI() != null)
    // return false;
    // } else if (!record.getDataURI().equals(other.record.getDataURI()))
    // return false;
    // return true;
    // }

}
