package gov.noaa.nws.ncep.viz.rsc.solarimage;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageData;

import java.nio.FloatBuffer;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Callback for Cylindrical Display
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
public class LogCylindCedDataCallback extends CylindCedDataCallback {

    /**
     * @param record
     * @throws VizException
     */
    public LogCylindCedDataCallback(SolarImageRecord record, ImageData imgData)
            throws VizException {
        super(record, imgData);
    }

    @Override
    public ColorMapData getColorMapData() throws VizException {

        // int[] dimensions = new int[] { getImageData().getNx(),
        // getImageData().getNy()};
        int[] dimensions = new int[] { 360, 180 };

        float[] vals = getImageData().getImageValues();
        float[] logVals = new float[vals.length];

        for (int n = 0; n < vals.length; n++) {
            if (vals[n] <= 0) {
                logVals[n] = Float.NEGATIVE_INFINITY;
            } else {
                logVals[n] = (float) (Math.log10(vals[n]));
            }
        }

        FloatBuffer buffer = FloatBuffer.wrap(logVals);

        return new ColorMapData(buffer, dimensions, ColorMapDataType.FLOAT);
    }

    @Override
    public double getOriginalValue(double val) {
        return Math.pow(10.0, val);
    }

}
