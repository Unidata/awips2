package gov.noaa.nws.ncep.viz.rsc.solarimage;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
import gov.noaa.nws.ncep.viz.rsc.solarimage.util.ImageData;

import java.nio.FloatBuffer;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.image.ColorMapData.ColorMapDataType;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * dataCallback for SolarImage when LOG scale
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * 01/22/2013    958         qzhou     Initial Creation.
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
public class LogSolarImageDataCallback extends SolarImageDataCallback {

    /**
     * @param dataURI
     * @throws VizException
     */
    public LogSolarImageDataCallback(SolarImageRecord record)
            throws VizException {
        super(record);
    }

    @Override
    public ColorMapData getColorMapData() throws VizException {

        if (imgData == null) {
            imgData = new ImageData(getImageNx(), getImageNy(), getHeaderData()
                    .getBitpix(), getHeaderData().getBscale(), getHeaderData()
                    .getBzero(), getDataFromHDF5().getKernel());
            setImageNx(imgData.getNx());
            setImageNy(imgData.getNy());
        }
        int[] dimensions = new int[] { getImageData().getNx(),
                getImageData().getNy() };

        float[] vals = getImageData().getImageValues();
        float[] logVals = new float[vals.length];

        for (int n = 0; n < vals.length; n++) {
            if (vals[n] <= 0)
                logVals[n] = Float.NEGATIVE_INFINITY;
            else
                logVals[n] = (float) (Math.log10(vals[n]));
        }

        FloatBuffer buffer = FloatBuffer.wrap(logVals);

        return new ColorMapData(buffer, dimensions, ColorMapDataType.FLOAT);
    }

    @Override
    public double getOriginalValue(double val) {
        return Math.pow(10.0, val);
    }

}
