/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.edex.uengine.tasks.ncgrib;

import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferFloat;
import java.awt.image.Raster;
import java.awt.image.SampleModel;

import javax.media.jai.BorderExtender;
import javax.media.jai.Interpolation;
import javax.media.jai.JAI;
import javax.media.jai.ParameterBlockJAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RasterFactory;
import javax.media.jai.TiledImage;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.edex.colormap.ColorMapManager;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * GribMap task derived from original uEngine GribMap task. Maps grid data to an
 * image.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class NcgribMap extends ScriptTask {

    private String colorMapName;

    private int scaleFactor = 1;

    private IDataRecord dataRecord;

    private GridGeometry2D gridGeometry;

    private static final float DEF_MINIMUM = Float.POSITIVE_INFINITY;

    private static final float DEF_MAXIMUM = Float.NEGATIVE_INFINITY;

    private float minimum = DEF_MINIMUM;

    private float maximum = DEF_MAXIMUM;

    public NcgribMap(String aPlugin, String aColorMapName,
            IDataRecord aDataRecord, GridGeometry2D aGridGeometry) {
        colorMapName = aColorMapName;
        dataRecord = aDataRecord;
        gridGeometry = aGridGeometry;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        float max = -9999;
        float min = 9999;
        float userMin;
        float userMax;

        float[] gribData = (float[]) dataRecord.getDataObject();

        // ColorMap colorMap = ColorMap.getColorMap(colorMapName);
        // if (colorMap == null) {
        // throw new MicroEngineException(
        // "Invalid ColorMap name " + Util.printString(colorMapName) +
        // ", cannot decode image.");
        // }
        /*
         * subtract one since we map the GRIB values to valid pixel (grey scale)
         * values based on the color map. For example, a 32 color color map
         * requires pixel values 0 to 31.
         */
        int colors = (int) ColorMapManager.MAX_VALUE;

        int width = gridGeometry.getGridRange().getSpan(0);
        int height = gridGeometry.getGridRange().getSpan(1);

        if (scaleFactor > 1.0) {

            int len = width * height;
            Point origin = new Point(0, 0);

            // create a float sample model
            SampleModel sampleModel = RasterFactory.createBandedSampleModel(
                    DataBuffer.TYPE_FLOAT, width, height, 1);

            // create a TiledImage using the float SampleModel
            TiledImage tiledImage = new TiledImage(0, 0, width, height, 0, 0,
                    sampleModel, null);

            // create a DataBuffer from the float[][] array
            DataBufferFloat dataBuffer = new DataBufferFloat(gribData, len);

            // create a Raster
            Raster raster = RasterFactory.createWritableRaster(sampleModel,
                    dataBuffer, origin);

            // set the TiledImage data to that of the Raster
            tiledImage.setData(raster);

            PlanarImage scaledImg;

            // Interpolate the image using a scale factor and
            // the copy border extender
            ParameterBlockJAI param = new ParameterBlockJAI("Scale");
            param.addSource(tiledImage);
            param.setParameter("xScale", (float) scaleFactor);
            param.setParameter("yScale", (float) scaleFactor);
            Interpolation interpol = Interpolation
                    .getInstance(Interpolation.INTERP_BILINEAR);
            param.setParameter("interpolation", interpol);
            RenderingHints hint = new RenderingHints(JAI.KEY_BORDER_EXTENDER,
                    BorderExtender.createInstance(BorderExtender.BORDER_COPY));
            scaledImg = JAI.create("Scale", param, hint);

            // Get the floats back out
            DataBuffer newDb = scaledImg.getData().getDataBuffer();
            DataBufferFloat dbf = (java.awt.image.DataBufferFloat) newDb;

            // Update the grib data objects for further processing
            gribData = dbf.getData();
            width = width * scaleFactor;
            height = height * scaleFactor;
            gridGeometry = new GridGeometry2D(new GeneralGridEnvelope(
                    new int[] { 0, 0 }, new int[] { width, height }, false),
                    gridGeometry.getEnvelope());
        }
        int pixels = gribData.length;

        for (int i = 0; i < pixels; i++) {
            if (gribData[i] != Util.GRID_FILL_VALUE) {
                if (max < gribData[i]) {
                    max = gribData[i];
                }

                if (min > gribData[i]) {
                    min = gribData[i];
                }
            }
        }
        /*
         * validate the user specified minimum
         */
        userMin = minimum;
        if (minimum != DEF_MINIMUM) {
            userMin = minimum;
            // if (userMin > min) {
            // theLogger.warn("Requested minimum [" + userMin +
            // "] larger than computed minimum. "+
            // "Using computed minimum of " + min);
            // userMin = min;
            // }
        } else {
            userMin = min;
        }
        /*
         * validate user specified maximum
         */
        if (maximum != DEF_MAXIMUM) {
            userMax = maximum;
            // if (userMax < max) {
            // theLogger.warn("Requested maximum [" + userMax +
            // "] smaller than computed maximum. "+
            // "Using computed maximum of " + max);
            // userMax = max;
            // }
        } else {
            userMax = max;
        }
        /*
         * not sure if this can actually happen, but for completeness
         */
        if (userMax < userMin) {
            logger.warn("Invalid minimum/maximum specified. "
                    + "Using computed values of " + min + " and " + max);
            userMin = min;
            userMax = max;
        }
        /**
         * update the minimum and maximum values
         */
        min = userMin;
        max = userMax;

        /*
         * remap the data
         */
        float range = max - min;
        float diff = 0f;
        byte[] imageData = new byte[pixels];
        for (int index = 0; index < pixels; index++) {
            float pixel = gribData[index];
            if (pixel < min) {
                pixel = min;
            } else if (pixel > max) {
                pixel = max;
            }
            diff = pixel - min;
            imageData[index] = (byte) Math.abs((Math.round(colors
                    * (diff / range))));
        }

        return imageData;
    }

    public String getColorMapName() {
        return colorMapName;
    }

    public void setColorMapName(String aColorMapName) {
        colorMapName = aColorMapName;
    }

    public IDataRecord getDataRecord() {
        return dataRecord;
    }

    public void setDataRecord(IDataRecord aDataRecord) {
        dataRecord = aDataRecord;
    }

    public float getMaximum() {
        return maximum;
    }

    public void setMaximum(float aMaximum) {
        maximum = aMaximum;
    }

    public float getMinimum() {
        return minimum;
    }

    public void setMinimum(float aMinimum) {
        minimum = aMinimum;
    }

    public int getScaleFactor() {
        return scaleFactor;
    }

    public void setScaleFactor(int aScaleFactor) {
        scaleFactor = aScaleFactor;
    }

    /**
     * @return the gridGeometry
     */
    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    /**
     * @param gridGeometry
     *            the gridGeometry to set
     */
    public void setGridGeometry(GridGeometry2D gridGeometry) {
        this.gridGeometry = gridGeometry;
    }

}
