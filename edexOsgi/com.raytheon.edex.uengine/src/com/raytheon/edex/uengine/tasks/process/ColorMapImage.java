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

package com.raytheon.edex.uengine.tasks.process;

import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.edex.colormap.ColorMapManager;
import com.raytheon.edex.exception.ColorTableException;
import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;

/**
 * ColorMapImage task derived from original uEngine ColorMapImage task. Applies
 * a color map to an image.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 29, 2007                     njensen             Initial Creation
 * Jul 26, 2007                     njensen             Uses com.raytheon.edex.colormaps.ColorMap
 * </PRE>
 * 
 */
public class ColorMapImage extends ScriptTask {

    private String colorMapName;

    private byte[] image;

    private GridGeometry2D gridGeometry;

    /**
     * Constructor
     * 
     * @param aColorMapName
     *            the name of the color map
     * @param anImage
     *            the image in bytes
     * @param aGeometry
     *            the geometry of the image
     */
    public ColorMapImage(String aColorMapName, Object anImage,
            GridGeometry2D aGeometry) {
        colorMapName = aColorMapName;
        image = (byte[]) anImage;
        gridGeometry = aGeometry;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        // ByteBuffer buffer = ByteBuffer.allocate(image.length);
        // buffer.put(image);
        //
        // // must set buffer position before doing a get, otherwise get
        // Exception
        // buffer.position(0);
        //
        int width = gridGeometry.getGridRange().getSpan(0);
        int height = gridGeometry.getGridRange().getSpan(1);
        //
        // byte raster[] = new byte[width * height];
        // byte[] raster = image;

        // logger.debug("Reading raw image file: width=" + width
        // + ", height=" + height + ", img.len="
        // + image.length + ", ras.len=" + raster.length);

        // for (int iy = 0; iy < height; iy++) {
        // buffer.get(raster, iy * width, width);
        // }

        IndexColorModel cm = null;
        try {
            cm = ColorMapManager.buildColorModel(ColorMapManager.getInstance()
                    .getColorMap(colorMapName));
        } catch (ColorTableException e) {
            throw new MicroEngineException("Error creating color model", e);
        }
        DataBufferByte byteArray = new DataBufferByte(image, width * height);

        MultiPixelPackedSampleModel sample = new MultiPixelPackedSampleModel(
                DataBuffer.TYPE_BYTE, width, height,
                ColorMapManager.NUMBER_BITS);
        WritableRaster writeRaster = Raster.createWritableRaster(sample,
                byteArray, new Point(0, 0));

        BufferedImage bi = new BufferedImage(width, height,
                BufferedImage.TYPE_BYTE_INDEXED, cm);
        bi.setData(writeRaster);

        return bi;
    }

    public String getColorMapName() {
        return colorMapName;
    }

    public void setColorMapName(String aColorMapName) {
        colorMapName = aColorMapName;
    }

    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    public void setGridGeometry(GridGeometry2D aGridGeometry) {
        gridGeometry = aGridGeometry;
    }

    public byte[] getImage() {
        return image;
    }

    public void setImage(byte[] aImage) {
        image = aImage;
    }

}
