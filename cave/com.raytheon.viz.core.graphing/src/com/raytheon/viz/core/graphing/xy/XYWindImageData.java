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
package com.raytheon.viz.core.graphing.xy;

import java.awt.image.BufferedImage;

import javax.measure.converter.UnitConverter;

import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.viz.pointdata.PointWindDisplay;

/**
 * XYImageData for wind barbs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
public class XYWindImageData extends XYImageData {

    public static final int IMAGE_SIZE = 80;

    private double windSpd;

    private double windDir;

    public XYWindImageData(Object ax, Object ay, double aWindSpd,
            double aWindDir) {
        super(ax, ay);
        windSpd = aWindSpd;
        windDir = aWindDir;
    }

    /**
     * @return the windSpd
     */
    public double getWindSpd() {
        return windSpd;
    }

    /**
     * @return the windDir
     */
    public double getWindDir() {
        return windDir;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.graphing.xy.XYImageData#generateImage()
     */
    @Override
    protected void generateImage() {
        PointWindDisplay windDisplay = new PointWindDisplay(IMAGE_SIZE * 0.4,
                1.0, 0, IMAGE_SIZE / 32);
        windDisplay.setImageParameters(IMAGE_SIZE, IMAGE_SIZE, color.red,
                color.green, color.blue, 1);
        windDisplay.setWind(windDir, windSpd, true);
        BufferedImage windImage = windDisplay.getWindImage(false,
                PointWindDisplay.DisplayType.BARB, 4.0);
        image = target.initializeRaster(
                new IODataPreparer(windImage, "wind", 0), null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.graphing.xy.XYImageData#getDefaultSize()
     */
    @Override
    public int[] getDefaultSize() {
        return new int[] { IMAGE_SIZE, IMAGE_SIZE };
    }

    /**
     * @param converter
     */
    public void convert(UnitConverter converter) {
        windSpd = converter.convert(windSpd);
    }

}
