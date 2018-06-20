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
package com.raytheon.uf.viz.xy.graph.axis;

import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;

/**
 * Class to determine where to place axes given the Graph labels, calculates
 * linearly. See LogAxisPlacer for the logarithmic placing (TODO)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LinearAxisPlacer implements IAxisPlacer {

    private double pixelWidth;

    private double minDataValue;

    private double maxDataValue;

    private double zoomLevel;

    private double centerDataValue;

    public LinearAxisPlacer(double axisWidth, double minDataValue,
            double maxDataValue) {
        this.pixelWidth = axisWidth;
        this.maxDataValue = maxDataValue;
        this.minDataValue = minDataValue;
        this.zoomLevel = 1;
        this.centerDataValue = (minDataValue + maxDataValue) / 2;

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.ui.xy.graph.IAxisPlacer#placesAxes(double,
     * com.raytheon.uf.viz.ui.xy.graph.plots.IGraphLabel[])
     */
    @Override
    public double[] placeAxes(IAxis[] axes) {
        double[] offsets = new double[axes.length];
        int i = 0;
        for (IAxis axis : axes) {
            offsets[i++] = getPixelLoc(axis.getDiscreteValue());
        }
        return offsets;
    }

    @Override
    public double[] placeLabels(IGraphLabel<?>[] labels) {
        double[] offsets = new double[labels.length];
        int i = 0;
        for (IGraphLabel<?> label : labels) {
            offsets[i++] = getPixelLoc(label.getDiscreteValue());
        }
        return offsets;
    }

    @Override
    public double getDataValue(double pixelLoc) {
        return ((pixelLoc * getDataWidth()) / pixelWidth) + getMinDataValue();
    }

    @Override
    public double getDataWidth() {
        return getMaxDataValue() - getMinDataValue();
    }

    @Override
    public double getMaxDataValue() {
        double ratio = pixelWidth / (maxDataValue - minDataValue);
        double centerPixelLoc = (centerDataValue - minDataValue) * ratio;
        double offset = (pixelWidth / zoomLevel) / 2;
        double maxPixelLoc = centerPixelLoc + offset;
        return minDataValue + maxPixelLoc / ratio;
    }

    @Override
    public double getMinDataValue() {
        double ratio = pixelWidth / (maxDataValue - minDataValue);
        double centerPixelLoc = (centerDataValue - minDataValue) * ratio;
        double offset = (pixelWidth / zoomLevel) / 2;
        double minPixelLoc = centerPixelLoc - offset;
        return minDataValue + minPixelLoc / ratio;
    }

    @Override
    public double getPixelLoc(double dataValue) {
        double ratio = pixelWidth / (getDataWidth());
        double offset = getMinDataValue() * ratio;
        return (dataValue * ratio) - offset;
    }

    @Override
    public void pan(double distance) {
        double ratio = pixelWidth / (maxDataValue - minDataValue);
        double centerPixelLoc = (centerDataValue - minDataValue) * ratio;
        centerPixelLoc += distance / zoomLevel;
        if (centerPixelLoc < pixelWidth / (zoomLevel * 2)) {
            centerPixelLoc = pixelWidth / (zoomLevel * 2);
        } else if (centerPixelLoc > pixelWidth - pixelWidth / (zoomLevel * 2)) {
            centerPixelLoc = pixelWidth - pixelWidth / (zoomLevel * 2);
        }
        centerDataValue = minDataValue + centerPixelLoc / ratio;
    }

    @Override
    public void zoom(double offset, double zoomLevel) {
        double offsetDataValue = getDataValue(offset);
        double offsetPixelLoc = getPixelLoc(offsetDataValue);
        this.zoomLevel = zoomLevel;
        double distance = offsetPixelLoc - getPixelLoc(offsetDataValue);
        double ratio = pixelWidth / (maxDataValue - minDataValue);
        double centerPixelLoc = (centerDataValue - minDataValue) * ratio;
        centerPixelLoc -= distance / this.zoomLevel;
        if (centerPixelLoc < pixelWidth / (this.zoomLevel * 2)) {
            centerPixelLoc = pixelWidth / (this.zoomLevel * 2);
        } else if (centerPixelLoc > pixelWidth - pixelWidth
                / (this.zoomLevel * 2)) {
            centerPixelLoc = pixelWidth - pixelWidth / (this.zoomLevel * 2);
        }
        centerDataValue = minDataValue + centerPixelLoc / ratio;
    }

    @Override
    public void setPixelWidth(double pixelWidth) {
        this.pixelWidth = pixelWidth;

    }
}
