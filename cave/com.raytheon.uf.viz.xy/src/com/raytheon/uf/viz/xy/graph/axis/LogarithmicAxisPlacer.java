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
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LogarithmicAxisPlacer implements IAxisPlacer {

    private double pixelWidth;

    private double minLogValue;

    private double maxLogValue;

    private double zoomLevel;

    private double centerLogValue;

    public LogarithmicAxisPlacer(double axisWidth, double minDataValue,
            double maxDataValue) {
        this.pixelWidth = axisWidth;
        this.maxLogValue = Math.log(maxDataValue);
        this.minLogValue = Math.log(minDataValue);
        this.zoomLevel = 1;
        this.centerLogValue = (minLogValue + maxLogValue) / 2;

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
        return Math.exp(((pixelLoc * getLogWidth()) / pixelWidth)
                + getMinLogValue());
    }

    public double getLogWidth() {
        return getMaxLogValue() - getMinLogValue();
    }

    @Override
    public double getDataWidth() {
        return getMaxDataValue() - getMinDataValue();
    }

    protected double getMaxLogValue() {
        double ratio = pixelWidth / (maxLogValue - minLogValue);
        double centerPixelLoc = (centerLogValue - minLogValue) * ratio;
        double offset = (pixelWidth / zoomLevel) / 2;
        double maxPixelLoc = centerPixelLoc + offset;
        return minLogValue + maxPixelLoc / ratio;
    }

    @Override
    public double getMaxDataValue() {
        return Math.exp(getMaxLogValue());
    }

    protected double getMinLogValue() {
        double ratio = pixelWidth / (maxLogValue - minLogValue);
        double centerPixelLoc = (centerLogValue - minLogValue) * ratio;
        double offset = (pixelWidth / zoomLevel) / 2;
        double maxPixelLoc = centerPixelLoc - offset;
        return minLogValue + maxPixelLoc / ratio;
    }

    @Override
    public double getMinDataValue() {
        return Math.exp(getMinLogValue());
    }

    @Override
    public double getPixelLoc(double dataValue) {
        double logDataValue = Math.log(dataValue);
        if (logDataValue == minLogValue) {
            // Hardcode the min value to avoid rounding errors
            return 0;
        }
        if (logDataValue == maxLogValue) {
            // Hardcode the max value to avoid rounding errors
            return pixelWidth;
        }
        double ratio = pixelWidth / (getLogWidth());
        double offset = getMinLogValue() * ratio;
        return (logDataValue * ratio) - offset;
    }

    @Override
    public void pan(double distance) {
        double ratio = pixelWidth / (maxLogValue - minLogValue);
        double centerPixelLoc = (centerLogValue - minLogValue) * ratio;
        centerPixelLoc += distance / zoomLevel;
        if (centerPixelLoc < pixelWidth / (zoomLevel * 2)) {
            centerPixelLoc = pixelWidth / (zoomLevel * 2);
        } else if (centerPixelLoc > pixelWidth - pixelWidth / (zoomLevel * 2)) {
            centerPixelLoc = pixelWidth - pixelWidth / (zoomLevel * 2);
        }
        centerLogValue = minLogValue + centerPixelLoc / ratio;
    }

    @Override
    public void zoom(double offset, double zoomLevel) {
        double offsetDataValue = getDataValue(offset);
        double offsetPixelLoc = getPixelLoc(offsetDataValue);
        this.zoomLevel = zoomLevel;
        double distance = offsetPixelLoc - getPixelLoc(offsetDataValue);
        double ratio = pixelWidth / (maxLogValue - minLogValue);
        double centerPixelLoc = (centerLogValue - minLogValue) * ratio;
        centerPixelLoc -= distance / this.zoomLevel;
        if (centerPixelLoc < pixelWidth / (this.zoomLevel * 2)) {
            centerPixelLoc = pixelWidth / (this.zoomLevel * 2);
        } else if (centerPixelLoc > pixelWidth - pixelWidth
                / (this.zoomLevel * 2)) {
            centerPixelLoc = pixelWidth - pixelWidth / (this.zoomLevel * 2);
        }
        centerLogValue = minLogValue + centerPixelLoc / ratio;
    }

    @Override
    public void setPixelWidth(double pixelWidth) {
        this.pixelWidth = pixelWidth;

    }
}
