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
package com.raytheon.viz.core.graphing.axis;

import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.graphing.DataAxisInfo;

/**
 * Abstract class for graph axes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ------------------------------------
 * Oct 30, 2006  56       Phillippe  Initial Creation
 * Oct 2007               njensen    Major refactor
 * Nov 08, 2016  5976     bsteffen   Remove IColorableResource interface
 * 
 * </pre>
 * 
 * @author bphillip
 */
public abstract class Axis implements IAxis {

    protected static final RGB DEFAULT_AXIS_COLOR = new RGB(190, 190, 190);

    protected static final int LABEL_ADJUSTMENT = 4;

    protected static final double THIRD = (1.0 / 3.0);

    /** The bounds of the area available for plotting data * */
    protected Rectangle graphArea;

    /** The title of the axis * */
    protected ArrayList<String> titles = new ArrayList<>();

    /** The orientation of the axis * */
    protected IAxis.Orientation orientation;

    /** The color of the axis * */
    protected RGB color = ColorUtil.DEFAULT_ITEM_COLOR;

    /** The thickness of the axis * */
    protected int lineWeight = 1;

    /** The minimum value of the axis * */
    protected Double minVal;

    /** The maximum value of the axis * */
    protected Double maxVal;

    protected ArrayList<RGB> titleColors = new ArrayList<>();

    protected boolean drawLinesAtLabels = false;

    protected IGraphicsTarget.LineStyle labelLineStyle = IGraphicsTarget.LineStyle.DASHED_LARGE;

    protected DataAxisInfo info;

    protected boolean showTitle = true;

    protected AxisLabeling labeling = new AxisLabeling();

    /**
     * Convenience method for determining position of a value on the axis
     * 
     * @param value
     *            the value to put on the axis
     * @return the x or y coordinate on the axis (depends on the orientation of
     *         the axis)
     */
    protected double doubleValueToCoordinate(double value) {
        double range = this.getMaxVal() - this.getMinVal();
        double diff = value - this.getMinVal();

        double retVal = Double.NaN;
        if (orientation == IAxis.Orientation.VERTICAL) {
            retVal = graphArea.y + graphArea.height - (diff / range)
                    * graphArea.height;
        } else {
            retVal = graphArea.x + (diff / range) * graphArea.width;
        }

        return retVal;
    }

    /**
     * Sets the range of the axis
     * 
     * @param min
     *            The minimum value of the axis
     * @param max
     *            The maximum value of the axis
     */
    @Override
    public void setRange(Double min, Double max) {
        minVal = min;
        maxVal = max;
    }

    @Override
    public void addTitle(String aTitle, RGB aColor) {
        titles.add(aTitle);
        titleColors.add(aColor);
    }

    /**
     * Gets the minimum value of the axis
     * 
     * @return The minimum value of the axis
     */
    @Override
    public Double getMinVal() {
        return minVal;
    }

    /**
     * Gets the maximum value of the axis
     * 
     * @return The maximum value of the axis
     */
    @Override
    public Double getMaxVal() {
        return maxVal;
    }

    @Override
    public ArrayList<String> getTitles() {
        return titles;
    }

    @Override
    public IAxis.Orientation getOrientation() {
        return orientation;
    }

    @Override
    public void setOrientation(IAxis.Orientation orientation) {
        this.orientation = orientation;
    }

    public boolean isDrawLinesAtLabels() {
        return drawLinesAtLabels;
    }

    public void setDrawLinesAtLabels(boolean drawLinesAtLabels) {
        this.drawLinesAtLabels = drawLinesAtLabels;
    }

    public ArrayList<RGB> getColors() {
        return titleColors;
    }

    @Override
    public DataAxisInfo getInfo() {
        return info;
    }

    @Override
    public void setInfo(DataAxisInfo info) {
        this.info = info;
    }

    public boolean isShowTitle() {
        return showTitle;
    }


    public void setShowTitle(boolean showTitle) {
        this.showTitle = showTitle;
    }

    @Override
    public Rectangle getGraphArea() {
        return graphArea;
    }

    @Override
    public AxisLabeling getLabeling() {
        return labeling;
    }


    public void setLabeling(AxisLabeling labeling) {
        this.labeling = labeling;
    }

    public RGB getColor() {
        return color;
    }

    @Override
    public void setColor(RGB color) {
        this.color = color;
    }

    public ArrayList<RGB> getTitleColors() {
        return titleColors;
    }

    @Override
    public void setTitles(ArrayList<String> titles, ArrayList<RGB> titleColors) {
        this.titleColors = titleColors;
        this.titles = titles;
    }

    public IGraphicsTarget.LineStyle getLabelLineStyle() {
        return labelLineStyle;
    }

    public void setLabelLineStyle(IGraphicsTarget.LineStyle labelLineStyle) {
        this.labelLineStyle = labelLineStyle;
    }

}
