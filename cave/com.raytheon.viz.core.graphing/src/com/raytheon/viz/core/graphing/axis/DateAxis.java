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

import java.util.Arrays;
import java.util.GregorianCalendar;
import java.util.Set;
import java.util.Stack;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.GraphProperties;
import com.raytheon.viz.core.graphing.GraphUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Graph axis which uses dates.
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date       	Ticket#		Engineer	Description
 *     ------------	----------	-----------	--------------------------
 *     Oct 2006                 Phillipe    Initial creation
 *     Oct 2007                 njensen     Major refactor
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class DateAxis extends Axis {

    protected static final String LABEL_LINEBREAK = "\n";

    protected static final int LABEL_SEPARATION = 100;

    protected static double MARK_LENGTH = 5.0;

    protected Stack<AxisLabeling> zoomedAxes = new Stack<AxisLabeling>();

    public DateAxis(IAxis.Orientation orientation) {
        this.orientation = orientation;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.graphing.IGraphRenderable#paint()
     */
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        IExtent extent = ((GraphProperties) paintProps).getGraph()
                .getWorldExtent();
        Rectangle bounds = GraphUtil.getBounds(extent);
        graphArea = GraphUtil.getDrawingArea(extent);

        // Draws the axes
        // target.drawRect(new PixelExtent(graphArea), DEFAULT_AXIS_COLOR, 1.0f,
        // 1.0);

        if (orientation == IAxis.Orientation.VERTICAL) {

            // TODO not up to date with latest changes

        } else {
            double boundsGraphDiff = bounds.y + bounds.height - graphArea.y
                    - graphArea.height;
            double y = graphArea.y + graphArea.height + boundsGraphDiff * THIRD;

            Set<Double> keys = labeling.getLabels().keySet();
            RGB[] colors = new RGB[4];
            for (int i = 0; i < colors.length; i++) {
                colors[i] = color;
            }

            // we need to draw the labels in order so we can remove drawing
            // some if there's too many too close together
            double[] times = new double[keys.size()];
            int i = 0;
            for (double time : keys) {
                times[i] = time;
                i++;
            }
            Arrays.sort(times);

            double previousLabelX = 0;
            double startLabelX = valueToCoordinate(times[0]);
            double endLabelX = valueToCoordinate(times[times.length - 1]);

            for (double time : times) {
                double x = valueToCoordinate(time);
                y = graphArea.y + graphArea.height;
                target.drawLine(x - MARK_LENGTH, y + MARK_LENGTH, 0.0, x, y,
                        0.0, color, lineWeight);
                target.drawLine(x, y, 0.0, x + MARK_LENGTH, y + MARK_LENGTH,
                        0.0, color, lineWeight);
                if (x == startLabelX
                        || x == endLabelX
                        || (Math.abs(x - previousLabelX) > LABEL_SEPARATION
                                && Math.abs(x - startLabelX) > LABEL_SEPARATION && Math
                                .abs(x - endLabelX) > LABEL_SEPARATION)) {
                    String label = labeling.getLabel(time);
                    String[] split = label.split(LABEL_LINEBREAK);
                    target.drawStrings(null, split, x, y, 0.0,
                            TextStyle.NORMAL, colors,
                            HorizontalAlignment.CENTER, VerticalAlignment.TOP);
                    previousLabelX = x;
                }
            }
        }
    }

    @Override
    public double valueToCoordinate(Object aValue) {
        double val = GraphUtil.getNumberRepresentation(aValue);
        return doubleValueToCoordinate(val);
    }

    /**
     * Gets the minimum value of the axis
     * 
     * @return The minimum value of the axis
     */
    public GregorianCalendar getBeginDate() {

        GregorianCalendar retVal = new GregorianCalendar();
        retVal.setTimeInMillis(minVal.longValue());
        return retVal;
    }

    /**
     * Gets the maximum value of the axis
     * 
     * @return The maximum value of the axis
     */
    public GregorianCalendar getEndDate() {
        GregorianCalendar retVal = new GregorianCalendar();
        retVal.setTimeInMillis(maxVal.longValue());
        return retVal;
    }

    public double coordinateToValue(Coordinate aCoordinate) {
        double retVal = Double.NaN;
        double range = getMaxVal() - getMinVal();
        if (orientation == IAxis.Orientation.HORIZONTAL) {
            retVal = getMinVal()
                    + ((range * (aCoordinate.x - graphArea.x) / graphArea.width));
        } else {

            retVal = getMaxVal()
                    - ((range * (aCoordinate.y - graphArea.y) / graphArea.height));
        }

        return retVal;
    }

    @Override
    public boolean zoom(double x, double y, double zoom) {
        // this may not be a good solution, let's ensure there's always
        // two points on the axis

        if (zoom > 1) {
            if (labeling.getLabels().size() > 2) {
                zoomedAxes.push(this.labeling);

                // if you're closer one end, drop the time on that end
                double val = coordinateToValue(new Coordinate(x, y));

                double maxDiff = getMaxVal() - val;
                double minDiff = val - getMinVal();

                AxisLabeling zoomedLabeling = new AxisLabeling();
                zoomedLabeling.setSampleFormat(labeling.getSampleFormat());

                boolean removeFirst = false;
                boolean removeLast = false;
                if (minDiff < maxDiff) {
                    // zoom towards the min
                    removeLast = true;
                } else {
                    // zoom towards the max
                    removeFirst = true;
                }

                Set<Double> keys = this.labeling.getLabels().keySet();
                for (Double key : keys) {
                    zoomedLabeling.getLabels().put(key, labeling.getLabel(key));
                }
                double[] minMax = getLabelRanges(zoomedLabeling);
                if (removeFirst) {
                    zoomedLabeling.getLabels().remove(minMax[0]);
                } else if (removeLast) {
                    zoomedLabeling.getLabels().remove(minMax[1]);
                }

                minMax = getLabelRanges(zoomedLabeling);
                this.setRange(minMax[0], minMax[1]);
                labeling = zoomedLabeling;
            }
        } else if (zoom < 0) {
            if (zoomedAxes.size() > 0) {
                labeling = zoomedAxes.pop();
                double[] minMax = getLabelRanges(labeling);
                this.setRange(minMax[0], minMax[1]);
            }
        }

        return true;
    }

    private static double[] getLabelRanges(AxisLabeling labels) {
        Set<Double> keys = labels.getLabels().keySet();
        double min = Double.MAX_VALUE;
        double max = Double.MIN_VALUE;
        for (Double key : keys) {
            min = Math.min(min, key);
            max = Math.max(max, key);
        }

        return new double[] { min, max };
    }

    @Override
    public void updateLabeling(int numberOfGraphs) {
        // currently not necessary
    }

}
