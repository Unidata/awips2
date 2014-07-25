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
import java.util.List;
import java.util.Set;
import java.util.Stack;

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.DataAxisInfo;
import com.raytheon.viz.core.graphing.GraphProperties;
import com.raytheon.viz.core.graphing.GraphUtil;
import com.raytheon.viz.core.graphing.util.GraphUtilPorted;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A graph axis using numbers
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 *                   
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 24Oct2006				Phillippe	Initial Creation      
 * Oct 2007                 njensen     Major refactor
 * 24Jul2014    3429        mapeters    Updated deprecated drawLine() calls.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class NumberAxis extends Axis {

    // found it in old d2d
    private static final int PER_DECADE = 8;

    private String units;

    protected boolean logarithmic = false;

    protected Stack<DataAxisInfo> zoomedAxes = new Stack<DataAxisInfo>();

    protected double relativeLabelOffset = 2.0 / 3.0;

    protected boolean drawTickmarksAtLabels = false;

    protected Integer fontSize;

    /**
     * Constructor
     * 
     * @param graph
     *            The graph to add this axis to
     * @param title
     *            The title of the axis
     * @param orientation
     *            The orientation of the axis (HORIZONTAL or VERTICAL)
     */
    public NumberAxis(IAxis.Orientation orientation, String units) {
        this.orientation = orientation;
        this.units = units;
    }

    /**
     * Sets the thickness of the axis line
     * 
     * @param lineWeight
     *            An thickness value
     */
    public void setLineWeight(int lineWeight) {
        this.lineWeight = lineWeight;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.graphing.IGraphRenderable#paint()
     */
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        IFont font = target.getDefaultFont();
        if (fontSize != null && fontSize != font.getFontSize()) {
            // This should be a class variable but we have no dispose method .
            // :(
            font = target.initializeFont(font.getFontName(), fontSize, font
                    .getStyle());
        }

        IExtent extent = ((GraphProperties) paintProps).getGraph()
                .getWorldExtent();
        graphArea = GraphUtil.getDrawingArea(extent);

        // Draws the axes
        target.drawRect(new PixelExtent(graphArea), DEFAULT_AXIS_COLOR, 1.0f,
                1.0);
        
        List<DrawableLine> lines = new ArrayList<DrawableLine>();
        if (orientation == IAxis.Orientation.VERTICAL) {

            double maxLabelWidth = 0.0;

            // Label the axis
            if (labeling != null) {
                Set<Double> keys = labeling.getLabels().keySet();
                double xPos = 0;
                double yPos = 0;

                for (double labelVal : keys) {
                    double width = target.getStringBounds(font,
                            labeling.getLabel(labelVal)).getWidth();
                    maxLabelWidth = Math.max(width, maxLabelWidth);
                }

                maxLabelWidth *= paintProps.getView().getExtent().getWidth()
                        / paintProps.getCanvasBounds().width;

                xPos = (graphArea.x * (relativeLabelOffset / 5))
                        + (extent.getMinX() * (1 - (relativeLabelOffset / 5)));
                // Determine whether labels are too large for the default
                // calculation
                xPos = Math.min(xPos, graphArea.x - maxLabelWidth);

                for (double labelVal : keys) {
                    double y = valueToCoordinate(labelVal);
                    double[] coords = new double[] { graphArea.x, y };

                    if (extent.contains(coords)) {
                        if (drawLinesAtLabels) {
                            double x = graphArea.x;
                            double x2 = graphArea.x + graphArea.width;

                            DrawableLine line = new DrawableLine();
                            line.setCoordinates(x, y);
                            line.addPoint(x2, y);
                            line.basics.color = DEFAULT_AXIS_COLOR;
                            line.width = lineWeight;
                            line.lineStyle = labelLineStyle;
                            lines.add(line);
                        }

                        yPos = y
                                + (LABEL_ADJUSTMENT * paintProps.getZoomLevel());

                        if (drawTickmarksAtLabels) {
                            double x = graphArea.x;
                            double x2 = xPos
                                    + target.getStringBounds(font,
                                            labeling.getLabel(labelVal))
                                            .getWidth();

                            DrawableLine line = new DrawableLine();
                            line.setCoordinates(x, y);
                            line.addPoint(x2, y);
                            line.basics.color = DEFAULT_AXIS_COLOR;
                            line.width = lineWeight;
                            line.lineStyle = labelLineStyle;
                            lines.add(line);
                        }

                        target.drawString(font, labeling.getLabel(labelVal),
                                xPos, yPos, 0.0,
                                IGraphicsTarget.TextStyle.NORMAL, color,
                                HorizontalAlignment.LEFT, null);
                    }
                }
            }

            // Print the axis title
            for (int i = 0; i < titles.size(); i++) {
                String title = titles.get(i);
                double y = (graphArea.y + graphArea.y + graphArea.height) / 2;

                double yPos = y + (title.length() / 2 * 9);
                double xPos = extent.getMinX() / (i + 1);
                // If Necessary make room for large labels
                xPos = Math.min(xPos, graphArea.x - maxLabelWidth - 10);
                target.drawString(null, title, xPos, yPos, 0.0,
                        IGraphicsTarget.TextStyle.NORMAL, titleColors.get(i),
                        HorizontalAlignment.LEFT, 90.0);
            }
        } else {
            double yEnd = graphArea.y + graphArea.height;
            double xPos = 0;
            double yPos = 0;
            for (int i = 0; i < titles.size(); i++) {
                String title = titles.get(i);
                double x = (graphArea.x + graphArea.width) / 2;
                target.drawString(null, title, x, yEnd
                        + (graphArea.y * THIRD * 2) / (i + 1), 0.0,
                        IGraphicsTarget.TextStyle.NORMAL, titleColors.get(i),
                        HorizontalAlignment.LEFT, null);
            }

            if (labeling != null) {
                Set<Double> keys = labeling.getLabels().keySet();
                for (double labelVal : keys) {
                    double x = valueToCoordinate(labelVal);

                    xPos = x + (LABEL_ADJUSTMENT * paintProps.getZoomLevel());

                    yPos = ((graphArea.y + graphArea.height) * relativeLabelOffset)
                            + (extent.getMaxY() * (1 - relativeLabelOffset));

                    if (drawLinesAtLabels) {
                        double y = graphArea.y;

                        DrawableLine line = new DrawableLine();
                        line.setCoordinates(x, y);
                        line.addPoint(x, yEnd);
                        line.basics.color = DEFAULT_AXIS_COLOR;
                        line.width = lineWeight;
                        line.lineStyle = labelLineStyle;
                        lines.add(line);
                    }
                    if (drawTickmarksAtLabels) {
                        double y = yPos
                                - target.getStringBounds(null,
                                        labeling.getLabel(labelVal))
                                        .getHeight();
                        
                        DrawableLine line = new DrawableLine();
                        line.setCoordinates(x, y);
                        line.addPoint(x, yEnd);
                        line.basics.color = DEFAULT_AXIS_COLOR;
                        line.width = lineWeight;
                        line.lineStyle = labelLineStyle;
                        lines.add(line);
                    }

                    target.drawString(font, labeling.getLabel(labelVal), xPos,
                            yPos, 0.0, IGraphicsTarget.TextStyle.NORMAL, color,
                            HorizontalAlignment.CENTER, null);
                }
            }

        }
        target.drawLine(lines.toArray(new DrawableLine[0]));
        
        if (font != target.getDefaultFont()) {
            font.dispose();
        }

    }

    @Override
    public double valueToCoordinate(Object aValue) {
        double val = ((Number) aValue).doubleValue();
        if (logarithmic) {
            val = GraphUtilPorted.axisDivisionOfValue((float) val, this.info);
            double range = 10 - 0;
            double diff = val - 0;

            double retVal = Double.NaN;
            if (orientation == IAxis.Orientation.VERTICAL) {
                retVal = graphArea.y + graphArea.height - (diff / range)
                        * graphArea.height;
            } else {
                retVal = graphArea.x + (diff / range) * graphArea.width;
            }

            return retVal;

        } else {
            return doubleValueToCoordinate(val);
        }
    }

    public double coordinateToValue(Coordinate aCoordinate) {
        double retVal = Double.NaN;
        double range = getMaxVal() - getMinVal();
        if (orientation == IAxis.Orientation.HORIZONTAL) {
            if (logarithmic) {
                double x = GraphUtilPorted.N_GRAPH_DATA_DIVS
                        - ((GraphUtilPorted.N_GRAPH_DATA_DIVS
                                * (aCoordinate.x - graphArea.x) / graphArea.width));
                retVal = GraphUtilPorted.valueOfAxisDivision((float) x, info);

            } else {
                retVal = getMinVal()
                        + ((range * (aCoordinate.x - graphArea.x) / graphArea.width));
            }
        } else {
            if (logarithmic) {
                double y = GraphUtilPorted.N_GRAPH_DATA_DIVS
                        - ((GraphUtilPorted.N_GRAPH_DATA_DIVS
                                * (aCoordinate.y - graphArea.y) / graphArea.height));
                retVal = GraphUtilPorted.valueOfAxisDivision((float) y, info);
            } else {
                retVal = getMaxVal()
                        - ((range * (aCoordinate.y - graphArea.y) / graphArea.height));
            }
        }

        return retVal;
    }

    /**
     * @return the units
     */
    public String getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(String units) {
        this.units = units;
    }

    /**
     * @return the logarithmic
     */
    public boolean isLogarithmic() {
        return logarithmic;
    }

    /**
     * @param logarithmic
     *            the logarithmic to set
     */
    public void setLogarithmic(boolean logarithmic) {
        this.logarithmic = logarithmic;
    }

    /**
     * @param fontSize
     *            the fontSize to set
     */
    public void setFontSize(Integer fontSize) {
        this.fontSize = fontSize;
    }

    @Override
    public boolean zoom(double x, double y, double zoom) {
        boolean success = true;
        double val = coordinateToValue(new Coordinate(x, y));
        double maxVal = getMaxVal();
        double minVal = getMinVal();

        if (info != null) {
            double divMin = info.getDivMin();
            double divMax = info.getDivMax();

            if (zoom > 0) {
                if (!logarithmic) {
                    // linear axis

                    double newRange = (maxVal - minVal) / 2;
                    double midpoint = (maxVal + minVal) / 2;
                    double maxDiff = maxVal - val;
                    double minDiff = val - minVal;
                    double midDiff = Math.abs(val - midpoint);

                    if (minDiff < midDiff && minDiff < maxDiff) {
                        // zoom towards the min
                        divMin = minVal;
                        divMax = minVal + newRange;
                    } else if (maxDiff < midDiff && maxDiff < minDiff) {
                        // zoom towards the max
                        divMin = maxVal - newRange;
                        divMax = maxVal;
                    } else {
                        // zoom towards the middle
                        divMin = minVal + newRange / 2;
                        divMax = maxVal - newRange / 2;
                    }
                } else {
                    // logarithmic axis
                    double top = GraphUtilPorted.N_GRAPH_DATA_DIVS;
                    double mid = GraphUtilPorted.valueOfAxisDivision(
                            GraphUtilPorted.N_GRAPH_DATA_DIVS * .5f, info);
                    double bottom = 0;
                    double midPos = GraphUtilPorted.N_GRAPH_DATA_DIVS / 2;
                    double valPos = GraphUtilPorted.axisDivisionOfValue(
                            (float) val, info);

                    double minDiff = valPos - bottom;
                    double midDiff = Math.abs(valPos - midPos);
                    double maxDiff = top - valPos;

                    if (minDiff < midDiff && minDiff < maxDiff) {
                        // zoom towards the min
                        divMin = minVal;
                        divMax = mid;
                    } else if (maxDiff < midDiff && maxDiff < minDiff) {
                        // zoom towards the max
                        divMin = mid;
                        divMax = maxVal;
                    } else {
                        // zoom towards the middle
                        divMin = GraphUtilPorted.valueOfAxisDivision(
                                GraphUtilPorted.N_GRAPH_DATA_DIVS * .25f, info);
                        divMax = GraphUtilPorted.valueOfAxisDivision(
                                GraphUtilPorted.N_GRAPH_DATA_DIVS * .75f, info);
                    }
                }

                DataAxisInfo newInfo = GraphUtilPorted.calcDataAxisInfo(
                        (float) divMin, info.isZeroYes(), info.getAbsMin(),
                        (float) divMax, (int) zoom, info.getStyle(),
                        (float) divMin, (float) divMax, info.getInterval(),
                        info.getDLinear(), info.getZeroDiv(), PER_DECADE);
                // if we're zooming into an individual graph, there's always
                // only
                // one graph on screen
                AxisLabeling lb = AxisUtil.makeYaxis(newInfo, 1, true);

                zoomedAxes.push(info);
                this.info = newInfo;
                this.labeling = lb;
                this.setRange((double) info.getDivMin(), (double) info
                        .getDivMax());
            } else {
                // zoom back out
                this.info = zoomedAxes.pop();
                this.setRange((double) info.getDivMin(), (double) info
                        .getDivMax());
                boolean zooming = true;
                if (zoomedAxes.size() == 0) {
                    zooming = false;
                }
                this.labeling = AxisUtil.makeYaxis(info, 1, zooming);
            }
        }

        return success;
    }

    public void updateLabeling(int numberOfGraphs) {
        labeling = AxisUtil.makeYaxis(info, numberOfGraphs, false);
    }

    /**
     * @return the drawTickmarksAtLabels
     */
    public boolean isDrawTickmarksAtLabels() {
        return drawTickmarksAtLabels;
    }

    /**
     * @param drawTickmarksAtLabels
     *            the drawTickmarksAtLabels to set
     */
    public void setDrawTickmarksAtLabels(boolean drawTickmarksAtLabels) {
        this.drawTickmarksAtLabels = drawTickmarksAtLabels;
    }

}
