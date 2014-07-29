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
package com.raytheon.viz.radar.ui.xy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.AbstractDrawableObject;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.graphing.GraphProperties;
import com.raytheon.viz.core.graphing.GraphUtil;
import com.raytheon.viz.core.graphing.axis.AxisFactory;
import com.raytheon.viz.core.graphing.axis.AxisLabeling;
import com.raytheon.viz.core.graphing.axis.IAxis;
import com.raytheon.viz.core.graphing.axis.NumberAxis;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.core.graphing.xy.XYGraph;
import com.raytheon.viz.radar.ui.xy.RadarGraphResource.GraphPosition;

/**
 * This graph will handle cell trend data from the Storm Structure product
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009             askripsk    Initial creation
 * 07-21-14     #3412      mapeters    Updated deprecated drawCircle call.
 * 07-24-14     #3429      mapeters    Updated deprecated drawLine() calls.
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class CellTrendGraph extends XYGraph {
    public enum PointType {
        CIRCLE, X, UP_ARROW, DOWN_ARROW
    }

    private String xLabel = "";

    private String yLabel = "";

    private List<Integer> xValues;

    private ArrayList<Float> yValues;

    private ArrayList<XYDataList> dataSeries = new ArrayList<XYDataList>();

    private ArrayList<LineStyle> dataSeriesLineTypes = new ArrayList<LineStyle>();

    private ArrayList<PointType> dataSeriesPointTypes = new ArrayList<PointType>();

    private ArrayList<String> dataSeriesLabels = new ArrayList<String>();

    private GraphPosition position;

    private OutlineCapability outlineCap;

    private ColorableCapability colorCap;

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        GraphProperties graphProps = (GraphProperties) paintProps;

        createAxes();
        super.paint(target, paintProps);

        if (dataSeries.size() > 0) {
            if (domainAxis != null && rangeAxis != null) {
                paintLegend(target, graphProps);

                /*
                 * Paint the data series
                 */
                target.setupClippingPlane(this.worldExtent);
                double previousScreenX = 0.0;
                double previousScreenY = 0.0;

                // Paint each series in the xyData
                boolean first;
                int i = 0;

                LineStyle currLineStyle;
                PointType currPointType;
                List<DrawableCircle> circles = new ArrayList<DrawableCircle>();
                List<DrawableLine> lines = new ArrayList<DrawableLine>();
                for (XYDataList currSeries : dataSeries) {
                    currLineStyle = dataSeriesLineTypes.get(i);
                    currPointType = dataSeriesPointTypes.get(i++);
                    
                    first = true;
                    for (XYData currPoint : currSeries.getData()) {
                        double x = ((Number) currPoint.getX()).doubleValue();
                        double y = ((Number) currPoint.getY()).doubleValue();

                        if (Double.compare(y, 100) > 0) {
                            y /= 10;
                        }

                        double screenX = domainAxis.valueToCoordinate(x);
                        double screenY = rangeAxis.valueToCoordinate(y);

                        AbstractDrawableObject object = drawPoint(target,
                                screenX, screenY, currPointType);
                        // Add the point to its corresponding list
                        if (object instanceof DrawableCircle) {
                            circles.add((DrawableCircle) object);
                        } else if (object instanceof DrawableLine) {
                            lines.add((DrawableLine) object);
                        }

                        if (first) {
                            first = false;
                        } else {
                            DrawableLine line = new DrawableLine();
                            line.setCoordinates(screenX, screenY);
                            line.addPoint(previousScreenX, previousScreenY);
                            line.basics.color = colorCap.getColor();
                            line.width = outlineCap.getOutlineWidth();
                            line.lineStyle = currLineStyle;
                            lines.add(line);
                        }
                        previousScreenX = screenX;
                        previousScreenY = screenY;
                    }
                }
                target.drawLine(lines.toArray(new DrawableLine[0]));
                target.drawCircle(circles.toArray(new DrawableCircle[0]));

                target.clearClippingPlane();
            }
        }
    }
    
    private AbstractDrawableObject drawPoint(IGraphicsTarget target, double x, double y,
            PointType currPointType) throws VizException {
        if (currPointType.equals(PointType.CIRCLE)) {
            DrawableCircle circle = new DrawableCircle();
            circle = new DrawableCircle();
            circle.setCoordinates(x, y);
            circle.radius = 3.0;
            circle.basics.color = colorCap.getColor();
            return circle;
        } else if (currPointType.equals(PointType.X)) {
            DrawableLine line = new DrawableLine();
            line = new DrawableLine();
            line.setCoordinates(x - 3, y - 3);
            line.addPoint(x + 3, y + 3);
            line.addPoint(x, y);
            line.addPoint(x - 3, y + 3);
            line.addPoint(x + 3, y - 3);
            line.basics.color = colorCap.getColor();
            return line;
        } else if (currPointType.equals(PointType.UP_ARROW)) {
            DrawableLine line = new DrawableLine();
            line = new DrawableLine();
            line.setCoordinates(x - 3, y + 3);
            line.addPoint(x, y - 3);
            line.addPoint(x + 3, y + 3);
            line.basics.color = colorCap.getColor();
            return line;
        } else if (currPointType.equals(PointType.DOWN_ARROW)) {
            DrawableLine line = new DrawableLine();
            line = new DrawableLine();
            line.setCoordinates(x - 3, y - 3);
            line.addPoint(x, y + 3);
            line.addPoint(x + 3, y - 3);
            line.basics.color = colorCap.getColor();
            return line;
        } else {
            return null;
        }
    }

    private void paintLegend(IGraphicsTarget target, GraphProperties graphProps)
            throws VizException {
        // Get the area for the legend
        IExtent extent = graphProps.getGraph().getWorldExtent();

        Rectangle graphArea = GraphUtil.getDrawingArea(extent);

        Rectangle legendArea = new Rectangle(
                graphArea.x,
                (int) ((graphArea.y + graphArea.height + extent.getMaxY()) / 2),
                graphArea.width,
                (int) ((extent.getMaxY() - (graphArea.y + graphArea.height)) / 2));

        double labelX0 = legendArea.x;
        double labelX1 = legendArea.x + (legendArea.width / 2);
        double labelY0 = legendArea.y;
        double labelY1 = legendArea.y + (legendArea.height / 2);

        double[] labelx = new double[] { labelX0, labelX1, labelX0, labelX1 };
        double[] labely = new double[] { labelY0, labelY0, labelY1, labelY1 };

        double offset = (labelX1 - labelX0) * 1 / 4;

        List<DrawableCircle> circles = new ArrayList<DrawableCircle>();
        List<DrawableLine> lines = new ArrayList<DrawableLine>();
        // Write legend from left to right and top to bottom
        for (int i = 0; i < dataSeriesLabels.size(); i++) {
            // Point type
            PointType pt = dataSeriesPointTypes.get(i);
            AbstractDrawableObject object = drawPoint(target, labelx[i],
                    labely[i], pt);
            //Add the point to its corresponding list
            if (object instanceof DrawableCircle) {
                circles.add((DrawableCircle) object);
            } else if (object instanceof DrawableLine) {
                lines.add((DrawableLine) object);
            }

            // Draw line sample
            DrawableLine line = new DrawableLine();
            line.setCoordinates(labelx[i], labely[i]);
            line.addPoint(labelx[i] + (offset * 3 / 4), labely[i]);
            line.basics.color = colorCap.getColor();
            line.lineStyle = dataSeriesLineTypes.get(i);
            lines.add(line);

            // Label Text
            target.drawString(null, dataSeriesLabels.get(i),
                    labelx[i] + offset, labely[i], 0.0, TextStyle.NORMAL,
                    colorCap.getColor(), HorizontalAlignment.LEFT,
                    VerticalAlignment.MIDDLE, 0.0);
        }
        
        target.drawLine(lines.toArray(new DrawableLine[0]));
        target.drawCircle(circles.toArray(new DrawableCircle[0]));
    }

    private void createAxes() {
        // Create Y axis
        NumberAxis yAxis;
        double min = 0;
        double max;
        if (position == GraphPosition.LR) {
            max = 100;
        } else {
            if (!dataSeries.isEmpty() && !dataSeries.get(0).getData().isEmpty()) {
                max = (Integer) dataSeries.get(0).getData().get(0).getY();

                for (XYDataList list : dataSeries) {
                    for (XYData value : list.getData()) {
                        if (((Integer) value.getY()) > max) {
                            max = (Integer) value.getY();
                        }
                    }
                }
            } else {
                max = 100;
            }

            if (Double.compare(max, 100.0) > 0) {
                max /= 10;
            }

            // Round up the maximum value
            max = ((max / 10) + 1) * 10;
        }

        HashMap<Double, String> labelVals = new HashMap<Double, String>();

        for (int i = 0; i < max; i += 10) {
            labelVals.put(Double.valueOf(i), String.valueOf(i));
        }

        AxisLabeling axisLabels = new AxisLabeling();
        axisLabels.setLabels(labelVals);

        yAxis = AxisFactory.buildNumberAxis(IAxis.Orientation.VERTICAL, min,
                max, "");

        yAxis.setLabeling(axisLabels);
        yAxis.setDrawLinesAtLabels(true);
        yAxis.setLabelLineStyle(IGraphicsTarget.LineStyle.DASHED_LARGE);
        yAxis.setFontSize(10);

        ArrayList<String> label = new ArrayList<String>();
        label.add(this.yLabel);

        ArrayList<RGB> color = new ArrayList<RGB>();
        color.add(colorCap.getColor());

        yAxis.setTitles(label, color);

        setRangeAxis(yAxis);
    }

    /**
     * @return the xLabel
     */
    public String getXLabel() {
        return xLabel;
    }

    /**
     * @param label
     *            the xLabel to set
     */
    public void setXLabel(String label) {
        xLabel = label;
    }

    /**
     * @return the yLabel
     */
    public String getYLabel() {
        return yLabel;
    }

    /**
     * @param label
     *            the yLabel to set
     */
    public void setYLabel(String label) {
        yLabel = label;
    }

    /**
     * @return the xValues
     */
    public List<Integer> getXValues() {
        return xValues;
    }

    /**
     * @param labels
     *            the xValues to set
     */
    public void setXValues(List<Integer> labels) {
        xValues = labels;

        if (xValues != null && xValues.size() > 0) {
            HashMap<Double, String> labelVals = new HashMap<Double, String>();

            int min = xValues.get(0);
            int max = xValues.get(0);

            for (int time : xValues) {
                if (time < min) {
                    min = time;
                }
                if (time > max) {
                    max = time;
                }

                labelVals.put(Double.valueOf(time), String.format("%02d%02d",
                        time / 60, time % 60));
            }

            NumberAxis xAxis = AxisFactory.buildNumberAxis(
                    IAxis.Orientation.HORIZONTAL, min, max, "");
            xAxis.setDrawLinesAtLabels(false);
            xAxis.setDrawTickmarksAtLabels(true);
            xAxis.setLabelLineStyle(IGraphicsTarget.LineStyle.SOLID);
            xAxis.setFontSize(10);

            AxisLabeling axisLabels = new AxisLabeling();
            axisLabels.setLabels(labelVals);

            xAxis.setLabeling(axisLabels);

            setDomainAxis(xAxis);
        }
    }

    /**
     * @return the yValues
     */
    public ArrayList<Float> getYValues() {
        return yValues;
    }

    /**
     * @param values
     *            the yValues to set
     */
    public void setYValues(ArrayList<Float> values) {
        yValues = values;

        NumberAxis yAxis = AxisFactory.buildNumberAxis(
                IAxis.Orientation.HORIZONTAL, 0, 100, "Percent");
        yAxis.setFontSize(10);
        yAxis.setDrawLinesAtLabels(true);
        // axis.addTitle(getName(), new RGB(100, 100, 100));
        yAxis.setLabelLineStyle(IGraphicsTarget.LineStyle.DASHED);
        // axis.setColor(getCapability(ColorableCapability.class).getColor());

        setRangeAxis(yAxis);
    }

    public void addDataSeries(XYDataList newSeries, LineStyle lineType,
            PointType pointType, String label) {
        // Add the data
        dataSeries.add(newSeries);

        // Need to set the line type for the series
        dataSeriesLineTypes.add(lineType);

        // Need to set the point type for the series
        dataSeriesPointTypes.add(pointType);

        // The name that will appear in the legend
        dataSeriesLabels.add(label);
    }

    /**
     * @return the dataSeries
     */
    public ArrayList<XYDataList> getDataSeries() {
        return dataSeries;
    }

    /**
     * @param dataSeries
     *            the dataSeries to set
     */
    public void setDataSeries(ArrayList<XYDataList> dataSeries) {
        System.out.println("Calling setDataSeries with size: "
                + dataSeries.size());
        this.dataSeries = dataSeries;
    }

    /**
     * @return the position
     */
    public GraphPosition getPosition() {
        return position;
    }

    /**
     * @param position
     *            the position to set
     */
    public void setPosition(GraphPosition position) {
        this.position = position;
    }

    public <T> void setCapabilities(T... capabilites) {
        // TODO Auto-generated method stub

        for (T currCap : capabilites) {
            if (currCap instanceof OutlineCapability) {
                outlineCap = (OutlineCapability) currCap;
            } else if (currCap instanceof ColorableCapability) {
                colorCap = (ColorableCapability) currCap;
            }
        }
    }
}
