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
package com.raytheon.uf.viz.xy.graph;

import java.awt.geom.Rectangle2D;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.xy.graph.axis.GraphAxis;
import com.raytheon.uf.viz.xy.graph.axis.IAxis;
import com.raytheon.uf.viz.xy.graph.axis.IAxisPlacer;
import com.raytheon.uf.viz.xy.graph.axis.OutlineAxis;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.util.AbstractGraphZoomHandler;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract graph, contains common functionality
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 06, 2009  3312     mschenke  Initial creation
 * Feb 10, 2011  8244     bkowal    Keep track of the current magnification
 *                                  settings.
 * Jun 18, 2014  3242     njensen   Replace deprecated calls
 * Jul 21, 2015  4220     mapeters  Added zoomHandler
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * Aug 09, 2016  5803     bsteffen  Normalize very small ranges better
 * Aug 12, 2016  5822     bsteffen  Fix NPE
 * 
 * </pre>
 * 
 * @author mschenke
 */
public abstract class AbstractGraph implements IGraph {

    /** Date format */
    protected static final SimpleDateFormat sdf = new SimpleDateFormat(
            "ddMMMyy");

    /** Format for height axis labels */
    protected static final DecimalFormat heightDF = new DecimalFormat("#.##");

    /** x axis placer, returns offsets on extent */
    protected IAxisPlacer xAxisPlacer;

    /** x label placer */
    protected IAxisPlacer yAxisPlacer;

    protected IAxis[] xAxes = new IAxis[0];

    protected IAxis[] yAxes = new IAxis[0];

    /** The resources on the graph */
    protected List<IGraphableResource<?, ?>> graphResource;

    /** Graph descriptor */
    protected XyGraphDescriptor descriptor;

    /** The extent of the graph */
    protected IExtent graphExtent;

    /** Has the graph extent changed? */
    protected boolean extentChanged = false;

    /** Redraw the graph */
    protected boolean redraw = false;

    /** Does the graph have new resources */
    protected boolean newResources = false;

    /** THe wireframe shapes that are the graph */
    protected Map<LineStyle, IWireframeShape> shapes;

    /** The graph target */
    protected IGraphicsTarget target;

    /** The min/max x/y axes */
    protected IAxis minXAxis;

    protected IAxis minYAxis;

    protected IAxis maxXAxis;

    protected IAxis maxYAxis;

    protected IFont unitsFont = null;

    protected IFont titleFont = null;

    /** is the graph visible */
    protected boolean displayed = true;

    protected Double currentMagnification = 1.0;

    protected AbstractGraphZoomHandler zoomHandler;

    public AbstractGraph(XyGraphDescriptor descriptor) {
        this.descriptor = descriptor;
        extentChanged = true;
        graphResource = new ArrayList<IGraphableResource<?, ?>>();
        minXAxis = new OutlineAxis();
        minYAxis = new OutlineAxis();
        maxXAxis = new OutlineAxis();
        maxYAxis = new OutlineAxis();
        shapes = new HashMap<LineStyle, IWireframeShape>();
    }

    /**
     * Add an axis to be drawn
     * 
     * @param axis
     */
    protected void addAxis(IAxis axis) {
        if (!axis.isDrawAxis()) {
            return;
        }
        IWireframeShape shape = shapes.get(axis.getLineStyle());
        if (shape == null) {
            shape = target.createWireframeShape(false, descriptor);
            shapes.put(axis.getLineStyle(), shape);
        }
        Coordinate[] coords = axis.getCoordinates();
        shape.addLineSegment(new double[][] {
                { coords[0].x, coords[0].y, coords[0].z },
                { coords[1].x, coords[1].y, coords[1].z } });
    }

    /**
     * Here we need to create the data axes, called when size of graph has
     * changed (graphExtent)
     */
    private void baseCreateAxes() {
        double minX = graphExtent.getMinX();
        double maxX = graphExtent.getMaxX();
        double minY = graphExtent.getMinY();
        double maxY = graphExtent.getMaxY();

        // Set up the bounding box axes
        minXAxis.setStartLoc(new Coordinate(minX, maxY, 0));
        minXAxis.setEndLoc(new Coordinate(maxX, maxY, 0));

        maxXAxis.setStartLoc(new Coordinate(minX, minY, 0));
        maxXAxis.setEndLoc(new Coordinate(maxX, minY, 0));

        minYAxis.setStartLoc(new Coordinate(minX, maxY, 0));
        minYAxis.setEndLoc(new Coordinate(minX, minY, 0));

        maxYAxis.setStartLoc(new Coordinate(maxX, maxY, 0));
        maxYAxis.setEndLoc(new Coordinate(maxX, minY, 0));
        createAxes();
    }

    /**
     * Construct the graph, called if the graph extent has changed
     */
    protected void constructGraph() {
        baseCreateAxes();
        redraw = true;
    }

    protected void redrawGraph() {
        // dispose and clear the old shapes
        for (LineStyle ls : shapes.keySet()) {
            if (shapes.get(ls) != null) {
                shapes.get(ls).dispose();
            }
        }
        shapes.clear();

        // Put together the outline
        for (IAxis axis : new IAxis[] { minXAxis, maxXAxis, minYAxis, maxYAxis }) {
            addAxis(axis);
        }

        // Add the internal axes (graph specific)
        for (IAxis axis : xAxes) {
            addAxis(axis);
        }
        for (IAxis axis : yAxes) {
            addAxis(axis);
        }

        // Compile the shapes
        for (LineStyle ls : shapes.keySet()) {
            shapes.get(ls).compile();
        }
    }

    private void baseConstructVirtualExtent() {
        if (graphResource.size() == 0) {
            updateVirtualExtent();
            newResources = false;
            return;
        }
        constructVirtualExtent();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.IGraph#addGraphResource(com.raytheon.uf.
     * viz.xy.map.rsc.IGraphableResource)
     */
    @Override
    public void addGraphResource(IGraphableResource<?, ?> rsc) {
        if (canHandleResoruce(rsc)) {
            if (graphResource.contains(rsc) == false) {
                graphResource.add(rsc);
                newResources = true;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.IGraph#getExtent()
     */
    @Override
    public IExtent getExtent() {
        return graphExtent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.IGraph#getGridLocation(double, double)
     */
    @Override
    public double[] getGridLocation(double x, double y) {
        if (graphExtent == null) {
            return null;
        } else {
            return new double[] {
                    graphExtent.getMinX() + yAxisPlacer.getPixelLoc(x),
                    graphExtent.getMaxY() - xAxisPlacer.getPixelLoc(y) };
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.IGraph#getResourceCount()
     */
    @Override
    public int getResourceCount() {
        return graphResource.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.IGraph#getResources()
     */
    @Override
    public List<?> getResources() {
        return graphResource;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.IGraph#getVirtualLocation(double,
     * double)
     */
    @Override
    public double[] getVirtualLocation(double x, double y) {
        if (graphExtent == null) {
            return null;
        } else {
            double xOffset = x - graphExtent.getMinX();
            double yOffset = graphExtent.getMaxY() - y;
            return new double[] { yAxisPlacer.getDataValue(xOffset),
                    xAxisPlacer.getDataValue(yOffset) };
        }
    }

    /**
     * Given a min and a max, divide into 10 parts and normalize the endpoitns
     * based on a logical increment
     * 
     * @param minMax
     *            an array where the 1st element is a min and the second is a
     *            max
     */
    protected static void normalizeAxis(double[] minMax) {
        if (minMax.length != 2) {
            throw new IllegalArgumentException("Expecting array of length 2");
        }

        double min = minMax[0];
        double max = minMax[1];
        double range = max - min;
        double midPoint = min + range / 2;
        double inc = determineIncrement(range);

        double roundedMidPoint = inc * Math.round(midPoint / inc);
        double roundedMin = roundedMidPoint - 5 * inc;
        double roundedMax = roundedMidPoint + 5 * inc;

        // Never cross zero if the original bounds did not cross zero.
        if (roundedMin < 0 && min >= 0) {
            roundedMax -= roundedMin;
            roundedMin = 0;
        }
        if (roundedMax > 0 && max <= 0) {
            roundedMin -= roundedMax;
            roundedMax = 0;
        }
        minMax[0] = roundedMin;
        minMax[1] = roundedMax;

    }

    /**
     * Given a distance determine some increment that evenly divides the space
     * into roundish numbers. This method attempts to come up with an increment
     * that divides the distance somewhere between 5 and 10 times depending on
     * how evenly the distance can be divided into roundish increments.
     * 
     * @param distance the distance to divide
     * @return a roundish number for dividing distance into 5 to 10 increments.
     */
    protected static double determineIncrement(double distance) {
        /*
         * This will determine the largest power of 10 that is smaller than
         * distance. Powers of 10 are the ideal increment because of how nicely
         * they can be formatted for the user. And even when they aren't the
         * best increment it serves as a good starting point. This effectively
         * normalizes the exponent for the rest of the code. The rest of the
         * comments give example output assuming the distance is between 100 and
         * 999, but the algorithm actually works on any value, the increment
         * would just be scaled to match the exponent of the distance. For
         * example a distance of 432.1 results in an increment of 50 if the
         * distance is something more like 43.21 the increment is scaled
         * similarly to 5 and a distance 0.0004321 would result in an increment
         * of 0.00005.
         */
        double inc = Math.pow(10, Math.floor(Math.log10(distance)));
        if (distance / inc > 4.5) {
            /* Values from 451 to 999 use an increment of 100. */
            return inc;
        } else if (distance / inc > 2.5) {
            /* Values from 251 to 450 use an increment of 50. */
            return inc / 2;
        } else {
            /* Values from 100 to 250 use an increment of 25. */
            return inc / 4;
        }
    }

    @Override
    public boolean isDisplayed() {
        return displayed;
    }

    @Override
    public void setDisplayed(boolean displayed) {
        this.displayed = displayed;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.IGraph#isReady()
     */
    @Override
    public boolean isReady() {
        return !extentChanged && isDisplayed();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.IGraph#removeGraphResource(com.raytheon.
     * uf.viz.xy.map.rsc.IGraphableResource)
     */
    @Override
    public void removeGraphResource(IGraphableResource<?, ?> rsc) {
        graphResource.remove(rsc);
        newResources = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.IGraph#updateExtent(com.raytheon.uf.viz.
     * core.IExtent)
     */
    @Override
    public void updateExtent(IExtent extent) {
        this.graphExtent = extent;
        extentChanged = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.IGraph#updateVirtualExtent(com.raytheon.
     * uf.viz.core.IExtent)
     */
    @Override
    public void updateVirtualExtent() {
        redraw = true;
        for (IGraphableResource<?, ?> rsc : graphResource) {
            rsc.redraw();
        }
        createAxes();
        target.setNeedsRefresh(true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        this.target = target;
        if (newResources) {
            baseConstructVirtualExtent();
        }

        if (extentChanged) {
            constructGraph();
            extentChanged = false;
        }

        if (redraw) {
            redrawGraph();
            redraw = false;
        }

        if (unitsFont == null) {
            unitsFont = target.initializeFont((String) null, 8,
                    new IFont.Style[] {});
        }

        if (titleFont == null) {
            titleFont = target.initializeFont((String) null, 9,
                    new IFont.Style[] { IFont.Style.BOLD });
        }

        target.setupClippingPlane(graphExtent);

        RGB graphColor = descriptor.getGraphResource()
                .getCapability(ColorableCapability.class).getColor();
        // Paint the graph axes
        for (Entry<LineStyle, IWireframeShape> entry : shapes.entrySet()) {
            target.drawWireframeShape(entry.getValue(), graphColor, 1.0f,
                    entry.getKey());
        }

        target.clearClippingPlane();
        paintTitles(target, paintProps);
        paintUnits(target, paintProps);
    }

    @Override
    public void dispose() {
        for (IWireframeShape shape : shapes.values()) {
            if (shape != null) {
                shape.dispose();
            }
        }
        shapes.clear();
        unitsFont.dispose();
        unitsFont = null;
        titleFont.dispose();
        titleFont = null;
    }

    @Override
    public void reconstruct() {
        // easiest way to get graph to reconstruct
        newResources = true;
        extentChanged = true;
    }

    /**
     * Call this to paint the title in vertically on the target in the color,
     * for multiple titles increment the index to shift each over
     * 
     * @param target
     * @param paintProps
     * @param title
     * @param titleColor
     * @param index
     * @throws VizException
     */
    protected void paintYTitle(IGraphicsTarget target,
            PaintProperties paintProps, String title, RGB titleColor, int index)
            throws VizException {
        // Paint the titles
        double ratio = paintProps.getCanvasBounds().height
                / paintProps.getView().getExtent().getHeight();
        DrawableString titleString = new DrawableString(title, titleColor);
        titleString.addTextStyle(TextStyle.DROP_SHADOW);
        titleString.horizontalAlignment = HorizontalAlignment.LEFT;
        titleString.verticallAlignment = VerticalAlignment.BOTTOM;
        titleString.rotation = 90;
        titleString.magnification = this.currentMagnification;
        int width = target.getStringsBounds(titleString).getBounds().width;
        int height = target.getStringsBounds(titleString, "H").getBounds().height * 2;
        double x = graphExtent.getMinX() - 75 - height * (index);
        double y = graphExtent.getMaxY()
                - ((graphExtent.getHeight() - (width / ratio)) / 2);
        titleString.setCoordinates(x, y);

        target.drawStrings(titleString);
    }

    /**
     * For graphs with times along the x axis, call this to paint the times
     * 
     * @param target
     * @param paintProps
     * @param xLabels
     * @throws VizException
     */
    protected void paintDataTimeUnits(IGraphicsTarget target,
            PaintProperties paintProps, List<IGraphLabel<DataTime>> xLabels)
            throws VizException {
        double ratio = paintProps.getCanvasBounds().height
                / paintProps.getView().getExtent().getHeight();

        double labelsHeight = 0;
        double offset = 75;
        int dayOfYear = -1;
        double prevUsedOffset = 0;
        int j = 0;
        int i = 0;

        List<DrawableString> strings = new ArrayList<DrawableString>(
                xLabels.size());

        for (IGraphLabel<DataTime> xLabel : xLabels) {
            double val = xLabel.getDiscreteValue();
            if (val < yAxisPlacer.getMinDataValue()
                    && val < yAxisPlacer.getMaxDataValue()) {
                continue;
            }
            if (val > yAxisPlacer.getMinDataValue()
                    && val > yAxisPlacer.getMaxDataValue()) {
                continue;
            }

            offset = yAxisPlacer.getPixelLoc(val);
            Coordinate loc = new Coordinate(graphExtent.getMinX() + offset,
                    graphExtent.getMaxY(), 0);
            String label = xLabel.toLabelString();
            Calendar cal = xLabel.getUnderlyingObject().getValidTime();
            int day = cal.get(Calendar.DAY_OF_YEAR);
            if (day != dayOfYear) {
                sdf.setCalendar(cal);
                label += IGraphLabel.DELIMETER + sdf.format(cal.getTime());
            }
            ResourceProperties props = xLabel.getResource().getProperties();
            if (props != null && !props.isVisible()) {
                continue;
            }

            RGB labelColor = xLabel.getResource()
                    .getCapability(ColorableCapability.class).getColor();
            String[] labels = label.split(IGraphLabel.DELIMETER);
            RGB[] rgbs = new RGB[labels.length];
            for (i = 0; i < rgbs.length; ++i) {
                rgbs[i] = labelColor;
            }

            DrawableString parameters = null;

            double diff = Math.abs(offset - prevUsedOffset) * ratio
                    / this.currentMagnification;
            if (j == 0 || diff > 43) {
                dayOfYear = day;
                prevUsedOffset = offset;
                parameters = new DrawableString(labels, rgbs);
            } else {
                parameters = new DrawableString(new String[] { labels[0] },
                        new RGB[] { rgbs[0] });
            }

            parameters.font = unitsFont;
            parameters.horizontalAlignment = HorizontalAlignment.CENTER;
            parameters.verticallAlignment = VerticalAlignment.TOP;
            parameters.magnification = this.currentMagnification;

            if (labelsHeight == 0) {
                labelsHeight = target.getStringsBounds(parameters).getHeight()
                        / ratio;
            }
            if (loc.y + labelsHeight > paintProps.getView().getExtent()
                    .getMaxY()) {
                loc = new Coordinate(loc.x, paintProps.getView().getExtent()
                        .getMaxY()
                        - labelsHeight, loc.z);
            }

            // Do the label spacing for datatimes
            parameters.setCoordinates(loc.x, loc.y, loc.z);
            strings.add(parameters);
            ++j;
        }
        target.drawStrings(strings);
    }

    /**
     * For height graphs, call this to paint the height units on both sides of
     * the graph
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    protected void paintHeightUnits(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        RGB graphColor = descriptor.getGraphResource()
                .getCapability(ColorableCapability.class).getColor();

        double ratio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();

        double padding = 3 / ratio;

        List<DrawableString> labels = new ArrayList<DrawableString>(
                xAxes.length * 2);

        IExtent viewExtent = paintProps.getView().getExtent();

        for (IAxis axis : xAxes) {

            Coordinate coord = axis.getCoordinates()[0];
            if (coord.y < graphExtent.getMinY()
                    || coord.y > graphExtent.getMaxY()
                    || coord.y < viewExtent.getMinY()
                    || coord.y > viewExtent.getMaxY()) {
                continue;
            }
            String value = heightDF.format(axis.getDiscreteValue());
            DrawableString label1 = new DrawableString(value, graphColor);
            DrawableString label2 = new DrawableString(value, graphColor);
            label1.magnification = this.currentMagnification;
            label2.magnification = this.currentMagnification;
            label1.font = unitsFont;
            label2.font = unitsFont;
            label1.verticallAlignment = VerticalAlignment.MIDDLE;
            label2.verticallAlignment = VerticalAlignment.MIDDLE;
            label1.horizontalAlignment = HorizontalAlignment.RIGHT;
            label2.horizontalAlignment = HorizontalAlignment.LEFT;
            Rectangle2D bounds = target.getStringsBounds(label1);

            label1.setCoordinates(coord.x, coord.y, coord.z);
            if (label1.basics.y + bounds.getHeight() / 2 > graphExtent
                    .getMaxY()) {
                label1.verticallAlignment = VerticalAlignment.BOTTOM;
                label2.verticallAlignment = VerticalAlignment.BOTTOM;
            }
            if (label1.basics.y - bounds.getHeight() / 2 < graphExtent
                    .getMinY()) {
                label1.verticallAlignment = VerticalAlignment.TOP;
                label2.verticallAlignment = VerticalAlignment.TOP;
            }
            if (label1.basics.x - bounds.getWidth() / ratio < viewExtent
                    .getMinX()) {
                label1.horizontalAlignment = HorizontalAlignment.LEFT;
                label1.basics.x = viewExtent.getMinX() + padding;
            }

            coord = axis.getCoordinates()[1];
            label2.setCoordinates(coord.x, coord.y, coord.z);
            if (label2.basics.x + bounds.getWidth() / ratio > viewExtent
                    .getMaxX()) {
                label2.horizontalAlignment = HorizontalAlignment.RIGHT;
                label2.basics.x = viewExtent.getMaxX() - padding;
            }

            labels.add(label1);
            labels.add(label2);
        }
        target.drawStrings(labels);
    }

    /**
     * For height graphs call this to create properly spaces xAxes
     * 
     * @param heightScale
     * @param zoomLevel
     */
    protected void createHeightAxis(HeightScale heightScale, double zoomLevel) {
        xAxisPlacer.setPixelWidth(graphExtent.getHeight());
        if (heightScale.getLabels() == null) {
            double range = Math.abs(heightScale.getMaxVal()
                    - heightScale.getMinVal());
            double inc = determineIncrement(range);
            inc /= zoomLevel;
            int length = (int) Math.abs(range / inc);

            // Create the Axis if they do not exist
            if (xAxes.length != length + 1) {
                xAxes = new IAxis[length + 1];
                for (int i = 0; i < xAxes.length; ++i) {
                    xAxes[i] = new GraphAxis();
                    xAxes[i].setLineStyle(LineStyle.SOLID);
                    xAxes[i].setDrawAxis(true);
                }
            }

            // Update the values
            double val = Math.min(heightScale.getMinVal(),
                    heightScale.getMaxVal());

            for (int i = 0; i < xAxes.length; i++) {
                xAxes[i].setDiscreteValue(val + inc * i);
            }
        } else {
            List<Double> labels = heightScale.getLabelsList();
            // zoomLevel will be a power of 2. (1,2,4,8,16).
            zoomLevel = Math.pow(2,
                    Math.round(Math.log(zoomLevel) / Math.log(2)));
            int mult = (int) Math.round(zoomLevel);
            int length = labels.size() * mult - mult + 1;

            // Create the Axis if they do not exist
            if (xAxes.length != length + 1) {
                xAxes = new IAxis[length + 1];
                for (int i = 0; i < xAxes.length; ++i) {
                    xAxes[i] = new GraphAxis();
                    xAxes[i].setLineStyle(LineStyle.SOLID);
                    xAxes[i].setDrawAxis(true);
                }
            }

            double lastVal = labels.get(0);
            xAxes[0].setDiscreteValue(lastVal);
            for (int i = 1; i < labels.size(); i++) {
                double val = labels.get(i);
                double inc = (val - lastVal) / mult;
                for (int j = 1; j <= mult; j++) {
                    xAxes[(i - 1) * mult + j].setDiscreteValue(lastVal + inc
                            * j);
                }
                lastVal = val;
            }
        }

        // Place them
        double minX = graphExtent.getMinX();
        double maxX = graphExtent.getMaxX();
        double maxY = graphExtent.getMaxY();

        // Place the data axes
        double[] offsets = xAxisPlacer.placeAxes(xAxes);

        for (int i = 0; i < offsets.length; ++i) {
            double offset = offsets[i];
            xAxes[i].setStartLoc(new Coordinate(minX, maxY - offset, 0));
            xAxes[i].setEndLoc(new Coordinate(maxX, maxY - offset, 0));
        }
    }

    /**
     * Fill each of the lists with graph labels from the resources. All
     * resources must implement IGraphableResource<X, Y>. Lists will be sorted
     * before returning.
     * 
     * @param <X>
     * @param <Y>
     * @param xLabels
     * @param yLabels
     */
    @SuppressWarnings("unchecked")
    protected <X, Y> void getRangeData(List<IGraphLabel<X>> xLabels,
            List<IGraphLabel<Y>> yLabels) {
        for (IGraphableResource<?, ?> grsc : graphResource) {
            IGraphableResource<X, Y> rsc = (IGraphableResource<X, Y>) grsc;
            if (rsc.getXRangeData().length > 0) {
                IGraphLabel<Y>[] yVals = rsc.getYRangeData();
                for (IGraphLabel<Y> val : yVals) {
                    val.setResource((AbstractVizResource<?, ?>) rsc);
                    yLabels.add(val);
                }

                IGraphLabel<X>[] xVals = rsc.getXRangeData();
                for (IGraphLabel<X> val : xVals) {
                    val.setResource((AbstractVizResource<?, ?>) rsc);
                    xLabels.add(val);
                }
            }
        }
        Collections.sort(xLabels, new GraphLabelComparator());
        Collections.sort(yLabels, new GraphLabelComparator());
    }

    /**
     * Paint the graph's titles
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    protected abstract void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException;

    /**
     * Paint the graphs units
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    protected abstract void paintUnits(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException;

    /**
     * Check the rsc attempting to be added to the graph to determine if the
     * graph is capable is displaying the resource
     * 
     * @param rsc
     * @return
     */
    protected abstract boolean canHandleResoruce(IGraphableResource<?, ?> rsc);

    /**
     * Based on the resources loaded, calculate and construct the graph's
     * virtual extent
     */
    protected abstract void constructVirtualExtent();

    /**
     * Create the graph's axes
     */
    protected abstract void createAxes();

    @Override
    public void setCurrentMagnification(Double currentMagnification) {
        this.currentMagnification = currentMagnification;
    }

    @Override
    public AbstractGraphZoomHandler getZoomHandler() {
        return zoomHandler;
    }

    @Override
    public void setZoomHandler(AbstractGraphZoomHandler zoomHandler) {
        this.zoomHandler = zoomHandler;
    }
}
