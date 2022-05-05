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
package com.raytheon.uf.viz.xy.crosssection.graph;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.level.Level.LevelType;
import com.raytheon.uf.common.topo.CachedTopoQuery;
import com.raytheon.uf.common.wxmath.Hgt2Pres;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.rsc.AbstractCrossSectionResource;
import com.raytheon.uf.viz.xy.graph.AbstractGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.graph.axis.LinearAxisPlacer;
import com.raytheon.uf.viz.xy.graph.axis.LogarithmicAxisPlacer;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.scales.HeightScale.ScaleType;
import com.raytheon.viz.core.map.GeoUtil;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.Point;

/**
 *
 * Creates a cross section on a graph display.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jul 03, 2010           bsteffen  Initial creation
 * Feb 15, 2013  1638     mschenke  Got rid of viz/edex topo classes
 *                                  and moved into common
 * Aug 13, 2013  2262     dgilling  Use new wxmath hgt2pres method.
 * Jun 14, 2014  3242     njensen   Null safety checks
 * Oct 27, 2015  5051     bsteffen  Use cached topo
 * Nov 05, 2015  5070     randerso  Adjust font sizes for dpi scaling
 * Jul 24, 2017  6048     mapeters  Prevent possible error in constructVirtualExtent()
 *                                  when resources have no datatimes
 *
 * </pre>
 *
 * @author bsteffen
 */
public class CrossSectionGraph extends AbstractGraph {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrossSectionGraph.class);

    private Map<LineString, IWireframeShape> topoLines = new HashMap<>();

    private Map<LineString, double[]> topoData = new HashMap<>();

    private Map<LineString, String[]> cityData = new HashMap<>();

    private int zoomLevel = 1;

    /**
     * @param descriptor
     */
    public CrossSectionGraph(XyGraphDescriptor descriptor) {
        super(descriptor);
    }

    @Override
    protected boolean canHandleResoruce(IGraphableResource<?, ?> rsc) {
        return rsc instanceof AbstractCrossSectionResource;
    }

    @Override
    protected void constructVirtualExtent() {
        ArrayList<IGraphLabel<Double>> xLabels = new ArrayList<>();
        getRangeData(xLabels, new ArrayList<IGraphLabel<Double>>());

        double minX, maxX;
        if (!xLabels.isEmpty()) {
            minX = xLabels.get(0).getDiscreteValue();
            maxX = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        } else {
            // Setting min/max to the same value causes no labels to be shown
            minX = 0;
            maxX = 0;
        }

        HeightScale heightScale = ((CrossSectionDescriptor) descriptor)
                .getHeightScale();
        if (heightScale.getScale() == ScaleType.LOG) {
            xAxisPlacer = new LogarithmicAxisPlacer(graphExtent.getHeight(),
                    heightScale.getMinVal(), heightScale.getMaxVal());
        } else {
            xAxisPlacer = new LinearAxisPlacer(graphExtent.getHeight(),
                    heightScale.getMinVal(), heightScale.getMaxVal());
        }
        yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(), minX, maxX);

        updateVirtualExtent();
        for (IWireframeShape shape : topoLines.values()) {
            shape.dispose();
        }
        topoLines.clear();
        topoData.clear();
        newResources = false;

    }

    @Override
    protected void createAxes() {
        if (yAxisPlacer != null && xAxisPlacer != null) {
            yAxisPlacer.setPixelWidth(graphExtent.getWidth());

            createHeightAxis(
                    ((CrossSectionDescriptor) descriptor).getHeightScale(),
                    zoomLevel);
        }
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if ((1 / paintProps.getZoomLevel()) != zoomLevel) {
            zoomLevel = (int) (1 / paintProps.getZoomLevel());
            createAxes();
            redraw = true;
        }
        super.paint(target, paintProps);
    }

    @Override
    protected void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (titleFont == null) {
            titleFont = target.initializeFont((String) null, 9,
                    new IFont.Style[] { IFont.Style.BOLD });
        }

        // Paint height axis title
        RGB titleColor = descriptor.getGraphResource()
                .getCapability(ColorableCapability.class).getColor();
        String title = ((CrossSectionDescriptor) descriptor).getHeightScale()
                .getUnit();
        paintYTitle(target, paintProps, title, titleColor, 0);

        // paint Distance Title
        double ratio = paintProps.getCanvasBounds().height
                / paintProps.getView().getExtent().getHeight();

        double x = graphExtent.getCenter()[0];
        // get height of the units strings
        DrawableString testString = new DrawableString("H", null);
        testString.magnification = currentMagnification;
        double unitsHeight = target.getStringsBounds(testString).getHeight();
        // Leave 4.5 lines for the cities labels and the distance labels
        double y = graphExtent.getMaxY() + ((unitsHeight / ratio) * 4.5);
        if (y > paintProps.getView().getExtent().getMaxY()) {
            y = paintProps.getView().getExtent().getMaxY();
        }
        DrawableString titleString = new DrawableString("Distance (km)",
                titleColor);
        titleString.font = titleFont;
        titleString.setCoordinates(x, y);
        titleString.addTextStyle(TextStyle.DROP_SHADOW);
        titleString.horizontalAlignment = HorizontalAlignment.LEFT;
        titleString.verticallAlignment = VerticalAlignment.BOTTOM;
        titleString.magnification = currentMagnification;
        target.drawStrings(titleString);

    }

    @Override
    protected void paintUnits(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (unitsFont == null) {
            unitsFont = target.initializeFont((String) null, 8,
                    new IFont.Style[] {});
        }

        paintHeightUnits(target, paintProps);

        paintTopoLine(target);

        paintCities(target, paintProps);

        paintDistances(target, paintProps);

    }

    @Override
    public void dispose() {
        for (IWireframeShape shape : topoLines.values()) {
            shape.dispose();
        }
        topoLines.clear();
        super.dispose();
    }

    protected void paintCities(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        RGB graphColor = ((CrossSectionDescriptor) descriptor)
                .getGraphResource().getCapability(ColorableCapability.class)
                .getColor();

        LineString line = ((CrossSectionDescriptor) descriptor)
                .getCurrentLine();

        // return if there are no frames
        if (line == null) {
            return;
        }

        String[] cities = getCities(line);

        double ratio = paintProps.getCanvasBounds().height
                / paintProps.getView().getExtent().getHeight();

        // get height of the units strings
        DrawableString testString = new DrawableString("H", null);
        testString.magnification = currentMagnification;
        double height = target.getStringsBounds(testString).getHeight() / ratio;

        double y = graphExtent.getMaxY() + height;
        // Leave space for the distance labels below this
        if (y > (paintProps.getView().getExtent().getMaxY() - (height * 3.5))) {
            y = paintProps.getView().getExtent().getMaxY() - (height * 3.5);
        }
        List<DrawableString> labels = new ArrayList<>();
        for (int i = 0; i < cities.length; i++) {
            if (cities[i] == null) {
                continue;
            }
            double x = graphExtent.getMinX()
                    + ((graphExtent.getWidth() / (cities.length - 1)) * i);
            DrawableString caret = new DrawableString("^", graphColor);
            DrawableString city = new DrawableString(cities[i], graphColor);
            caret.setCoordinates(x, y);
            city.setCoordinates(x, y);
            caret.font = city.font = unitsFont;
            city.addTextStyle(TextStyle.BLANKED);
            caret.horizontalAlignment = city.horizontalAlignment = HorizontalAlignment.CENTER;
            caret.verticallAlignment = VerticalAlignment.BOTTOM;
            city.verticallAlignment = VerticalAlignment.MIDDLE;
            caret.magnification = city.magnification = currentMagnification;
            labels.add(caret);
            labels.add(city);
        }
        target.drawStrings(labels);
    }

    protected void paintDistances(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        double ratio = paintProps.getCanvasBounds().height
                / paintProps.getView().getExtent().getHeight();
        // get height of the units strings
        DrawableString testString = new DrawableString("H", null);
        testString.magnification = currentMagnification;
        double height = target.getStringsBounds(testString).getHeight() / ratio;

        RGB graphColor = ((CrossSectionDescriptor) descriptor)
                .getGraphResource().getCapability(ColorableCapability.class)
                .getColor();
        double inc = determineIncrement(yAxisPlacer.getDataWidth());
        // Leave space for the cities labels above and the Distance label below
        double y = graphExtent.getMaxY() + (height * 2);
        if (y > (paintProps.getView().getExtent().getMaxY() - (height * 2.5))) {
            y = paintProps.getView().getExtent().getMaxY() - (height * 2.5);
        }
        List<DrawableString> labels = new ArrayList<>();
        for (double d = yAxisPlacer.getMinDataValue(); d <= yAxisPlacer
                .getMaxDataValue(); d += inc) {
            String label = Integer.toString((int) Math.round(d / 1000));
            double x = graphExtent.getMinX() + yAxisPlacer.getPixelLoc(d);
            DrawableString dString = new DrawableString(label, graphColor);
            dString.font = unitsFont;
            dString.setCoordinates(x, y);
            dString.horizontalAlignment = HorizontalAlignment.CENTER;
            dString.verticallAlignment = VerticalAlignment.TOP;
            dString.magnification = currentMagnification;
            labels.add(dString);
        }
        target.drawStrings(labels);
    }

    protected void paintTopoLine(IGraphicsTarget target) throws VizException {
        // Draw topo graph

        RGB graphColor = ((CrossSectionDescriptor) descriptor)
                .getGraphResource().getCapability(ColorableCapability.class)
                .getColor();

        LineString line = ((CrossSectionDescriptor) descriptor)
                .getCurrentLine();

        // return if there are no frames
        if (line == null) {
            return;
        }

        IWireframeShape shape = topoLines.get(line);
        if (shape == null) {
            // try to use already generated topo
            double[] topoData = this.topoData.get(line);
            if (topoData == null) {
                topoData = getTopoData(line, 100);
            }
            shape = target.createWireframeShape(false, descriptor);
            double[][] lineSegment = new double[topoData.length][2];
            for (int i = 0; i < topoData.length; i++) {
                lineSegment[i][0] = graphExtent.getMinX()
                        + ((i * graphExtent.getWidth())
                                / (topoData.length - 1));
                lineSegment[i][1] = topoData[i];
                lineSegment[i][1] = Math.min(lineSegment[i][1],
                        graphExtent.getMaxY());
                if ((i % 2) == 0) {
                    double[][] bar = new double[2][2];
                    bar[0] = lineSegment[i];
                    bar[1][0] = lineSegment[i][0];
                    bar[1][1] = graphExtent.getMaxY();
                    shape.addLineSegment(bar);
                }
            }
            shape.addLineSegment(lineSegment);
            topoLines.put(line, shape);
        }
        target.drawWireframeShape(shape, graphColor, 1.0f);
    }

    public double[] getTopoData(LineString line, int numPoints) {
        if (topoData.containsKey(line)
                && (topoData.get(line).length == numPoints)) {
            return topoData.get(line);
        }

        CrossSectionDescriptor csDesc = (CrossSectionDescriptor) descriptor;

        double[] heights;
        switch (csDesc.getHeightScale().getHeightType()) {
        case HEIGHT_MSL:
        case PRESSURE:
            Coordinate[] lineData;
            lineData = GeoUtil.splitLine(numPoints, line.getCoordinates());

            heights = CachedTopoQuery.getInstance().getHeight(lineData);

            if (csDesc.getHeightScale().getHeightType() == LevelType.PRESSURE) {
                for (int i = 0; i < heights.length; i++) {
                    heights[i] = Hgt2Pres.hgt2pres((float) heights[i]);
                }
            } else {
                for (int i = 0; i < heights.length; i++) {
                    heights[i] = heights[i] / 1000;
                }
            }
            break;
        case HEIGHT_AGL:
            heights = new double[numPoints];
            for (int i = 0; i < heights.length; i++) {
                heights[i] = csDesc.getHeightScale().getMinVal();
            }
            break;
        default:
            statusHandler.handle(Priority.PROBLEM,
                    "Unrecognized Height Type for Cross Section Topo: "
                            + csDesc.getHeightScale().getHeightType());
            return new double[numPoints];
        }
        for (int i = 0; i < heights.length; i++) {
            heights[i] = graphExtent.getMaxY()
                    - xAxisPlacer.getPixelLoc(heights[i]);
        }
        topoData.put(line, heights);
        return heights;
    }

    protected String[] getCities(LineString line) {
        String[] cities = cityData.get(line);
        if (cities == null) {
            Coordinate[] cityCoords;
            cityCoords = GeoUtil.splitLine(5, line.getCoordinates());

            cities = new String[cityCoords.length];

            // city labels
            GeometryFactory gf = new GeometryFactory();
            for (int i = 0; i < cityCoords.length; i++) {
                cities[i] = GeoUtil.formatCoordinate(cityCoords[i]);
                double tolerance = 2.0;
                Envelope env = new Envelope(cityCoords[i].x - tolerance,
                        cityCoords[i].x + tolerance,
                        cityCoords[i].y - tolerance,
                        cityCoords[i].y + tolerance);
                Geometry geom = gf.toGeometry(env);

                Map<String, String> map = new HashMap<>();
                map.put("WARNGENLEV", "1");
                boolean exclusive = false;
                SpatialQueryResult[] features;
                try {
                    features = SpatialQueryFactory.create().query("City",
                            new String[] { "NAME", "ST" }, geom, map, exclusive,
                            SearchMode.CONTAINS);
                } catch (SpatialException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error occured requesting City data, cities will be unavailable.",
                            e);
                    return new String[0];
                }

                GeodeticCalculator gc = new GeodeticCalculator();
                double maxDistance = Double.MAX_VALUE;

                for (SpatialQueryResult feature : features) {
                    Geometry g = feature.geometry;
                    Point point = g.getCentroid();
                    gc.setStartingGeographicPoint(point.getX(), point.getY());
                    gc.setDestinationGeographicPoint(cityCoords[i].x,
                            cityCoords[i].y);
                    // curDist : distance in meters
                    double curDist = gc.getOrthodromicDistance();
                    if (curDist < maxDistance) {
                        String name = (String) feature.attributes.get("NAME");
                        String state = (String) feature.attributes.get("ST");
                        double distanceInKm = curDist / 1000.0;
                        String dir = GeoUtil.azimuthToString(GeoUtil
                                .roundAzimuth16Directions(gc.getAzimuth()),
                                true);
                        maxDistance = curDist;
                        cities[i] = new DecimalFormat("0").format(distanceInKm)
                                + dir + " " + name + " " + state;
                    }
                }
            }
            cityData.put(line, cities);
        }
        return cities;
    }

    @Override
    public void zoom(int index, Coordinate gridCoord) {
        // Not handled by this
    }

    @Override
    public void pan(double xDist, double yDist, boolean panning) {
        // Not handled by this
    }
}
