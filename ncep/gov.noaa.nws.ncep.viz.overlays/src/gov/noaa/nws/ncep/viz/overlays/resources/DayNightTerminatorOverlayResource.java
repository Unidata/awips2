package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.ui.pgen.display.DisplayElementFactory;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IDisplayable;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Implements a drawing layer to draw day/night terminator line, sub-solar point
 * and the midnight meridian line.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/24/14     1130       S. Gurung	Initial Creation
 * 
 * </pre>
 * 
 * @author sgurung
 * 
 */
public class DayNightTerminatorOverlayResource
        extends
        AbstractVizResource<DayNightTerminatorOverlayResourceData, IMapDescriptor>
        implements INatlCntrsResource {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DayNightTerminatorOverlayResource.class);

    private DayNightTerminatorOverlayResourceData dayNightTermOverlayResourceData;

    /** Map containing wireframeShape objects for drawing terminator line */
    private HashMap<Date, IWireframeShape> wireframeShapesMap = new HashMap<Date, IWireframeShape>();

    /** Map containing coordinates for the terminator line */
    private HashMap<Date, Coordinate[]> terminatorCoordsMap = new HashMap<Date, Coordinate[]>();

    private double offset = 0;

    private double viewMinX;

    private double viewMaxY;

    private double viewMinY;

    private double viewMaxX;

    private boolean needsUpdate = true;

    private double sun_lat_gse;

    private double sun_long_gse;

    private Coordinate sunPosCoord;

    protected DayNightTerminatorOverlayResource(
            DayNightTerminatorOverlayResourceData llRscData,
            LoadProperties props) {
        super(llRscData, props);
        this.dayNightTermOverlayResourceData = llRscData;
    }

    private void initializeViewMinAndMaxXAndY(PaintProperties paintProps) {
        viewMinX = paintProps.getView().getExtent().getMinX();
        viewMaxX = paintProps.getView().getExtent().getMaxX();
        viewMinY = paintProps.getView().getExtent().getMinY();
        viewMaxY = paintProps.getView().getExtent().getMaxY();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    public void disposeInternal() {
        clearWireFrameShapesArray(wireframeShapesMap);
    }

    private void clearWireFrameShapesArray(
            HashMap<Date, IWireframeShape> wireframeShapesMap) {

        for (Date date : wireframeShapesMap.keySet()) {
            IWireframeShape wireframeShape = wireframeShapesMap.get(date);
            if (wireframeShape != null) {
                wireframeShape.dispose();
                wireframeShape = null;
            }
        }
    }

    public double getViewMinX() {
        return viewMinX;
    }

    public double getViewMaxY() {
        return viewMaxY;
    }

    public double getViewMinY() {
        return viewMinY;
    }

    public double getViewMaxX() {
        return viewMaxX;
    }

    @Override
    public void resourceAttrsModified() {
        needsUpdate = true;
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        needsUpdate = true;
    }

    /***
     * Draws a shaded polygon with the given line color, line style and line
     * width Also, clips the polygons, if necessary.
     * 
     * @param polygonCoordinatesList
     *            - the list of polygon coordinates in Lat/Lon
     * @param target
     *            - the graphics target
     * @param color
     *            - the color of the polygon in RGB
     * @param alpha
     *            - the alpha value
     * @throws VizException
     */
    private void drawShadedPolygon(Coordinate[] polygonCoordinatesList,
            IGraphicsTarget target, RGB color, float alpha) throws VizException {

        if (polygonCoordinatesList != null) {
            IWireframeShape wireframeShape = target.createWireframeShape(false,
                    descriptor, 4.0f, false, new PixelExtent(getViewMinX()
                            + offset, getViewMaxX() - offset, getViewMinY()
                            + offset, getViewMaxY() - offset));
            // JTSCompiler jtsCompiler = new JTSCompiler(null, wireframeShape,
            // descriptor);
            GeometryFactory gf = new GeometryFactory();
            LineString ls = gf.createLineString(polygonCoordinatesList);

            // jtsCompiler.handle(ls, color, true);
            // wireframeShape.compile();
            // target.drawWireframeShape(wireframeShape,
            // dayNightTermOverlayResourceData.getTermLineColor(),
            // dayNightTermOverlayResourceData.getTermLineWidth(),
            // dayNightTermOverlayResourceData.getTermLineStyle());
            IShadedShape shadedShape = target.createShadedShape(false,
                    descriptor.getGridGeometry(), true);
            shadedShape.addPolygonPixelSpace(new LineString[] { ls }, color);
            shadedShape.addPolygon(new LineString[] { ls }, color);
            shadedShape.compile();

            if (getFillPatternFromString(dayNightTermOverlayResourceData
                    .getShadePattern()) != FillPattern.TRANSPARENCY
                    && getFillPatternFromString(dayNightTermOverlayResourceData
                            .getShadePattern()) != FillPattern.SOLID) {
                FillPatternList fpl = FillPatternList.getInstance();
                byte[] fpattern = fpl
                        .getFillPattern(getFillPatternFromString(dayNightTermOverlayResourceData
                                .getShadePattern()));
                shadedShape.setFillPattern(fpattern);
            }
            target.drawShadedShape(shadedShape, alpha);
            wireframeShape.dispose();
        }

    }

    /**
     * Returns the fill pattern given a string.
     */
    public FillPattern getFillPatternFromString(String fillPattern) {

        return FillPattern.valueOf(fillPattern);
    }

    /***
     * Checks whether a point lies within a polygon.
     * 
     * @param polygonCoordList
     *            - the coordinates for the main polygon
     * @param toCheckCoord
     *            - the coordinates for the point
     * @return boolean - whether a point lies within a polygon
     */
    public boolean isPointWithinPolygon(Coordinate[] polygonCoordList,
            Coordinate toCheckCoord) {
        int[] xpoints = new int[polygonCoordList.length];
        int[] ypoints = new int[polygonCoordList.length];
        for (int ii = 0; ii < polygonCoordList.length; ii++) {
            xpoints[ii] = (int) polygonCoordList[ii].x;
            ypoints[ii] = (int) polygonCoordList[ii].y;
        }

        java.awt.Polygon poly = new java.awt.Polygon(xpoints, ypoints,
                polygonCoordList.length);

        return poly.contains(toCheckCoord.x, toCheckCoord.y);

    }

    /***
     * Checks whether a polygon is within another polygon.
     * 
     * @param mainPolygonCoordList
     *            - the coordinates for the main polygon
     * @param toCheckPolygonCoordList
     *            - the coordinates for the smaller polygon
     * @return boolean - whether the polygon lies within another polygon
     */
    public boolean isPolygonWithinPolygon(Coordinate[] mainPolygonCoordList,
            Coordinate[] toCheckPolygonCoordList) {

        for (int ii = 0; ii < toCheckPolygonCoordList.length; ii++) {
            if (isPointWithinPolygon(mainPolygonCoordList,
                    toCheckPolygonCoordList[ii])) {
                return true;
            }
        }

        return false;

    }

    /***
     * Draws a midnight meridian line with the given line color, line style and
     * line width.
     * 
     * @param graphicsTarget
     *            - the graphics target
     * @param sunPositionLon
     *            - longitude of the sub-solar point
     * @param lineColor
     *            - the color of the line in RGB
     * @param lineWidth
     *            - the width of the line in int
     * @param lineStyle
     *            - the type of line to drawn
     * @throws VizException
     */
    private void drawMidnightMeridianLine(IGraphicsTarget target,
            double sunPositionLon, RGB lineColor, int lineWidth,
            LineStyle lineStyle) {

        // meridian for local midnight
        double midnightlong = sunPositionLon + 180.0;
        if (midnightlong > 180.0) {
            midnightlong = midnightlong - 360.0;
        }

        double[] midnightlats = new double[179];
        for (int j = 0; j < 179; j++) {
            midnightlats[j] = -89 + j;
        }
        double[] midnightlongs = new double[179];
        for (int j = 0; j < 179; j++) {
            midnightlongs[j] = midnightlong;

        }

        Coordinate[] meridianCoords = new Coordinate[179];
        for (int i = 0; i < 179; i++) {
            meridianCoords[i] = new Coordinate(midnightlongs[i],
                    midnightlats[i]);
        }

        if (meridianCoords != null) {
            IWireframeShape wireframeShape = target.createWireframeShape(false,
                    descriptor);

            JTSCompiler jtsCompiler = new JTSCompiler(null, wireframeShape,
                    descriptor);
            GeometryFactory gf = new GeometryFactory();
            LineString ls = gf.createLineString(meridianCoords);

            try {
                jtsCompiler.handle(ls, lineColor, true);
                wireframeShape.compile();
                target.drawWireframeShape(wireframeShape, lineColor, lineWidth,
                        lineStyle);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error when drawing midnight meridian line: " + e);
            }
            wireframeShape.dispose();
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal()
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        initializeViewMinAndMaxXAndY(paintProps);
        // Only need to recreate the wireframeShapes if attributes or area
        // changed.
        if (needsUpdate) {
            needsUpdate = false;
            initInternal(target);
        }

        Date currentFrameTime = getCurrentFrameTime();

        if (wireframeShapesMap != null && wireframeShapesMap.size() == 1) {
            currentFrameTime = (Date) (wireframeShapesMap.keySet().toArray()[0]);
        }

        IWireframeShape wireframeShape = wireframeShapesMap
                .get(currentFrameTime);

        Coordinate[] terminatorCoords = terminatorCoordsMap
                .get(currentFrameTime);

        if (wireframeShape == null || terminatorCoords == null) {
            terminatorCoords = calulateDayNightTerminator(currentFrameTime);
            wireframeShape = createWireframeShape(terminatorCoords, target);
        }

        if (wireframeShape != null) {

            // draw terminator line
            try {
                target.drawWireframeShape(wireframeShape,
                        dayNightTermOverlayResourceData.getTermLineColor(),
                        dayNightTermOverlayResourceData.getTermLineWidth(),
                        dayNightTermOverlayResourceData.getTermLineStyle());
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error when drawing day/night terminator line: " + e);
            }

            // draw sub-solar point
            sunPosCoord = SunPosition.sunPosition(currentFrameTime.getTime());
            sunPosCoord.x = MapUtil.correctLon(sunPosCoord.x);
            sunPosCoord.y = MapUtil.correctLat(sunPosCoord.y);

            if (dayNightTermOverlayResourceData.getDisplaySun()) {

                Symbol sunSymbol = new Symbol(null,
                        new Color[] { new Color(dayNightTermOverlayResourceData
                                .getSunMarkerColor().red,
                                dayNightTermOverlayResourceData
                                        .getSunMarkerColor().green,
                                dayNightTermOverlayResourceData
                                        .getSunMarkerColor().blue) },
                        dayNightTermOverlayResourceData.getSunMarkerWidth(),
                        dayNightTermOverlayResourceData.getSunMarkerSize(),
                        false, sunPosCoord, new String("Marker"),
                        dayNightTermOverlayResourceData.getSunMarkerType()
                                .toString());

                DisplayElementFactory df = new DisplayElementFactory(target,
                        this.descriptor);

                ArrayList<IDisplayable> elements = df.createDisplayElements(
                        sunSymbol, paintProps);
                for (IDisplayable each : elements) {
                    try {
                        each.draw(target, paintProps);
                        each.dispose();
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error when drawing sub-solar point: " + e);
                    }
                }

            }

            // draw midnight meridian line
            if (dayNightTermOverlayResourceData.getDisplayMidnightMeridian()) {
                drawMidnightMeridianLine(target, sunPosCoord.x,
                        dayNightTermOverlayResourceData
                                .getMidnightMeridianLineColor(),
                        dayNightTermOverlayResourceData
                                .getMidnightMeridianLineWidth(),
                        dayNightTermOverlayResourceData
                                .getMidnightMeridianLineStyle());
            }

            // apply shading to day/night side
            if (dayNightTermOverlayResourceData.getApplyShading()) {
                // get polygon coordinates for the upper two and lower two
                // polygons
                // divided by the terminator line
                terminatorCoords = calulateDayNightTerminator(currentFrameTime);

                Coordinate[] termCoordsFrom0to180 = divideTerminatorCoords(
                        terminatorCoords, "0to180");
                Coordinate[] termCoordsForMinus180to0 = divideTerminatorCoords(
                        terminatorCoords, "-180to-0");

                Coordinate[] lowerPoly1Coords = createLowerPolygon0to180Coords(termCoordsFrom0to180);
                Coordinate[] lowerPoly2Coords = createLowerPolygonMinus180to0Coords(termCoordsForMinus180to0);
                Coordinate[] upperPoly1Coords = createUpperPolygon0to180Coords(termCoordsFrom0to180);
                Coordinate[] upperPoly2Coords = createUpperPolygonMinus180to0Coords(termCoordsForMinus180to0);

                boolean sunInLowerPolygon = true;
                if (isPointWithinPolygon(lowerPoly1Coords, sunPosCoord)
                        || isPointWithinPolygon(lowerPoly2Coords, sunPosCoord)) {
                    sunInLowerPolygon = true;

                    // day side
                    drawShadedPolygon(lowerPoly1Coords, target,
                            dayNightTermOverlayResourceData.getDayShadeColor(),
                            dayNightTermOverlayResourceData.getShadeAlpha());
                    drawShadedPolygon(lowerPoly2Coords, target,
                            dayNightTermOverlayResourceData.getDayShadeColor(),
                            dayNightTermOverlayResourceData.getShadeAlpha());

                    // night side
                    drawShadedPolygon(upperPoly1Coords, target,
                            dayNightTermOverlayResourceData
                                    .getNightShadeColor(),
                            dayNightTermOverlayResourceData.getShadeAlpha());
                    drawShadedPolygon(upperPoly2Coords, target,
                            dayNightTermOverlayResourceData
                                    .getNightShadeColor(),
                            dayNightTermOverlayResourceData.getShadeAlpha());

                } else if (isPointWithinPolygon(upperPoly1Coords, sunPosCoord)
                        || isPointWithinPolygon(upperPoly2Coords, sunPosCoord)) {

                    sunInLowerPolygon = false;

                    // day side
                    drawShadedPolygon(upperPoly1Coords, target,
                            dayNightTermOverlayResourceData.getDayShadeColor(),
                            dayNightTermOverlayResourceData.getShadeAlpha());
                    drawShadedPolygon(upperPoly2Coords, target,
                            dayNightTermOverlayResourceData.getDayShadeColor(),
                            dayNightTermOverlayResourceData.getShadeAlpha());

                    // night side
                    drawShadedPolygon(lowerPoly1Coords, target,
                            dayNightTermOverlayResourceData
                                    .getNightShadeColor(),
                            dayNightTermOverlayResourceData.getShadeAlpha());
                    drawShadedPolygon(lowerPoly2Coords, target,
                            dayNightTermOverlayResourceData
                                    .getNightShadeColor(),
                            dayNightTermOverlayResourceData.getShadeAlpha());
                }

                // get polygon coordinates for the north pole and the south pole
                Coordinate[] northPolygonCoords = createNorthPolePolygonCoords(
                        target, terminatorCoords);
                Coordinate[] southPolygonCoords = createSouthPolePolygonCoords(
                        target, terminatorCoords);

                // apply shading (based on day or night) to the north and south
                // pole polygons
                if (isPolygonWithinPolygon(lowerPoly1Coords, southPolygonCoords)
                        || isPolygonWithinPolygon(lowerPoly2Coords,
                                southPolygonCoords)) {

                    if (sunInLowerPolygon) {
                        // polygon in the south pole is in the day side
                        drawShadedPolygon(southPolygonCoords, target,
                                dayNightTermOverlayResourceData
                                        .getDayShadeColor(),
                                dayNightTermOverlayResourceData.getShadeAlpha());
                        // polygon in the north pole is in the night side
                        drawShadedPolygon(northPolygonCoords, target,
                                dayNightTermOverlayResourceData
                                        .getNightShadeColor(),
                                dayNightTermOverlayResourceData.getShadeAlpha());
                    } else {
                        // polygon in the south pole is in the night side
                        drawShadedPolygon(southPolygonCoords, target,
                                dayNightTermOverlayResourceData
                                        .getNightShadeColor(),
                                dayNightTermOverlayResourceData.getShadeAlpha());
                        // polygon in the north pole is in the day side
                        drawShadedPolygon(northPolygonCoords, target,
                                dayNightTermOverlayResourceData
                                        .getDayShadeColor(),
                                dayNightTermOverlayResourceData.getShadeAlpha());
                    }
                }
                if (isPolygonWithinPolygon(upperPoly1Coords, northPolygonCoords)
                        || isPolygonWithinPolygon(upperPoly2Coords,
                                northPolygonCoords)) {

                    if (sunInLowerPolygon) {
                        // polygon in the north pole is in the day side
                        drawShadedPolygon(northPolygonCoords, target,
                                dayNightTermOverlayResourceData
                                        .getDayShadeColor(),
                                dayNightTermOverlayResourceData.getShadeAlpha());
                        // polygon in the south pole is in the day side
                        drawShadedPolygon(southPolygonCoords, target,
                                dayNightTermOverlayResourceData
                                        .getNightShadeColor(),
                                dayNightTermOverlayResourceData.getShadeAlpha());
                    } else {
                        // polygon in the north pole is in the night side
                        drawShadedPolygon(northPolygonCoords, target,
                                dayNightTermOverlayResourceData
                                        .getNightShadeColor(),
                                dayNightTermOverlayResourceData.getShadeAlpha());
                        // polygon in the south pole is in the day side
                        drawShadedPolygon(southPolygonCoords, target,
                                dayNightTermOverlayResourceData
                                        .getDayShadeColor(),
                                dayNightTermOverlayResourceData.getShadeAlpha());
                    }
                }

            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal()
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

        // get frame times from the current active editor
        AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();

        if (editor == null
                || NcEditorUtil.getNcDisplayType(editor) != NcDisplayType.NMAP_DISPLAY) {
            throw new VizException("The active editor is not an NMAP display.");
        }

        IDisplayPane activePane = editor.getActiveDisplayPane();
        FramesInfo info = activePane.getDescriptor().getFramesInfo();
        DataTime[] frameTimes = info.getFrameTimes();

        if (frameTimes != null) {

            // For each frameTime, calculate the coordinates for the day/night
            // terminator line , create wireframe shape
            // and put the wireframe shape and the terminator coordinates in
            // appropriate maps.
            for (DataTime frameTime : frameTimes) {
                Coordinate[] terminatorCoords = calulateDayNightTerminator(frameTime
                        .getRefTime());
                terminatorCoordsMap.put(frameTime.getRefTime(),
                        terminatorCoords);

                IWireframeShape wireframeShape = createWireframeShape(
                        terminatorCoords, target);
                wireframeShapesMap.put(frameTime.getRefTime(), wireframeShape);
            }
        }

    }

    /***
     * Divide the terminator curve coordinates into two sections (from lon 0 to
     * 180 and -180 to 0) and return the coordinates based on the type
     * requested.
     * 
     * @param terminatorCoords
     *            - the lat/lon coordinates for the terminator curve
     * @param type
     *            - the requested type (0to180 or -180to0)
     * @return the list of coordinates based on the type passed (0to180 or
     *         -180to0)
     */
    private Coordinate[] divideTerminatorCoords(Coordinate[] terminatorCoords,
            String type) {

        List<Coordinate> terminatorCoordList = new ArrayList<Coordinate>();
        for (int i = 0; i < terminatorCoords.length; i++) {
            terminatorCoordList.add(terminatorCoords[i]);
        }
        // sort by longitude
        Collections.sort(terminatorCoordList, CoordLonComparator);

        List<Coordinate> polygon1CoordList = new ArrayList<Coordinate>();
        List<Coordinate> polygon2CoordList = new ArrayList<Coordinate>();

        for (int i = 0; i < terminatorCoordList.size(); i++) {
            if (terminatorCoordList.get(i).x >= 0) {
                // coordinates with longitude from 0 to 180
                polygon1CoordList.add(terminatorCoordList.get(i));
            } else {
                // coordinates with longitude from -180 to 0
                polygon2CoordList.add(terminatorCoordList.get(i));
            }
        }

        if ("0to180".equals(type)) {
            Coordinate[] poly1CoordArray = new Coordinate[polygon1CoordList
                    .size()];

            for (int i = 0; i < polygon1CoordList.size(); i++) {
                poly1CoordArray[i] = polygon1CoordList.get(i);
            }
            return poly1CoordArray;
        } else {
            Coordinate[] poly2CoordArray = new Coordinate[polygon2CoordList
                    .size()];
            for (int i = 0; i < polygon2CoordList.size(); i++) {
                poly2CoordArray[i] = polygon2CoordList.get(i);
            }

            return poly2CoordArray;
        }
    }

    /***
     * Creates the list of coordinates for the lower polygon (from lon 0 to 180)
     * that needs to be shaded.
     * 
     * @param terminatorCoords
     *            - the lat/lon coordinates for the terminator curve
     * 
     * @return the list of coordinates for the for the lower polygon (from lon 0
     *         to 180)
     */
    private Coordinate[] createLowerPolygon0to180Coords(
            Coordinate[] terminatorCoords) {
        List<Coordinate> polyCoordList = new ArrayList<Coordinate>();

        for (int i = 0; i < terminatorCoords.length; i++) {
            polyCoordList.add(terminatorCoords[i]);
        }

        Coordinate polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        double lastLat = polyLastCoord.y;
        double lastLon = polyLastCoord.x;
        double startLat = polyCoordList.get(0).y;
        double startLon = polyCoordList.get(0).x;

        polyCoordList.add(0, new Coordinate(0.0, startLat));
        polyCoordList.add(new Coordinate(180, lastLat));

        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLat = polyLastCoord.y;
        lastLon = polyLastCoord.x;
        startLat = polyCoordList.get(0).y;
        startLon = polyCoordList.get(0).x;

        while (lastLat >= -80) {
            double tmpLat = lastLat--;
            Coordinate tmp = new Coordinate(polyLastCoord.x, tmpLat);
            polyCoordList.add(tmp);
        }

        polyCoordList.add(new Coordinate(polyLastCoord.x, -80.0));
        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLon = polyLastCoord.x;
        lastLat = polyLastCoord.y;

        while (lastLon >= 0) {
            double tmpLon = lastLon--;
            Coordinate tmp = new Coordinate(tmpLon, polyLastCoord.y);
            polyCoordList.add(tmp);
        }

        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLon = polyLastCoord.x;
        lastLat = polyLastCoord.y;

        while (lastLat <= startLat) {
            double tmpLat = lastLat++;
            Coordinate tmp = new Coordinate(polyLastCoord.x, tmpLat);
            polyCoordList.add(tmp);
        }

        polyCoordList.add(new Coordinate(polyCoordList.get(0).x, polyCoordList
                .get(0).y));

        Coordinate[] polyCoordArray = new Coordinate[polyCoordList.size()];
        for (int i = 0; i < polyCoordList.size(); i++) {
            polyCoordArray[i] = polyCoordList.get(i);
        }

        return polyCoordArray;
    }

    /***
     * Creates the list of coordinates for the lower polygon (from lon 180 to 0)
     * that needs to be shaded.
     * 
     * @param terminatorCoords
     *            - the lat/lon coordinates for the terminator curve
     * 
     * @return the list of coordinates for the lower polygon (from lon -180 to
     *         0)
     */
    private Coordinate[] createLowerPolygonMinus180to0Coords(
            Coordinate[] terminatorCoords) {
        List<Coordinate> polyCoordList = new ArrayList<Coordinate>();

        for (int i = 0; i < terminatorCoords.length; i++) {
            polyCoordList.add(terminatorCoords[i]);
        }

        Coordinate polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        double lastLat = polyLastCoord.y;
        double lastLon = polyLastCoord.x;
        double startLat = polyCoordList.get(0).y;
        double startLon = polyCoordList.get(0).x;

        polyCoordList.add(0, new Coordinate(-180.0, startLat));
        polyCoordList.add(new Coordinate(-0.01, lastLat));

        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLat = polyLastCoord.y;
        lastLon = polyLastCoord.x;
        startLat = polyCoordList.get(0).y;
        startLon = polyCoordList.get(0).x;

        while (lastLat >= -80) {
            double tmpLat = lastLat--;
            Coordinate tmp = new Coordinate(polyLastCoord.x, tmpLat);
            polyCoordList.add(tmp);
        }
        polyCoordList.add(new Coordinate(polyLastCoord.x, -80.0));
        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLon = polyLastCoord.x;
        lastLat = polyLastCoord.y;

        while (lastLon >= -180) {
            double tmpLon = lastLon--;
            Coordinate tmp = new Coordinate(tmpLon, polyLastCoord.y);
            polyCoordList.add(tmp);
        }

        polyCoordList.add(new Coordinate(-180.00, polyLastCoord.y));
        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLon = polyLastCoord.x;
        lastLat = polyLastCoord.y;

        while (lastLat <= startLat) {
            double tmpLat = lastLat++;
            Coordinate tmp = new Coordinate(polyLastCoord.x, tmpLat);
            polyCoordList.add(tmp);
        }

        polyCoordList.add(new Coordinate(polyCoordList.get(0).x, polyCoordList
                .get(0).y));

        Coordinate[] polyCoordArray = new Coordinate[polyCoordList.size()];
        for (int i = 0; i < polyCoordList.size(); i++) {
            polyCoordArray[i] = polyCoordList.get(i);
        }

        return polyCoordArray;
    }

    /***
     * Creates the list of coordinates for the upper polygon (from lon 0 to 180)
     * that needs to be shaded.
     * 
     * @param terminatorCoords
     *            - the lat/lon coordinates for the terminator curve
     * 
     * @return the list of coordinates for the for the upper polygon (from lon 0
     *         to 180)
     */
    private Coordinate[] createUpperPolygon0to180Coords(
            Coordinate[] terminatorCoords) {
        List<Coordinate> polyCoordList = new ArrayList<Coordinate>();

        for (int i = 0; i < terminatorCoords.length; i++) {
            polyCoordList.add(terminatorCoords[i]);
        }

        Coordinate polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        double lastLat = polyLastCoord.y;
        double lastLon = polyLastCoord.x;
        double startLat = polyCoordList.get(0).y;
        double startLon = polyCoordList.get(0).x;

        polyCoordList.add(0, new Coordinate(0.001, startLat));
        polyCoordList.add(new Coordinate(180, lastLat));

        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLat = polyLastCoord.y;
        lastLon = polyLastCoord.x;
        startLat = polyCoordList.get(0).y;
        startLon = polyCoordList.get(0).x;

        while (lastLat <= 80) {
            double tmpLat = lastLat++;
            Coordinate tmp = new Coordinate(polyLastCoord.x, tmpLat);
            polyCoordList.add(tmp);
        }

        polyCoordList.add(new Coordinate(polyLastCoord.x, 80.0));
        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLon = polyLastCoord.x;
        lastLat = polyLastCoord.y;

        while (lastLon >= 0) {
            double tmpLon = lastLon--;
            Coordinate tmp = new Coordinate(tmpLon, polyLastCoord.y);
            polyCoordList.add(tmp);
        }

        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLon = polyLastCoord.x;
        lastLat = polyLastCoord.y;
        while (lastLat >= startLat) {
            double tmpLat = lastLat--;
            Coordinate tmp = new Coordinate(polyLastCoord.x, tmpLat);
            polyCoordList.add(tmp);
        }

        polyCoordList.add(new Coordinate(polyCoordList.get(0).x, polyCoordList
                .get(0).y));

        Coordinate[] polyCoordArray = new Coordinate[polyCoordList.size()];
        for (int i = 0; i < polyCoordList.size(); i++) {
            polyCoordArray[i] = polyCoordList.get(i);
        }

        return polyCoordArray;
    }

    /***
     * Creates the list of coordinates for the upper polygon (from lon 180 to 0)
     * that needs to be shaded.
     * 
     * @param terminatorCoords
     *            - the lat/lon coordinates for the terminator curve
     * 
     * @return the list of coordinates for the upper polygon (from lon -180 to
     *         0)
     */
    private Coordinate[] createUpperPolygonMinus180to0Coords(
            Coordinate[] terminatorCoords) {
        List<Coordinate> polyCoordList = new ArrayList<Coordinate>();

        for (int i = 0; i < terminatorCoords.length; i++) {
            polyCoordList.add(terminatorCoords[i]);
        }

        Coordinate polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        double lastLat = polyLastCoord.y;
        double lastLon = polyLastCoord.x;
        double startLat = polyCoordList.get(0).y;
        double startLon = polyCoordList.get(0).x;

        polyCoordList.add(0, new Coordinate(-180.00, startLat));
        polyCoordList.add(new Coordinate(-0.001, lastLat));
        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLat = polyLastCoord.y;
        lastLon = polyLastCoord.x;
        startLat = polyCoordList.get(0).y;
        startLon = polyCoordList.get(0).x;

        while (lastLat <= 80) {
            double tmpLat = lastLat++;
            Coordinate tmp = new Coordinate(polyLastCoord.x, tmpLat);
            polyCoordList.add(tmp);
        }

        polyCoordList.add(new Coordinate(polyLastCoord.x, 80.0));
        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLon = polyLastCoord.x;
        lastLat = polyLastCoord.y;

        while (lastLon >= -180) {
            double tmpLon = lastLon--;
            Coordinate tmp = new Coordinate(tmpLon, polyLastCoord.y);
            polyCoordList.add(tmp);
        }
        polyCoordList.add(new Coordinate(-180.0, polyLastCoord.y));
        polyLastCoord = polyCoordList.get(polyCoordList.size() - 1);
        lastLon = polyLastCoord.x;
        lastLat = polyLastCoord.y;

        while (lastLat <= startLat) {
            double tmpLat = lastLat++;
            Coordinate tmp = new Coordinate(polyLastCoord.x, tmpLat);
            polyCoordList.add(tmp);
        }

        polyCoordList.add(new Coordinate(polyCoordList.get(0).x, polyCoordList
                .get(0).y));

        Coordinate[] polyCoordArray = new Coordinate[polyCoordList.size()];
        for (int i = 0; i < polyCoordList.size(); i++) {
            polyCoordArray[i] = polyCoordList.get(i);
        }

        return polyCoordArray;
    }

    /***
     * Creates the list of coordinates for the area (from lat 80 to 90) in the
     * north pole that needs to be shaded.
     * 
     * @param target
     *            - the graphics target
     * @param terminatorCoords
     *            - the lat/lon coordinates for the terminator curve
     * 
     * @return the list of coordinates for the area in the north pole
     */
    private Coordinate[] createNorthPolePolygonCoords(IGraphicsTarget target,
            Coordinate[] terminatorCoordList) {
        List<Coordinate> polyNorthCoordList = new ArrayList<Coordinate>();
        polyNorthCoordList.add(new Coordinate(0.0, 90.0));

        Coordinate polyNorthLastCoord = polyNorthCoordList
                .get(polyNorthCoordList.size() - 1);

        double lastLat = polyNorthLastCoord.y;
        double lastLon = polyNorthLastCoord.x;
        double startLat = polyNorthCoordList.get(0).y;
        double startLon = polyNorthCoordList.get(0).x;

        while (lastLat >= 80) {
            double tmpLat = lastLat--;
            Coordinate tmp = new Coordinate(polyNorthLastCoord.x, tmpLat);
            polyNorthCoordList.add(tmp);
        }

        polyNorthLastCoord = polyNorthCoordList
                .get(polyNorthCoordList.size() - 1);
        lastLon = polyNorthLastCoord.x;
        lastLat = polyNorthLastCoord.y;

        polyNorthCoordList.add(new Coordinate(lastLon, lastLat));
        polyNorthLastCoord = polyNorthCoordList
                .get(polyNorthCoordList.size() - 1);
        lastLon = polyNorthLastCoord.x;
        lastLat = polyNorthLastCoord.y;

        while (lastLon <= 360) {
            double tmpLon = lastLon++;
            Coordinate tmp = new Coordinate(MapUtil.correctLon(tmpLon),
                    polyNorthLastCoord.y);
            polyNorthCoordList.add(tmp);
        }

        polyNorthLastCoord = polyNorthCoordList
                .get(polyNorthCoordList.size() - 1);
        lastLon = polyNorthLastCoord.x;
        lastLat = polyNorthLastCoord.y;

        while (lastLat <= startLat) {
            double tmpLat = lastLat++;
            Coordinate tmp = new Coordinate(polyNorthLastCoord.x, tmpLat);
            polyNorthCoordList.add(tmp);
        }

        polyNorthCoordList.add(new Coordinate(polyNorthCoordList.get(0).x,
                polyNorthCoordList.get(0).y));

        Coordinate[] polyNorthCoordArray = new Coordinate[polyNorthCoordList
                .size()];
        for (int i = 0; i < polyNorthCoordList.size(); i++) {
            polyNorthCoordArray[i] = polyNorthCoordList.get(i);
        }

        return polyNorthCoordArray;
    }

    /***
     * Creates the list of coordinates for the area (from lat -80 to -90) in the
     * south pole that needs to be shaded.
     * 
     * @param target
     *            - the graphics target
     * @param terminatorCoords
     *            - the lat/lon coordinates for the terminator curve
     * 
     * @return the list of coordinates for the area in the south pole
     */
    private Coordinate[] createSouthPolePolygonCoords(IGraphicsTarget target,
            Coordinate[] terminatorCoordList) {
        List<Coordinate> polySouthCoordList = new ArrayList<Coordinate>();
        polySouthCoordList.add(0, new Coordinate(0.0, -90.0));

        Coordinate polySouthLastCoord = polySouthCoordList
                .get(polySouthCoordList.size() - 1);

        double lastLat = polySouthLastCoord.y;
        double lastLon = polySouthLastCoord.x;
        double startLat = polySouthCoordList.get(0).y;
        double startLon = polySouthCoordList.get(0).x;

        while (lastLat <= -80) {
            double tmpLat = lastLat++;
            Coordinate tmp = new Coordinate(polySouthLastCoord.x, tmpLat);
            polySouthCoordList.add(tmp);
        }

        polySouthLastCoord = polySouthCoordList
                .get(polySouthCoordList.size() - 1);
        lastLon = polySouthLastCoord.x;
        lastLat = polySouthLastCoord.y;

        polySouthCoordList.add(new Coordinate(lastLon, lastLat));
        polySouthLastCoord = polySouthCoordList
                .get(polySouthCoordList.size() - 1);
        lastLon = polySouthLastCoord.x;
        lastLat = polySouthLastCoord.y;

        while (lastLon <= 360) {
            double tmpLon = lastLon++;
            Coordinate tmp = new Coordinate(MapUtil.correctLon(tmpLon),
                    polySouthLastCoord.y);
            polySouthCoordList.add(tmp);
        }

        polySouthLastCoord = polySouthCoordList
                .get(polySouthCoordList.size() - 1);
        lastLon = polySouthLastCoord.x;
        lastLat = polySouthLastCoord.y;

        while (lastLat <= startLat) {
            double tmpLat = lastLat++;
            Coordinate tmp = new Coordinate(polySouthLastCoord.x, tmpLat);
            polySouthCoordList.add(tmp);
        }

        polySouthCoordList.add(new Coordinate(polySouthCoordList.get(0).x,
                polySouthCoordList.get(0).y));

        Coordinate[] polySouthCoordArray = new Coordinate[polySouthCoordList
                .size()];
        for (int i = 0; i < polySouthCoordList.size(); i++) {
            polySouthCoordArray[i] = polySouthCoordList.get(i);
        }

        return polySouthCoordArray;
    }

    /***
     * Creates the wireframe shape given the list of coordinates.
     * 
     * @param terminatorCoords
     *            - the lat/lon coordinates for the terminator curve
     * @param target
     *            - the graphics target
     * 
     * @return wireframeShape
     */
    private IWireframeShape createWireframeShape(Coordinate[] terminatorCoords,
            IGraphicsTarget target) {

        IWireframeShape wireframeShape = target.createWireframeShape(false,
                descriptor);

        JTSCompiler jtsCompiler = new JTSCompiler(null, wireframeShape,
                descriptor);
        GeometryFactory gf = new GeometryFactory();
        LineString ls = gf.createLineString(terminatorCoords);

        try {
            jtsCompiler.handle(ls, new RGB(255, 255, 255), true);
            wireframeShape.compile();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error when creating wireframes." + e);
            return null;
        }
        return wireframeShape;

    }

    /***
     * Returns the current frame time for the active NCMap display. If current
     * active NCMap display does not have any frame times, returns the current
     * date.
     * 
     * @return date
     */
    public Date getCurrentFrameTime() {

        AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
        IDisplayPane activePane = editor.getActiveDisplayPane();

        FramesInfo info = activePane.getDescriptor().getFramesInfo();
        DataTime[] frameTimes = info.getFrameTimes();

        if (frameTimes != null && frameTimes.length > 0) {
            return info.getCurrentFrame().getRefTime();
        }

        return new Date();
    }

    /***
     * Calculates the lat/lon coordinates for the day/night terminator line for
     * a given date
     * 
     * @param date
     *            - the date for which the coordinates for the terminator line
     *            is to be calculated
     * @return Coordinate[] - array of lat/lon coordinates for the day/night
     *         terminator line
     */
    public Coordinate[] calulateDayNightTerminator(Date date) {

        // Calculate the number of days between what was input
        // and January 1, 1974 00:00:00
        // (example, so that January 1, 1974 12:00:00 is 0.5)

        // begin by setting some variables that are relevant to
        // the passed in current time

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(date);

        int hh = cal.get(Calendar.HOUR_OF_DAY);
        int mm = cal.get(Calendar.MINUTE);
        int ss = cal.get(Calendar.SECOND);
        int doy = cal.get(Calendar.DAY_OF_YEAR);

        double curday = getJulianDate(cal);

        double frac_hr = (hh + mm / 60d + ss / 3600d) - 6d;

        double DTOR = 0.0174532925199433;
        //
        // Calendar cal1 = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        // cal1.set(Calendar.YEAR, 1975);
        // cal1.set(Calendar.MONTH, Calendar.JANUARY);
        // cal1.set(Calendar.DAY_OF_YEAR, 1);
        // cal1.set(Calendar.HOUR_OF_DAY, 0);
        // cal1.set(Calendar.MINUTE, 0);
        // cal1.set(Calendar.SECOND, 0);
        //
        // // Duffet-Smith time
        // double duffday = getJulianDate(cal1);
        // double days_between = curday + (hh + mm / 60d + ss / 3600d) / 24d
        // - duffday;
        //
        // // Mean anomaly - Subtract full rotations
        // double xm = days_between * (0.017202424d) - 6.0544180138855d - 2;
        //
        // // Number of full rotations
        // double iyr = Math.floor(xm / (2 * Math.PI));
        //
        // // Mean anomaly in radians
        // double xmean = xm - 2d * Math.PI * iyr;
        //
        // // double N = ORBIT_RADS_PER_DAY * daysSinceEpoch;
        // // N %= TWO_PI;
        // // if (N < 0)
        // // N += TWO_PI;
        // //
        // // double M0 = N + epsilon_g - omega_bar_g;
        // // if (M0 < 0)
        // // M0 += TWO_PI;
        //
        // // System.out.println(" --- swpc algorithm mean anomaly = " + xmean);
        //
        // // Eccentric anomaly
        // double xecc = xmean + (0.016720 * Math.sin(xmean))
        // + (0.5 * (2.795584d - 4) * Math.sin(xmean * 2));
        // double x = 1.0168621 * Math.tan(0.5 * xecc);
        //
        // // True anomaly
        // double xtrue = 2 * Math.atan(x);
        //
        // // Safe keeping while checking if xtrue is off by PI
        // double xt = xtrue;
        //
        // // True anomaly
        // double xta = xmean - (2 * 0.0168621 * Math.sin(xmean));
        //
        // // if xtrue/2 is off by PI, xtrue will be off by 2*PI
        // if (Math.abs(xtrue * xta) > (Math.PI / 10d)) {
        // if (xtrue > xta) {
        // xtrue = xtrue - 2 * Math.PI;
        // } else {
        // xtrue = xtrue + 2 * Math.PI;
        // }
        // }
        // // Lambda is the angle between solar radius vector and celestial
        // x-axis
        // double xlam = xtrue - 1.3524488;
        // double clam = Math.cos(xlam);
        // double slam = Math.sin(xlam);
        // double cs_th_sun = slam * 0.397681215556;
        //
        // double sn_th_sun = Math.pow((1.0 - cs_th_sun * cs_th_sun), 0.5);
        // double cs_ph_sun = clam / sn_th_sun;
        // double sn_ph_sun = slam * (0.917523651354d / sn_th_sun);
        //
        // // Calcluate Greenwich Mean Sidereal time
        // double n_days_elapsed = Math.floor(days_between);
        // double frac_days_elapsed = days_between - n_days_elapsed;
        // double elapsed_years = Math.floor(n_days_elapsed / 365);
        // double day_defect = n_days_elapsed - elapsed_years * 365d;
        // double gmst = (frac_days_elapsed * 24.06588) + (day_defect *
        // 0.065709)
        // - (elapsed_years * 0.015917) - 17.397221;
        // if (gmst < 24)
        // gmst = gmst + 24;
        // gmst = gmst * Math.PI / 12d;
        //
        // // Rotate the x-axis from Geocentric Ecliptic coordinates to
        // Geographic
        // // coordinates
        // double[][] rot_mat1 = new double[3][3];
        // rot_mat1[0][0] = sn_th_sun * cs_ph_sun;
        // rot_mat1[0][1] = -1.0
        // * (0.397681215556 * cs_th_sun + 0.917523651354 * sn_th_sun
        // * sn_ph_sun);
        // rot_mat1[0][2] = 0.0;
        // rot_mat1[1][0] = sn_th_sun * sn_ph_sun;
        // rot_mat1[1][1] = 0.917523651354 * sn_th_sun * cs_ph_sun;
        // rot_mat1[1][2] = -0.397681215556d;
        // rot_mat1[2][0] = cs_th_sun;
        // rot_mat1[2][1] = 0.397681215556 * sn_th_sun * cs_ph_sun;
        // rot_mat1[2][2] = 0.917523651354;
        //
        // double[][] gam_mat = new double[3][3];
        // gam_mat[0][0] = Math.cos(gmst);
        // gam_mat[0][1] = Math.sin(gmst);
        // gam_mat[0][2] = 0;
        // gam_mat[1][0] = -Math.sin(gmst);
        // gam_mat[1][1] = Math.cos(gmst);
        // gam_mat[1][2] = 0;
        // gam_mat[2][0] = 0;
        // gam_mat[2][1] = 0;
        // gam_mat[2][2] = 1;
        //
        // double[][] rot_mat = new double[3][3];
        // for (int i = 0; i <= 2; i++) {
        // rot_mat[i][0] = gam_mat[i][0] * rot_mat1[0][0] + gam_mat[i][1]
        // * rot_mat1[1][0] + gam_mat[i][2] * rot_mat1[2][0];
        // }
        //
        // // rotate only the x-axis with position vector [1,0,0]
        // double xprim = rot_mat[0][0];
        // double yprim = rot_mat[1][0];
        // double zprim = rot_mat[2][0];
        //
        //
        //
        // double dist = Math.pow((xprim * xprim + yprim * yprim + zprim *
        // zprim),
        // 0.5);
        // sun_lat_gse = 90 - Math.asin(zprim / dist) / DTOR;
        // sun_long_gse = Math.atan2(yprim, xprim) / DTOR;
        //
        //
        // if (sun_long_gse < 0) {
        // sun_long_gse = 360 + sun_long_gse;
        // }
        //
        // sun_lat_gse = 90 - sun_lat_gse;
        // if (sun_long_gse > 180) {
        // sun_long_gse = sun_long_gse - 360;
        // }

        // Calculate the zenith angle and eccentricity
        double delta = 23.44 * Math.sin((0.9856 * (doy - 80.7)) * DTOR);

        Coordinate sunPosCoord = SunPosition.sunPosition(date.getTime());
        sun_long_gse = sunPosCoord.x;
        sun_lat_gse = sunPosCoord.y;

        Coordinate[] terminatorCoordArray = new Coordinate[360];

        // formula for the terminator line
        for (int d = 0; d <= 359; d++) {
            double param = Math.sin(d * DTOR) * Math.cos(sun_lat_gse * DTOR);
            double theta_t = Math.asin(param) / DTOR;

            double param1 = Math.sin(theta_t * DTOR)
                    * Math.sin(sun_lat_gse * DTOR);
            double param2 = Math.cos(sun_lat_gse * DTOR) * Math.cos(d * DTOR);
            double lambda_t = Math.atan2(-param1, param2) / DTOR;
            lambda_t = lambda_t - frac_hr * 15.0;
            if (lambda_t < -180) {
                lambda_t = lambda_t + 360;
            }
            terminatorCoordArray[d] = new Coordinate(
                    MapUtil.correctLon(lambda_t), MapUtil.correctLat(theta_t));

        }

        return terminatorCoordArray;

    }

    /***
     * Converts a date to julian date
     * 
     * @param calendarDate
     *            - the date for which needs to be converted
     * @return double - converted julian date in ms
     */
    public static double getJulianDate(Calendar calendarDate) {

        int year = calendarDate.get(Calendar.YEAR);
        int month = calendarDate.get(Calendar.MONTH) + 1;
        int day = calendarDate.get(Calendar.DAY_OF_MONTH);
        double hour = calendarDate.get(Calendar.HOUR_OF_DAY);
        double minute = calendarDate.get(Calendar.MINUTE);
        double second = calendarDate.get(Calendar.SECOND);
        int isGregorianCal = 1;
        int A;
        int B;
        int C;
        int D;
        double fraction = day
                + ((hour + (minute / 60) + (second / 60 / 60)) / 24);

        if (year < 1582) {
            isGregorianCal = 0;
        }

        if (month < 3) {
            year = year - 1;
            month = month + 12;
        }

        A = year / 100;
        B = (2 - A + (A / 4)) * isGregorianCal;

        if (year < 0) {
            C = (int) ((365.25 * year) - 0.75);
        } else {
            C = (int) (365.25 * year);
        }

        D = (int) (30.6001 * (month + 1));
        double JD = B + C + D + 1720994.5 + fraction;

        // test
        if (month <= 2) {
            year -= 1;
            month += 12;
        }
        double Aa = Math.floor(year / 100);
        double Bb = 2 - Aa + Math.floor(Aa / 4);

        JD = Math.floor(365.25 * (year + 4716))
                + Math.floor(30.6001 * (month + 1)) + day + Bb - 1524.5;
        // end test

        return JD;
    }

    /**
     * Comparator class to compare longitudes of two coordinates
     */
    Comparator<Coordinate> CoordLonComparator = new Comparator<Coordinate>() {
        @Override
        public int compare(Coordinate o1, Coordinate o2) {
            return new Double(o1.x).compareTo(o2.x);
        }
    };

}
