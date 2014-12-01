package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Implements a drawing layer to draw lat/lon lines
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 *   06/15/09    #127        M. Gao	     Initial Creation
 *   06/17/09    #115        Greg Hull   Integrate with AbstractNatlCntrsResource
 *   08/07/09                Greg Hull   remove unused variables and methods
 *   11/18/09                Greg Hull   Incorporate to11d6 changes 
 *   11/04/13    #880        Xiaochuan   set one wireframeShape for one lat or Lon lines.
 *                                       Set spatialChopFlag to be false.
 *   05/23/2014  #970        P.Swamy     Lat/lon label sizes need to be larger
 * </pre>
 * 
 * @author mgao
 * 
 */
public class LatLonOverlayResource extends
        AbstractVizResource<LatLonOverlayResourceData, IMapDescriptor>
        implements INatlCntrsResource {

    private final static org.apache.log4j.Logger log = org.apache.log4j.Logger
            .getLogger(LatLonOverlayResource.class);

    private LatLonOverlayResourceData latLonOverlayResourceData;

    /** The wireframe object for drawing Latitude lines */
    private IWireframeShape wireframeShapeForLatLineArray;

    /** The wireframe object for drawing Longitude lines */
    private IWireframeShape wireframeShapeForLonLineArray;

    private Map<Double, Geometry> latitudeLineGeometries;

    private Map<Double, Geometry> longitudeLineGeometries;

    private double offset = 0; // 50000;

    private double mapMinX;

    private double mapMaxY;

    private double mapMinY;

    private double mapMaxX;

    private double viewMinX;

    private double viewMaxY;

    private double viewMinY;

    private double viewMaxX;

    /*
     * The four effective view borders (map borders inside the viewable user
     * window) used to paint label
     */
    private Geometry effectiveLeftBorder;

    private Geometry effectiveRightBorder;

    private Geometry effectiveBottomBorder;

    private Geometry effectiveTopBorder;

    private Geometry mapBoundary;

    private Geometry mapMedianLine;

    private final double latLonDrawingPointInterval = 1.0; // 0.7; //1.0; //0.5

    private final double drawingGap = 120;

    private double labelGap;

    private boolean needsUpdate = true;

    protected LatLonOverlayResource(LatLonOverlayResourceData llRscData,
            LoadProperties props) {
        super(llRscData, props);
        latLonOverlayResourceData = llRscData;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    public void initInternal(IGraphicsTarget target) throws VizException {
        initializeMapMinAndMaxXAndY();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        IFont lfont = target.initializeFont("Courier", 14,
                new Style[] { Style.BOLD });
        float zoomFactor = paintProps.getZoomLevel();
        labelGap = drawingGap * zoomFactor;

        initializeViewMinAndMaxXAndY(paintProps);
        int latitudeDrawingLineNumber = getLatitudeDrawingLineNumber(latLonOverlayResourceData
                .getLatitudeInterval());
        int longitudeDrawingLineNumber = getLongitudeDrawingLineNumber(latLonOverlayResourceData
                .getLongitudeInterval());

        // Only need to recreate the wireframeShapes if the intervals or line
        // types changed.
        if (needsUpdate) {
            needsUpdate = false;

            clearWireFrameShapeArray(wireframeShapeForLatLineArray);
            clearWireFrameShapeArray(wireframeShapeForLonLineArray);

            latitudeLineGeometries = new HashMap<Double, Geometry>(
                    latitudeDrawingLineNumber);
            longitudeLineGeometries = new HashMap<Double, Geometry>(
                    longitudeDrawingLineNumber);

            wireframeShapeForLatLineArray = target.createWireframeShape(false,
                    descriptor, 4.0f, false, new PixelExtent(getViewMinX()
                            + offset, getViewMaxX() - offset, getViewMinY()
                            + offset, getViewMaxY() - offset));

            double latitudeValue = -90;
            for (int i = 0; i < latitudeDrawingLineNumber
                    && latitudeValue <= 90; i++) {
                double[][] latLonCoordinateArray = createCoordinateArrayForLatitudeLine(
                        latitudeValue, latLonDrawingPointInterval);

                if (!(latitudeValue == -90 || latitudeValue == 90)
                        && latLonCoordinateArray.length > 0) {
                    wireframeShapeForLatLineArray
                            .addLineSegment(latLonCoordinateArray);
                }
                latitudeValue += latLonOverlayResourceData
                        .getLatitudeInterval();
            }
            wireframeShapeForLatLineArray.compile();

            wireframeShapeForLonLineArray = target.createWireframeShape(false,
                    descriptor, 4.0f, false, new PixelExtent(getViewMinX()
                            + offset, getViewMaxX() - offset, getViewMinY()
                            + offset, getViewMaxY() - offset));

            double longitudeValue = -180;
            for (int i = 0; i < longitudeDrawingLineNumber
                    && longitudeValue <= 180; i++) {
                double[][] latLonCoordinateArray = createCoordinateArrayLongitudeLine(
                        longitudeValue, latLonDrawingPointInterval);

                if (latLonCoordinateArray.length > 0) {
                    wireframeShapeForLonLineArray
                            .addLineSegment(latLonCoordinateArray);
                }
                longitudeValue += latLonOverlayResourceData
                        .getLongitudeInterval();
            }
            wireframeShapeForLonLineArray.compile();
        }
        double latitudeValue = -90;

        for (int i = 0; i < latitudeDrawingLineNumber && latitudeValue <= 90; i++) {
            Geometry latLine = latitudeLineGeometries.get(latitudeValue);
            if (latLine != null) {
                wireframeShapeForLatLineArray.clearLabels();
                Coordinate intersection = null;
                if (effectiveLeftBorder.intersects(latLine)) {
                    intersection = effectiveLeftBorder.intersection(latLine)
                            .getCoordinate();
                } else if (latLine.within(mapBoundary)
                        && mapMedianLine.intersects(latLine)) {
                    intersection = mapMedianLine.intersection(latLine)
                            .getCoordinate();
                    // Removes default latitude label if it is too close to the
                    // bottom of the viewable edge to prevent the latitude label
                    // from being displayed over the longitude labels.
                    if (intersection.y > effectiveBottomBorder.getCoordinate().y
                            - 2.5 * labelGap) {
                        intersection = null;
                    }
                }
                if (intersection != null) {
                    wireframeShapeForLatLineArray
                            .addLabel(String.valueOf((int) latitudeValue),
                                    new double[] { intersection.x + labelGap,
                                            intersection.y });
                }
            }
            target.drawWireframeShape(wireframeShapeForLatLineArray,
                    latLonOverlayResourceData.getColor(),
                    latLonOverlayResourceData.getLineWidth(),
                    latLonOverlayResourceData.getLineStyle(), lfont);
            latitudeValue += latLonOverlayResourceData.getLatitudeInterval();
        }
        double longitudeValue = -180;

        for (int i = 0; i < longitudeDrawingLineNumber && longitudeValue <= 180; i++) {
            Geometry lonLine = longitudeLineGeometries.get(longitudeValue);
            if (lonLine != null) {
                wireframeShapeForLonLineArray.clearLabels();
                if (lonLine.intersects(effectiveBottomBorder)) {
                    Coordinate intersection = effectiveBottomBorder
                            .intersection(lonLine).getCoordinate();
                    int lonInt = (int) longitudeValue;
                    String label = Math.abs(lonInt) != 180 ? String
                            .valueOf(lonInt) : "ID";
                    wireframeShapeForLonLineArray.clearLabels();
                    wireframeShapeForLonLineArray.addLabel(label, new double[] {
                            intersection.x, intersection.y - labelGap });
                }
            }
            target.drawWireframeShape(wireframeShapeForLonLineArray,
                    latLonOverlayResourceData.getColor(),
                    latLonOverlayResourceData.getLineWidth(),
                    latLonOverlayResourceData.getLineStyle(), lfont);
            longitudeValue += latLonOverlayResourceData.getLongitudeInterval();
        }
        lfont.dispose();
    }

    private void initializeMapMinAndMaxXAndY() {
        mapMinX = descriptor.getGridGeometry().getGridRange().getLow(0);
        mapMaxX = descriptor.getGridGeometry().getGridRange().getHigh(0);
        mapMinY = descriptor.getGridGeometry().getGridRange().getLow(1);
        mapMaxY = descriptor.getGridGeometry().getGridRange().getHigh(1);

        double mapMidX = (mapMaxX - mapMinX) / 2;
        double mapMidY = (mapMaxY - mapMinY) / 2;
        mapMedianLine = new GeometryFactory()
                .createLineString(new Coordinate[] {
                        new Coordinate(mapMidX, mapMinY),
                        new Coordinate(mapMidX, mapMidY) });
        // Map boundary must be created in a continuous coordinate order to form
        // a rectangle!
        mapBoundary = new GeometryFactory().createPolygon(
                new GeometryFactory().createLinearRing(new Coordinate[] {
                        new Coordinate(mapMinX, mapMinY),
                        new Coordinate(mapMinX, mapMaxY),
                        new Coordinate(mapMaxX, mapMaxY),
                        new Coordinate(mapMaxX, mapMinY),
                        new Coordinate(mapMinX, mapMinY) }), null);
    }

    private void initializeViewMinAndMaxXAndY(PaintProperties paintProps) {
        viewMinX = paintProps.getView().getExtent().getMinX();
        viewMaxX = paintProps.getView().getExtent().getMaxX();
        viewMinY = paintProps.getView().getExtent().getMinY();
        viewMaxY = paintProps.getView().getExtent().getMaxY();

        updateEffectiveView();
    }

    private void updateEffectiveView() {
        double effectiveMinX = viewMinX;
        double effectiveMaxX = viewMaxX;
        double effectiveMinY = viewMinY;
        double effectiveMaxY = viewMaxY;

        if (mapMinX > viewMinX && mapMinX < viewMaxX) {
            effectiveMinX = mapMinX;
        }
        if (mapMaxX > viewMinX && mapMaxX < viewMaxX) {
            effectiveMaxX = mapMaxX;
        }
        if (mapMinY > viewMinY && mapMinY < viewMaxY) {
            effectiveMinY = mapMinY;
        }
        if (mapMaxY > viewMinY && mapMaxY < viewMaxY) {
            effectiveMaxY = mapMaxY;
        }
        double cornerGap = 2.5 * labelGap;
        Coordinate[] leftBorderCoordinates = new Coordinate[] {
                new Coordinate(effectiveMinX, effectiveMinY + cornerGap),
                new Coordinate(effectiveMinX, effectiveMaxY - cornerGap) };
        Coordinate[] rightBorderCoordinates = new Coordinate[] {
                new Coordinate(effectiveMaxX, effectiveMinY + cornerGap),
                new Coordinate(effectiveMaxX, effectiveMaxY - cornerGap) };
        Coordinate[] bottomBorderCoordinates = new Coordinate[] {
                new Coordinate(effectiveMinX + cornerGap, effectiveMaxY),
                new Coordinate(effectiveMaxX - cornerGap, effectiveMaxY) };
        Coordinate[] topBorderCoordinates = new Coordinate[] {
                new Coordinate(effectiveMinX + cornerGap, effectiveMinY),
                new Coordinate(effectiveMaxX - cornerGap, effectiveMinY) };
        effectiveLeftBorder = new GeometryFactory()
                .createLineString(leftBorderCoordinates);
        effectiveRightBorder = new GeometryFactory()
                .createLineString(rightBorderCoordinates);
        effectiveBottomBorder = new GeometryFactory()
                .createLineString(bottomBorderCoordinates);
        effectiveTopBorder = new GeometryFactory()
                .createLineString(topBorderCoordinates);
    }

    private int getLatitudeDrawingLineNumber(int latInterval) {
        int latLineNumber = 180 / 15; // set a default value
        if (latInterval > 0 && latInterval <= 180) {
            latLineNumber = 180 / latInterval;
        }
        return latLineNumber + 1;
    }

    private int getLongitudeDrawingLineNumber(int lonInterval) {
        int lonLineNumber = 360 / 15; // set a default value
        if (lonInterval > 0 && lonInterval <= 360) {
            lonLineNumber = 360 / lonInterval;
            if (lonLineNumber > 360)
                lonLineNumber = 360; // if we draw 360 lines, the last line will
                                     // overlap with the first line
        }
        return lonLineNumber + 1;
    }

    private double[][] createCoordinateArrayForLatitudeLine(
            double latitudeValue, double latLonPointInterval) {
        int coordinateArrayLength = (int) (360 / latLonPointInterval) + 1;
        ArrayList<Coordinate> latLineCoordinates = new ArrayList<Coordinate>(
                coordinateArrayLength);
        ArrayList<double[]> latLinePixels = new ArrayList<double[]>(
                coordinateArrayLength);
        double longitude = -180;
        for (int i = 0; i < coordinateArrayLength && longitude <= 180; i++) {
            double[] latLon = new double[] { longitude, latitudeValue };
            double[] screenPixel = descriptor.worldToPixel(latLon);
            if (screenPixel != null) {
                latLinePixels.add(screenPixel);
                latLineCoordinates.add(new Coordinate(screenPixel[0],
                        screenPixel[1]));
            }
            longitude += latLonPointInterval;
        }
        if (!latLineCoordinates.isEmpty())
            latitudeLineGeometries.put(latitudeValue, new GeometryFactory()
                    .createLineString(latLineCoordinates
                            .toArray(new Coordinate[] {})));
        return latLinePixels.toArray(new double[][] {});
    }

    private double[][] createCoordinateArrayLongitudeLine(
            double longitudeValue, double latLonPointInterval) {
        int coordinateArrayLength = (int) ((180 - 10) / latLonPointInterval);
        ArrayList<Coordinate> lonLineCoordinates = new ArrayList<Coordinate>(
                coordinateArrayLength);
        ArrayList<double[]> lonLinePixels = new ArrayList<double[]>(
                coordinateArrayLength);
        double latitude = -90 + latLonPointInterval;
        for (int i = 0; i < coordinateArrayLength && latitude <= 90; i++) {
            double[] latLon = new double[] { longitudeValue, latitude };
            double[] screenPixel = descriptor.worldToPixel(latLon);
            if (screenPixel != null) {
                lonLinePixels.add(screenPixel);
                lonLineCoordinates.add(new Coordinate(screenPixel[0],
                        screenPixel[1]));
            }
            latitude += latLonPointInterval;
        }
        if (!lonLineCoordinates.isEmpty())
            longitudeLineGeometries.put(longitudeValue, new GeometryFactory()
                    .createLineString(lonLineCoordinates
                            .toArray(new Coordinate[] {})));
        return lonLinePixels.toArray(new double[][] {});
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    public void disposeInternal() {
        clearWireFrameShapeArray(wireframeShapeForLatLineArray);
        clearWireFrameShapeArray(wireframeShapeForLonLineArray);
    }

    private void clearWireFrameShapeArray(IWireframeShape wireframeShapeArray) {
        if (wireframeShapeArray != null) {
            wireframeShapeArray.dispose();
            wireframeShapeArray = null;
        }
    }

    /*
     * the getters for Map's Min and Max X and Y, View's Min and Max X and Y
     */
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

    public double getMapMinX() {
        return mapMinX;
    }

    public double getMapMaxY() {
        return mapMaxY;
    }

    public double getMapMinY() {
        return mapMinY;
    }

    public double getMapMaxX() {
        return mapMaxX;
    }

    // @Override
    public void resourceAttrsModified() {
        needsUpdate = true;
    }

    // @Override
    public boolean isProjectable(CoordinateReferenceSystem mapData) {
        return true;
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        needsUpdate = true;
        initializeMapMinAndMaxXAndY();
    }
}
