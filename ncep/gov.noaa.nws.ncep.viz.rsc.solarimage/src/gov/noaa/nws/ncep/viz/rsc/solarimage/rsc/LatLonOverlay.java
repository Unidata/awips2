package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.viz.rsc.solarimage.util.HeaderData;
import gov.noaa.nws.ncep.viz.rsc.solarimage.wcs.CSConversions;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The class that draws the latitude and longitude overlays.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- -----------      --------------------------
 * 02/21/2013   958        qzhou, sgurung   Initial creation
 * 03/19/2013   958        qzhou            Modified CarrLon start/end intervals and display.
 * 11/04/2013   958        qzhou            Refacted code and Combined Cylindrical latlon overlay to here
 * 11/12/2013   958        qzhou            Added latlonOverlay dispose method
 * </pre>
 * 
 * @author qzhou, sgurung
 * @version 1.0
 */

public class LatLonOverlay {

    // Latitude lines
    private IWireframeShape[] wireframeShapeForLatLineArray;

    private List<Coordinate[]> latCoordPointArrayList;

    private IWireframeShape[] wireframeShapeForLonLineArray;

    private IWireframeShape[] wireframeShapeForLonLineArrayBnd;

    private List<Coordinate[]> lonCoordPointArrayList;

    private double latLonDrawingPointInterval = 1.0;

    protected int latLonInterval;

    private IDescriptor descriptor;

    private PaintProperties paintProps;

    private float zoomFactor;

    private int addToPoint = 1;// 25;

    private double mapMinX;

    private double mapMaxY;

    private double mapMinY;

    private double mapMaxX;

    private double viewMinX;

    private double viewMaxY;

    private double viewMinY;

    private double viewMaxX;

    /*
     * The four minimum and maximum of X and Y used to paint label
     */
    private double effectiveMinX;

    private double effectiveMaxY;

    private double effectiveMinY;

    private double effectiveMaxX;

    private double hgln = 0.0;

    private double crln = 0.0;

    private boolean isCarrington;

    private int cylindrical = 0;

    private MathTransform worldToPixel;

    private MathTransform pixelToWorld;

    protected CSConversions csConv;

    private RGB rgb;

    private IFont.Style[] fontStyle;

    public LatLonOverlay(SolarImageDisplay imageDisplay,
            IDescriptor descriptor, int latLonInterval,
            PaintProperties paintProps, boolean isCarrington, int cylindrical,
            HeaderData headerData) {
        this.descriptor = descriptor;
        this.latLonInterval = latLonInterval;
        this.paintProps = paintProps;
        this.zoomFactor = paintProps.getZoomLevel();
        this.isCarrington = isCarrington;
        this.cylindrical = cylindrical;
        this.worldToPixel = imageDisplay.getWorldToPixel();
        this.pixelToWorld = imageDisplay.getPixelToWorld();
        this.csConv = imageDisplay.getCSConversions();
        this.rgb = new RGB(255, 0, 0);
        this.fontStyle = new IFont.Style[1];
        fontStyle[0] = IFont.Style.BOLD;

        if (headerData != null) {
            this.hgln = headerData.getHgln();
            this.crln = headerData.getCrln();

            if (crln == 0.0) {
                if (headerData.getSolarL0() != 0) { // halpha
                    crln = headerData.getSolarL0();
                }
                if (crln == 0.0) // all other
                    crln = headerData.getL0B0()[0];
            }

        }
        // System.out.println(" ****** hgln crln: " + hgln + " " + crln + " "
        // + headerData.getL0B0()[1]);

        initializeMapMinAndMaxXAndY();
    }

    private void initializeMapMinAndMaxXAndY() {
        mapMinX = descriptor.getGridGeometry().getGridRange().getLow(0);
        mapMaxX = descriptor.getGridGeometry().getGridRange().getHigh(0);
        mapMinY = descriptor.getGridGeometry().getGridRange().getLow(1);
        mapMaxY = descriptor.getGridGeometry().getGridRange().getHigh(1);
    }

    private void initializeViewMinAndMaxXAndY() {
        viewMinX = paintProps.getView().getExtent().getMinX();
        viewMaxX = paintProps.getView().getExtent().getMaxX();
        viewMinY = paintProps.getView().getExtent().getMinY();
        viewMaxY = paintProps.getView().getExtent().getMaxY();

        effectiveMinX = viewMinX;
        effectiveMaxX = viewMaxX;
        effectiveMinY = viewMinY;
        effectiveMaxY = viewMaxY;
    }

    public double[] getStonyLonStartEndInterval() {

        double[] startEndInterval = new double[2];
        double startValue = -90 + hgln - hgln % latLonInterval;
        double endValue = startValue + 180;
        if (startValue < -90 + hgln) {
            startValue += latLonInterval;
        }

        if (endValue > 90 + hgln) {
            endValue -= latLonInterval;
        }

        /*
         * if (startValue > endValue) { double tem = startValue; startValue =
         * endValue; endValue = tem; }
         */

        startEndInterval[0] = startValue;
        startEndInterval[1] = endValue;

        return startEndInterval;
    }

    public double[] getStonyLonForLatStartEndInterval() {

        double[] startEndInterval = new double[2];
        double startValue = -90 + hgln;
        if (startValue > 180)
            startValue = startValue - 360;
        if (startValue < -180) {
            startValue = startValue + 360;
        }
        double endValue = startValue + 180; // 90 + hgln;
        // if (headerData.isStereo()) {
        // if (endValue > 180) {
        // endValue = endValue - 360;
        // }
        // if (startValue > endValue) {
        // double tem = startValue;
        // startValue = endValue;
        // endValue = tem;
        // }
        // }

        startEndInterval[0] = startValue;
        startEndInterval[1] = endValue;
        return startEndInterval;
    }

    public double[] getCarrLonStartEndInterval() {
        double[] startEndInterval = new double[2];
        double endValue = 0.0;
        double startValue = -90 + crln - crln % latLonInterval;

        if (startValue > 180)
            startValue = startValue - 360;

        endValue = startValue + 180;

        if (startValue < -90 + crln) {
            startValue += latLonInterval;
        }

        if (endValue > 90 + crln) {
            endValue -= latLonInterval;
        }

        /*
         * if (startValue > endValue) { double tem = startValue; startValue =
         * endValue; endValue = tem; }
         */

        startEndInterval[0] = startValue;
        startEndInterval[1] = endValue;

        return startEndInterval;
    }

    public double[] getCarrLonForLatStartEndInterval() {

        double[] startEndInterval = new double[2];
        double startValue = -90 + crln;

        if (startValue > 180)
            startValue = startValue - 360;
        if (startValue < -180)
            startValue = startValue + 360;

        double endValue = startValue + 180;

        // if (headerData.isStereo()) {
        // if (endValue > 180) {
        // endValue = endValue - 360;
        // }
        //
        // if (startValue > endValue) {
        // double tem = startValue;
        // startValue = endValue;
        // endValue = tem;
        // }
        // }
        startEndInterval[0] = startValue;
        startEndInterval[1] = endValue;
        return startEndInterval;
    }

    public void createLatLines(IGraphicsTarget target) throws VizException,
            TransformException {

        initializeViewMinAndMaxXAndY();

        int latDrawingLineNumber = getLatDrawingLineNumber(latLonInterval);

        latCoordPointArrayList = new ArrayList<Coordinate[]>(
                latDrawingLineNumber);
        wireframeShapeForLatLineArray = new IWireframeShape[latDrawingLineNumber];

        double latValue = -90;
        for (int i = 0; i < wireframeShapeForLatLineArray.length
                && latValue <= 90; i++) {
            wireframeShapeForLatLineArray[i] = target.createWireframeShape(
                    false, descriptor);

            Coordinate[] latLonCoordArray = null;
            if (cylindrical == 0)
                latLonCoordArray = createCoordArrayForLatLine(latValue,
                        latLonDrawingPointInterval);
            else
                latLonCoordArray = createCylinCoordArrayForLatLine(latValue,
                        latLonDrawingPointInterval);

            if (latLonCoordArray != null) {
                latCoordPointArrayList.add(latLonCoordArray);
                wireframeShapeForLatLineArray[i]
                        .addLineSegment(latLonCoordArray);
                wireframeShapeForLatLineArray[i].compile();
            }
            latValue += latLonInterval;
        }

        latValue = -90;
        for (int i = 0; i < wireframeShapeForLatLineArray.length
                && latValue <= 90; i++) {
            wireframeShapeForLatLineArray[i].clearLabels();
            addDefaultLabelByPointIndex(wireframeShapeForLatLineArray[i],
                    String.valueOf((int) latValue),
                    latCoordPointArrayList.get(i), 0);
            updateEffectiveMinX(getMapMinX(), getViewMinX(), getViewMaxX());
            int pointIndexForAddingLabel = getPointLabelIndexForAddingLatLabel(
                    latCoordPointArrayList.get(i), zoomFactor,
                    getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(),
                    getEffectiveMaxY());

            if (cylindrical == 0) {
                if ((pointIndexForAddingLabel == 0)) {
                    addLabelOnLatLonLine(wireframeShapeForLatLineArray[i],
                            latCoordPointArrayList.get(i),
                            pointIndexForAddingLabel,
                            String.valueOf((int) latValue));
                }
            } else {
                addLabelOnLatLine(wireframeShapeForLatLineArray[i],
                        latCoordPointArrayList.get(i),
                        pointIndexForAddingLabel + 1,
                        String.valueOf((int) latValue));
            }
            latValue += latLonInterval;
        }

    }

    public void createLonLines(IGraphicsTarget target) throws VizException,
            TransformException {

        IFont labelFont = target.initializeFont(target.getDefaultFont()
                .getFontName(), 12, fontStyle);
        labelFont.setSmoothing(false);
        labelFont.setScaleFont(false);

        initializeViewMinAndMaxXAndY();

        int lonDrawingLineNumber = getLonDrawingLineNumber(latLonInterval);

        lonCoordPointArrayList = new ArrayList<Coordinate[]>(
                lonDrawingLineNumber);
        wireframeShapeForLonLineArray = new IWireframeShape[lonDrawingLineNumber];

        double startLonValue = (isCarrington ? getCarrLonStartEndInterval()[0]
                : getStonyLonStartEndInterval()[0]);
        double endLonValue = (isCarrington ? getCarrLonStartEndInterval()[1]
                : getStonyLonStartEndInterval()[1]);

        if (cylindrical != 0) {
            startLonValue = -180;
            endLonValue = 180;
        }

        double lonValue = startLonValue;
        for (int i = 0; i < wireframeShapeForLonLineArray.length
                && lonValue <= endLonValue; i++) {
            wireframeShapeForLonLineArray[i] = target.createWireframeShape(
                    false, descriptor);

            Coordinate[] latLonCoordArray = null;
            if (cylindrical == 0)
                latLonCoordArray = createCoordArrayForLonLine(lonValue,
                        latLonDrawingPointInterval);
            else
                latLonCoordArray = createCylinCoordArrayForLonLine(lonValue,
                        latLonDrawingPointInterval);

            lonCoordPointArrayList.add(latLonCoordArray);
            wireframeShapeForLonLineArray[i].addLineSegment(latLonCoordArray);//
            wireframeShapeForLonLineArray[i].compile();
            lonValue += latLonInterval;
        }

        lonValue = startLonValue;
        for (int i = 0; i < wireframeShapeForLonLineArray.length
                && lonValue <= endLonValue; i++) {
            wireframeShapeForLonLineArray[i].clearLabels();
            updateEffectiveMaxY(getMapMaxY(), getViewMinY(), getViewMaxY());
            int pointIndexForAddingLabel = getPointLabelIndexForAddingLonLabel(
                    lonCoordPointArrayList.get(i), zoomFactor,
                    getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(),
                    getEffectiveMaxY());

            int displayVal = (int) lonValue;

            if (lonValue > 180)
                displayVal = displayVal - 360;
            if (lonValue < -180)
                displayVal = displayVal + 360;

            if (cylindrical == 0) {
                if (pointIndexForAddingLabel != -1)
                    addLabelOnLatLonLine(wireframeShapeForLonLineArray[i],
                            lonCoordPointArrayList.get(i),
                            (pointIndexForAddingLabel + addToPoint),
                            String.valueOf(displayVal));
            } else
                addLabelOnLonLine(wireframeShapeForLonLineArray[i],
                        lonCoordPointArrayList.get(i),
                        (pointIndexForAddingLabel + 1), // have to be 1
                        String.valueOf(displayVal));
            lonValue += latLonInterval;
        }

        // add boundary to 2 ends
        Coordinate[] latLonCoordArray = null;
        wireframeShapeForLonLineArrayBnd = new IWireframeShape[2];
        wireframeShapeForLonLineArrayBnd[0] = target.createWireframeShape(
                false, descriptor);
        if (cylindrical == 0) {
            latLonCoordArray = createCoordArrayForLonLine(-90
                    + (isCarrington ? crln : hgln), latLonDrawingPointInterval);

            lonCoordPointArrayList.add(latLonCoordArray);
            wireframeShapeForLonLineArrayBnd[0]
                    .addLineSegment(latLonCoordArray);
            wireframeShapeForLonLineArrayBnd[0].compile();
        } else {
            latLonCoordArray = createCylinCoordArrayForLonLine(-179,
                    latLonDrawingPointInterval);
            lonCoordPointArrayList.add(latLonCoordArray);
            wireframeShapeForLonLineArrayBnd[0]
                    .addLineSegment(latLonCoordArray);
            wireframeShapeForLonLineArrayBnd[0].compile();

            wireframeShapeForLonLineArrayBnd[0].clearLabels();
            updateEffectiveMaxY(getMapMaxY(), getViewMinY(), getViewMaxY());
            int pointIndexForAddingLabel = getPointLabelIndexForAddingLonLabel(
                    lonCoordPointArrayList.get(0), zoomFactor,
                    getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(),
                    getEffectiveMaxY());

            addLabelOnLonLine(wireframeShapeForLonLineArrayBnd[0],
                    lonCoordPointArrayList.get(0),
                    (pointIndexForAddingLabel + 1), String.valueOf(-180));
        }

        wireframeShapeForLonLineArrayBnd[1] = target.createWireframeShape(
                false, descriptor);
        if (cylindrical == 0) {
            latLonCoordArray = createCoordArrayForLonLine(
                    90 + (isCarrington ? crln : hgln),
                    latLonDrawingPointInterval);

            lonCoordPointArrayList.add(latLonCoordArray);
            wireframeShapeForLonLineArrayBnd[1]
                    .addLineSegment(latLonCoordArray);
            wireframeShapeForLonLineArrayBnd[1].compile();
        } else {
            latLonCoordArray = createCylinCoordArrayForLonLine(179,
                    latLonDrawingPointInterval); // 180 won't draw
            lonCoordPointArrayList.add(latLonCoordArray);
            wireframeShapeForLonLineArrayBnd[1]
                    .addLineSegment(latLonCoordArray);
            wireframeShapeForLonLineArrayBnd[1].compile();

            wireframeShapeForLonLineArrayBnd[1].clearLabels();
            updateEffectiveMaxY(getMapMaxY(), getViewMinY(), getViewMaxY());
            int pointIndexForAddingLabel = getPointLabelIndexForAddingLonLabel(
                    lonCoordPointArrayList.get(0), zoomFactor,
                    getEffectiveMinX(), getEffectiveMaxX(), getEffectiveMinY(),
                    getEffectiveMaxY());

            addLabelOnLonLine(
                    wireframeShapeForLonLineArrayBnd[1],
                    lonCoordPointArrayList.get(lonCoordPointArrayList.size() - 1), // 360
                                                                                   // /
                                                                                   // latLonInterval),
                    (pointIndexForAddingLabel + 1), String.valueOf(180));
        }
    }

    public void drawLatLines(IGraphicsTarget target) throws VizException {

        IFont labelFont = target.initializeFont(target.getDefaultFont()
                .getFontName(), 12, fontStyle);
        labelFont.setSmoothing(false);
        labelFont.setScaleFont(false);

        for (int i = 0; i < wireframeShapeForLatLineArray.length; i++) {
            target.drawWireframeShape(wireframeShapeForLatLineArray[i], rgb,
                    1.0f, LineStyle.DEFAULT, labelFont);
        }
    }

    public void drawLonLines(IGraphicsTarget target) throws VizException,
            TransformException {

        IFont labelFont = target.initializeFont(target.getDefaultFont()
                .getFontName(), 12, fontStyle);
        labelFont.setSmoothing(false);
        labelFont.setScaleFont(false);

        for (int i = 0; i < wireframeShapeForLonLineArray.length; i++) {
            target.drawWireframeShape(wireframeShapeForLonLineArray[i], rgb,
                    1.0f, LineStyle.DEFAULT, labelFont);
        }

        for (int i = 0; i < wireframeShapeForLonLineArrayBnd.length; i++) {
            target.drawWireframeShape(wireframeShapeForLonLineArrayBnd[i], rgb,
                    1.0f, LineStyle.DEFAULT, labelFont);
        }
    }

    private Coordinate[] createCoordArrayForLatLine(double latValue,
            double latLonPointInterval) throws VizException {
        try {

            int coordArrayLength = (int) (180 / latLonPointInterval);// + 1;
            Coordinate[] coordArray = new Coordinate[coordArrayLength];
            Coordinate[] coordPixelArray = new Coordinate[coordArrayLength];

            double startLonValue = (isCarrington ? getCarrLonForLatStartEndInterval()[0]
                    : getStonyLonForLatStartEndInterval()[0]);
            double endLonValue = (isCarrington ? getCarrLonForLatStartEndInterval()[1]
                    : getStonyLonForLatStartEndInterval()[1]);

            double lon = startLonValue;

            for (int i = 0; i < coordArray.length && lon <= endLonValue; i++) {
                coordArray[i] = new Coordinate(lon, latValue);

                double[] coord = new double[2];
                coord[0] = coordArray[i].x;
                coord[1] = coordArray[i].y;

                double[] world = csConv.heliographicToHeliocentric(coord,
                        isCarrington);
                double[] pixel = new double[2];
                worldToPixel.transform(world, 0, pixel, 0, 1);

                Coordinate latLon = new Coordinate(pixel[0], pixel[1]); // +7,
                                                                        // -7
                if (latLon != null)
                    coordPixelArray[i] = latLon;

                lon += latLonPointInterval;

            }

            return coordPixelArray;

        } catch (TransformException e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
            throw new VizException(e.getMessage());
        }
    }

    private Coordinate[] createCylinCoordArrayForLatLine(double latValue,
            double latLonPointInterval) throws VizException {

        int coordArrayLength = (int) (360 / latLonPointInterval) + 1;

        Coordinate[] coordArray = new Coordinate[coordArrayLength];
        Coordinate[] coordPixelArray = new Coordinate[coordArrayLength];

        double lon = -180;
        for (int i = 0; i < coordArray.length && lon <= 180; i++) {
            coordArray[i] = new Coordinate(lon, latValue);

            double[] coord = new double[2];
            coord[0] = coordArray[i].x;
            coord[1] = coordArray[i].y;

            double[] pixel = new double[2];
            try {
                worldToPixel.transform(coord, 0, pixel, 0, 1);
            } catch (TransformException e) {
                UFStatus.getHandler().handle(
                        Priority.PROBLEM,
                        "Error in worldToPixel transform: "
                                + e.getLocalizedMessage(), e);
            }

            coordPixelArray[i] = new Coordinate(pixel[0], pixel[1]);// latLon;

            lon += latLonPointInterval;
        }

        return coordPixelArray;
    }

    private Coordinate[] createCoordArrayForLonLine(double lonValue,
            double latLonPointInterval) throws VizException {
        try {

            int coordArrayLength = (int) ((180 - 5) / latLonPointInterval);
            Coordinate[] coordArray = new Coordinate[coordArrayLength];
            Coordinate[] coordPixelArray = new Coordinate[coordArrayLength];
            double lat = -90 + latLonPointInterval;
            for (int i = 0; i < coordArray.length && lat <= 90; i++) {
                coordArray[i] = new Coordinate(lonValue, lat);
                lat += latLonPointInterval;

                double[] coord = new double[2];
                coord[0] = coordArray[i].x;
                coord[1] = coordArray[i].y;
                double world[] = csConv.heliographicToHeliocentric(coord,
                        isCarrington);
                double[] pixel = new double[2];
                worldToPixel.transform(world, 0, pixel, 0, 1);
                coordPixelArray[i] = new Coordinate(pixel[0], pixel[1]);
            }

            return coordPixelArray;

        } catch (TransformException e) {
            throw new VizException(e.getMessage());
        }
    }

    private Coordinate[] createCylinCoordArrayForLonLine(double lonValue,
            double latLonPointInterval) throws VizException {
        int coordArrayLength = (int) (180 / latLonPointInterval);// +1;

        Coordinate[] coordArray = new Coordinate[coordArrayLength];
        Coordinate[] coordPixelArray = new Coordinate[coordArrayLength];

        double lat = -90; // + latLonPointInterval;
        for (int i = 0; i < coordArray.length && lat <= 90; i++) {
            coordArray[i] = new Coordinate(lonValue, lat);

            double[] coord = new double[2];
            coord[0] = coordArray[i].x;
            coord[1] = coordArray[i].y;

            double[] pixel = new double[2];
            try {
                worldToPixel.transform(coord, 0, pixel, 0, 1);
            } catch (TransformException e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }

            coordPixelArray[i] = new Coordinate(pixel[0], pixel[1]);
            lat += latLonPointInterval;
        }

        return coordPixelArray;
    }

    private int getLatDrawingLineNumber(int latInterval) {
        int latLineNumber = 180 / 15; // set a default value
        if (latInterval > 0 && latInterval <= 180) {
            latLineNumber = 180 / latInterval;
        }
        return latLineNumber + 1;
    }

    private int getLonDrawingLineNumber(int lonInterval) {
        int lonLineNumber = 360 / 15; // set a default value
        if (lonInterval > 0 && lonInterval <= 360) {
            lonLineNumber = 360 / lonInterval;
            if (lonLineNumber > 360)
                lonLineNumber = 360; // if we draw 360 lines, the last line will
                                     // overlap with the first line
        }
        return lonLineNumber + 1;
    }

    private void addDefaultLabelByPointIndex(IWireframeShape wireframeShape,
            String labelValue, Coordinate[] latLonCoordinateArray,
            int defaultPointIndex) throws TransformException {
        double[] tmp = { latLonCoordinateArray[defaultPointIndex].x,
                latLonCoordinateArray[defaultPointIndex].y };
        double[] screenPixel = descriptor.worldToPixel(tmp);// new double[2];

        if (screenPixel != null)
            wireframeShape.addLabel(labelValue, screenPixel);
    }

    private void addLabelOnLatLonLine(IWireframeShape wireframeShape,
            Coordinate[] coordinateArray, int coordiantePointIndex, String label)
            throws TransformException {

        if (coordiantePointIndex < 175) {

            double[] tmp = { coordinateArray[coordiantePointIndex].x,
                    coordinateArray[coordiantePointIndex].y };
            double[] screenPixel = descriptor.worldToPixel(tmp);// new
                                                                // double[2];
            // imageDisplay.getWorldToPixel().transform(tmp, 0, screenPixel, 0,
            // 1);// descriptor.worldToPixel(tmp);
            if (screenPixel != null) {
                wireframeShape.clearLabels();
                wireframeShape.addLabel(label, screenPixel);
            }
        }
    }

    private void addLabelOnLatLine(IWireframeShape wireframeShape,
            Coordinate[] coordinateArray, int coordiantePointIndex, String label)
            throws TransformException {

        // if (coordiantePointIndex < 175) {

        double[] tmp = { coordinateArray[coordiantePointIndex].x - 15,
                coordinateArray[coordiantePointIndex].y };
        double[] screenPixel = descriptor.worldToPixel(tmp);

        if (screenPixel != null) {
            wireframeShape.clearLabels();
            wireframeShape.addLabel(label, screenPixel);
        }
        // }
    }

    private void addLabelOnLonLine(IWireframeShape wireframeShape,
            Coordinate[] coordinateArray, int coordiantePointIndex, String label)
            throws TransformException {

        if (coordiantePointIndex < 175) {

            double[] tmp = { coordinateArray[coordiantePointIndex].x,
                    coordinateArray[coordiantePointIndex].y - 10 };// move label
                                                                   // down
            double[] screenPixel = descriptor.worldToPixel(tmp);

            if (screenPixel != null) {
                wireframeShape.clearLabels();
                wireframeShape.addLabel(label, screenPixel);
            }
        }
    }

    private int getPointLabelIndexForAddingLatLabel(
            Coordinate[] latLonCoordinateArray, float zoomFactor, double minX,
            double maxX, double minY, double maxY) throws TransformException {
        int pointIndex = -1;
        if (latLonCoordinateArray == null || latLonCoordinateArray.length == 0)
            return pointIndex;

        double positionOffset = 120;
        for (int i = 0; i < latLonCoordinateArray.length; i++) {
            double[] tmp = { latLonCoordinateArray[i].x,
                    latLonCoordinateArray[i].y };
            double[] screenPixel = descriptor.worldToPixel(tmp);// new
                                                                // double[2];
            worldToPixel.transform(tmp, 0, screenPixel, 0, 1);// descriptor.worldToPixel(tmp);
            if (isPointForPlacingLatituteLabel(screenPixel, positionOffset,
                    zoomFactor, minX, maxX, minY, maxY)) {
                pointIndex = i;
                break;
            }
        }
        return pointIndex;
    }

    private int getPointLabelIndexForAddingLonLabel(
            Coordinate[] latLonCoordinateArray, float zoomFactor, double minX,
            double maxX, double minY, double maxY) throws TransformException {
        int pointIndex = -1;
        if (latLonCoordinateArray == null || latLonCoordinateArray.length == 0)
            return pointIndex;

        double positionOffset = 120;
        for (int i = 0; i < latLonCoordinateArray.length; i++) {
            double[] tmp = { latLonCoordinateArray[i].x,
                    latLonCoordinateArray[i].y };

            if (cylindrical == 0) {

                double[] world = new double[2];
                pixelToWorld.transform(tmp, 0, world, 0, 1);

                double[] locWorld = csConv.heliocentricToHeliographic(world,
                        isCarrington);

                if (locWorld[1] > -0.5 && locWorld[1] < 0.5) {
                    pointIndex = i;
                    break;
                }
            } else {

                double[] screenPixel = descriptor.worldToPixel(tmp);
                worldToPixel.transform(tmp, 0, screenPixel, 0, 1);// descriptor.worldToPixel(tmp);

                if (isPointForPlacingLongituteLabel(screenPixel,
                        positionOffset, zoomFactor, minX, maxX, minY, maxY)) {
                    pointIndex = i;
                    break;
                }
            }
        }
        return pointIndex;
    }

    private boolean isPointForPlacingLatituteLabel(double[] pixelValueArray,
            double positionOffset, float zoomFactor, double minX, double maxX,
            double minY, double maxY) {
        double delta = 200;
        double adjustedOffset = positionOffset * zoomFactor;
        boolean isPointForLabel = false;
        if (pixelValueArray != null) {
            if (pixelValueArray[0] > (minX + adjustedOffset)
                    && pixelValueArray[0] < maxX && pixelValueArray[1] > minY
                    && pixelValueArray[1] < maxY) {
                if (Math.abs(pixelValueArray[0] - minX
                        - (positionOffset * zoomFactor)) < delta) {
                    isPointForLabel = true;
                }
            }
        }
        return isPointForLabel;
    }

    private boolean isPointForPlacingLongituteLabel(double[] pixelValueArray,
            double positionOffset, float zoomFactor, double minX, double maxX,
            double minY, double maxY) {
        double delta = 200;
        double adjustedOffset = positionOffset * zoomFactor;
        boolean isPointForLabel = false;
        if (pixelValueArray != null) {
            if (pixelValueArray[0] > minX && pixelValueArray[0] < maxX
                    && pixelValueArray[1] > minY && pixelValueArray[1] < maxY) {
                if (Math.abs(pixelValueArray[1] - maxY) < delta) {
                    isPointForLabel = true;
                }
            }
        }
        return isPointForLabel;
    }

    private void updateEffectiveMinX(double mapMinXValue, double viewMinXValue,
            double viewMaxXValue) {
        if (isLeftEdgeOfMapInsideCurrentView(mapMinXValue, viewMinXValue,
                viewMaxXValue)) {
            effectiveMinX = mapMinXValue;
        }
    }

    private boolean isLeftEdgeOfMapInsideCurrentView(double minXOfMap,
            double minXOfCurrentView, double maxXOfCurrentView) {
        boolean isInsideResult = false;
        if (minXOfMap > minXOfCurrentView && minXOfMap < maxXOfCurrentView)
            isInsideResult = true;
        return isInsideResult;
    }

    private void updateEffectiveMaxY(double mapMaxYValue, double viewMinYValue,
            double viewMaxYValue) {
        if (isBottomEdgeOfMapInsideCurrentView(mapMaxYValue, viewMinYValue,
                viewMaxYValue))
            effectiveMaxY = mapMaxYValue;
    }

    private boolean isBottomEdgeOfMapInsideCurrentView(double maxYOfMap,
            double minYOfCurrentView, double maxYOfCurrentView) {
        boolean isInsideResult = false;
        if (maxYOfMap > minYOfCurrentView && maxYOfMap < maxYOfCurrentView) {
            isInsideResult = true;
        }
        return isInsideResult;
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

    public double getEffectiveMinX() {
        return effectiveMinX;
    }

    public double getEffectiveMaxY() {
        return effectiveMaxY;
    }

    public double getEffectiveMinY() {
        return effectiveMinY;
    }

    public double getEffectiveMaxX() {
        return effectiveMaxX;
    }

    public boolean isCarrington() {
        return isCarrington;
    }

    public void setCarrington(boolean isCarrington) {
        this.isCarrington = isCarrington;
    }

    public void dispose() {

        if (wireframeShapeForLatLineArray != null) {
            for (int i = 0; i < wireframeShapeForLatLineArray.length; i++)
                if (wireframeShapeForLatLineArray[i] != null)
                    wireframeShapeForLatLineArray[i].dispose();
        }

        if (wireframeShapeForLonLineArray != null) {
            for (int i = 0; i < wireframeShapeForLonLineArray.length; i++)
                if (wireframeShapeForLonLineArray[i] != null)
                    wireframeShapeForLonLineArray[i].dispose();
        }

        if (wireframeShapeForLonLineArrayBnd != null) {
            for (int i = 0; i < wireframeShapeForLonLineArrayBnd.length; i++)
                if (wireframeShapeForLonLineArrayBnd[i] != null)
                    wireframeShapeForLonLineArrayBnd[i].dispose();
        }

        if (fontStyle != null)
            fontStyle = null;

    }
}
