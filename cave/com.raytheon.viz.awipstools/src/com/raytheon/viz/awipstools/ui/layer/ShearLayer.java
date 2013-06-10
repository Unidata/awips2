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
package com.raytheon.viz.awipstools.ui.layer;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.converter.UnitConverter;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.ui.input.EditableManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.operation.buffer.BufferOp;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 15Mar2013	15693	mgamazaychikov	Added magnification capability.
 * 05/02/2013   DR 14587   D. Friedman Use base velocity.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ShearLayer extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> {

    private LineString baseline = null;

    private GeometryFactory gf = new GeometryFactory();

    protected GeodeticCalculator gc;

    private UnitConverter nmToMeter = javax.measure.unit.NonSI.NAUTICAL_MILE
            .getConverterTo(javax.measure.unit.SI.METER);

    private IInputHandler mouseHandler;

    private float endCircleRadius;

    public ShearLayer(GenericToolsResourceData<VRShearLayer> data,
            LoadProperties props, MapDescriptor descriptor) {
        super(data, props);
        setDescriptor(descriptor);
        gc = new GeodeticCalculator(this.descriptor.getCRS());
        this.baseline = createDefaultBaseline();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(mouseHandler);
        }
    }

    /**
     * @return the mouseHandler
     */
    public IInputHandler getMouseHandler() {
        return mouseHandler;
    }

    /**
     * @param mouseHandler
     *            the mouseHandler to set
     */
    public void setMouseHandler(IInputHandler mouseHandler) {
        this.mouseHandler = mouseHandler;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#getCoordinateReferenceSystem()
     */
    public CoordinateReferenceSystem getCoordinateReferenceSystem() {
        if (descriptor == null) {
            return null;
        }

        return descriptor.getCRS();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        gc = new GeodeticCalculator(descriptor.getCRS());
        EditableManager.makeEditable(this, true);

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null && mouseHandler != null) {
            container.registerMouseHandler(mouseHandler);
        }
    }

    public boolean isApplicable(PixelExtent extent) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        LineString ls = getBaseline();
        RGB color = this.getCapability(ColorableCapability.class).getColor();
        drawEndCircles(target, ls.getCoordinates(), paintProps.getZoomLevel());
        drawLineString(target, ls, color, IGraphicsTarget.LineStyle.SOLID);
        String label = drawLabeling(target, ls, color, paintProps);
        target.clearClippingPlane();
        drawUpperLeftCornerLabel(target, paintProps, label);
    }

    public void drawLines(Coordinate[] coors) {

    }

    public void drawLineString(IGraphicsTarget target, LineString lineString,
            RGB theColor, IGraphicsTarget.LineStyle lineStyle)
            throws VizException {
        IWireframeShape line = target.createWireframeShape(true, descriptor);

        line.addLineSegment(lineString.getCoordinates());

        if (lineStyle.equals(IGraphicsTarget.LineStyle.SOLID)) {
            target.drawWireframeShape(line, theColor, 1);
        } else {
            target.drawWireframeShape(line, theColor, 1,
                    IGraphicsTarget.LineStyle.DASHED);
        }
        line.dispose();
    }

    private void drawEndCircles(IGraphicsTarget target, Coordinate[] latLongs,
            float zoomLevel) throws VizException {

        for (int i = 0; i < latLongs.length; i++) {
            Coordinate c = latLongs[i];
            double[] dd = descriptor.worldToPixel(new double[] { c.x, c.y });
            this.endCircleRadius = zoomLevel * 50;
            DrawableCircle dc = new DrawableCircle();
            dc.basics.x = dd[0];
            dc.basics.y = dd[1];
            dc.radius = (double) this.endCircleRadius;
            dc.basics.color = this.getCapability(ColorableCapability.class)
                    .getColor();
            dc.lineWidth = 1.0f;
            dc.numberOfPoints = 24;
            target.drawCircle(dc);
        }
    }

    public LinearRing getCircle(Coordinate coor, double radius) {

        Coordinate circleCoordinates[] = new Coordinate[361];
        Coordinate firstCoor = null;

        for (int i = 0; i < 360; i++) {
            circleCoordinates[i] = getCoordinateOnCircle(coor, radius, i);
            if (i == 0) {
                // save the first coordinate, to add to end, so completes the
                // ring.
                firstCoor = circleCoordinates[0];
            }
        }
        circleCoordinates[360] = firstCoor;

        return gf.createLinearRing(circleCoordinates);
    }

    public Coordinate getCoordinateOnCircle(Coordinate coor, double radius,
            int angle) {

        double centerPixel[] = descriptor.worldToPixel(new double[] { coor.x,
                coor.y });
        double pointOnCircle[] = new double[2];

        pointOnCircle[0] = centerPixel[0] + radius
                * Math.cos(Math.toRadians(angle));
        pointOnCircle[1] = centerPixel[1] + radius
                * Math.sin(Math.toRadians(angle));
        double dd[] = descriptor.pixelToWorld(new double[] { pointOnCircle[0],
                pointOnCircle[1] });
        Coordinate coorOnCircle = new Coordinate(dd[0], dd[1]);

        return coorOnCircle;

    }

    protected void drawBaselineLabel(IGraphicsTarget target,
            Coordinate latLong, String label) throws VizException {

        double c1[] = descriptor.worldToPixel(new double[] { latLong.x,
                latLong.y });

        double c2[] = target.getPointOnCircle(c1[0], c1[1], 0.0, 6, 0);
        DrawableString ds = new DrawableString(label, this.getCapability(
                ColorableCapability.class).getColor());
        ds.basics.x = c2[0];
        ds.basics.y = c2[1];
        ds.font = null;
        ds.textStyle = IGraphicsTarget.TextStyle.NORMAL;
        ds.horizontalAlignment = HorizontalAlignment.LEFT;
        // set the magnification
        ds.magnification = this.getCapability(MagnificationCapability.class)
        .getMagnification().floatValue();
        target.drawStrings(ds);
    }

    public LineString getBaseline() {
        return baseline;
    }

    public LineString createDefaultBaseline() {

        Coordinate center = PointsDataManager.getInstance().getHome();

        gc.setStartingGeographicPoint(center.x, center.y);
        double meters = nmToMeter.convert(27);
        gc.setDirection(90, meters);
        Coordinate c2 = new Coordinate(gc.getDestinationGeographicPoint()
                .getX(), gc.getDestinationGeographicPoint().getY());

        Coordinate coors[] = { center, c2 };

        return gf.createLineString(coors);
    }

    public boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable();
    }

    /**
     * Checks and returns the coordinate of the endpoint that clicked in.
     * 
     * @param c
     *            Coordinate of the click.
     * @return Coordinate of the endpoint, null if not found.
     */
    public Coordinate isInsideEndpoint(Coordinate c) {

        return getEndpointClickedIn(c);
    }

    public void moveBaseline(Coordinate delta, int index) {

        for (Coordinate point : baseline.getCoordinates()) {
            point.x += delta.x;
            point.y += delta.y;
        }
        issueRefresh();
    }

    /**
     * Moves the point to the new coordinates. (Typically user drags the
     * point...)
     * 
     * @param delta
     *            The change of the point to update.
     * @param pointToUpdate
     *            The point to Update.
     */
    public void movePoint(Coordinate delta, Coordinate pointToUpdate) {

        for (Coordinate point : baseline.getCoordinates()) {
            if (point.equals(pointToUpdate)) {
                point.x += delta.x;
                point.y += delta.y;
            }
        }
        issueRefresh();
    }

    /**
     * Returns the endpoint the user clicked in, or null if they didn't click in
     * an endpoint.
     * 
     * @param coor
     *            Coordinate of the click point.
     * @return The coordinate of the endpoint they hit on.
     */
    public Coordinate getEndpointClickedIn(Coordinate coor) {

        Coordinate c1 = getBaseline().getCoordinates()[0];
        Coordinate c2 = getBaseline().getCoordinates()[1];

        if (gf.createPolygon(getCircle(c1, this.endCircleRadius), null)
                .contains(gf.createPoint(coor))) {
            return c1;
        } else if (gf.createPolygon(getCircle(c2, this.endCircleRadius), null)
                .contains(gf.createPoint(coor))) {
            return c2;
        }

        return null;
    }

    /**
     * Return the index of the linestring the user clicked in (for move for
     * instance).
     * 
     * @param coor
     * @return int Index of line they matched on.
     */
    public int isInsideLine(Coordinate coor) {

        int index = -1;
        for (int i = 0; i < getBuffer().length; i++) {
            if (getBuffer()[i].contains(gf.createPoint(coor))) {
                index = i;
            }
        }
        return index;
    }

    public Geometry[] getBuffer() {

        Geometry[] buffer = new Geometry[1];

        buffer[0] = BufferOp.bufferOp(getBaseline(), 0.001);

        return buffer;
    }

    /**
     * Draw label in the upper left corner.
     * 
     * @param target
     * @param paintProps
     * @param label
     * @throws VizException
     */
    protected void drawUpperLeftCornerLabel(IGraphicsTarget target,
            PaintProperties paintProps, String label) throws VizException {
        // TODO this screen location code is borrowed from MPELegendResource...
        // should it be put into a shared class, possibly a paint
        // properties method?
        IExtent screenExtent = paintProps.getView().getExtent();
        double scale = (screenExtent.getHeight() / paintProps.getCanvasBounds().height);
        DrawableString tmpDS = new DrawableString("0", new RGB(100, 100, 100));
        tmpDS.font = null;
        double textHeight = target.getStringsBounds(tmpDS).getHeight() * scale;
        double padding = 3 * scale;
        double textSpace = textHeight + padding;
        double cmapHeight = textHeight * 1.25;
        double legendHeight = cmapHeight + 2.0 * textSpace + 2.0 * padding;
        double y1 = screenExtent.getMinY() + legendHeight * 2.5;
        double x1 = screenExtent.getMinX() + padding * 10.0;
        DrawableString ds = new DrawableString(label, this.getCapability(
                ColorableCapability.class).getColor());
        ds.basics.x = x1;
        ds.basics.y = y1;
        ds.font = null;
        ds.textStyle = IGraphicsTarget.TextStyle.NORMAL;
        ds.horizontalAlignment = HorizontalAlignment.LEFT;
        // set the magnification
        ds.magnification = this.getCapability(MagnificationCapability.class)
        .getMagnification().floatValue();
        target.drawStrings(ds);
    }

    public String drawLabeling(IGraphicsTarget target, LineString lineString,
            RGB theColor, PaintProperties paintProps) throws VizException {
        return "NO DATA";
    }

    protected String calculateShearLabel(double length, Coordinate sCoor,
            Coordinate eCoor, Coordinate midpointCoor) throws VizException {
        return "NO DATA";
    }

    protected float getRangeValue(Map<String, Object> map) {
        if (map != null && map.containsKey("Mnemonic")
                && map.containsKey("Range")) {
            String mnemonic = map.get("Mnemonic").toString();

            if (mnemonic.equalsIgnoreCase("V")
                    || mnemonic.equalsIgnoreCase("SRM")
                    || mnemonic.equalsIgnoreCase("HV")) {
                String s = map.get("Range").toString();
                if (s != null && !s.equalsIgnoreCase("NO DATA")) {
                    int valueIndex = s.indexOf("nm");

                    if (valueIndex >= 0) {
                        return Float.valueOf(s.substring(0, valueIndex));
                    }
                }
            }
        }
        return 0.0f;
    }

    protected static class VelocityRange {
        private static String VALUE_KEY = "Value";

        private static String BASE_VELOCITY_VALUE_KEY = "baseVelocity-Value";

        Float lower;

        Float upper;

        String separatorSymbol;

        public VelocityRange(Map<String, Object> map) {
            if (map != null && map.containsKey("Mnemonic")
                    && (map.containsKey(BASE_VELOCITY_VALUE_KEY) ||
                            map.containsKey(VALUE_KEY))) {
                String mnemonic = map.get("Mnemonic").toString();

                if (mnemonic.equalsIgnoreCase("V")
                        || mnemonic.equalsIgnoreCase("SRM")
                        || mnemonic.equalsIgnoreCase("HV")) {
                    String s = map.get(BASE_VELOCITY_VALUE_KEY).toString();
                    if (s == null)
                        s = map.get(VALUE_KEY).toString();
                    if (s != null && !s.equalsIgnoreCase("NO DATA")) {
                        final String corePatternStr = "-?[0-9]+\\.[0-9]+";
                        final String symbolPatternStr = "[<>]";

                        StringBuilder lowerPatternString = new StringBuilder(
                                "^").append(corePatternStr);
                        Pattern lowerPattern = Pattern
                                .compile(lowerPatternString.toString());
                        Matcher lowerMatcher = lowerPattern.matcher(s);
                        if (lowerMatcher.find()) {
                            String match = lowerMatcher.group();
                            this.lower = Float.valueOf(match);
                        }

                        Pattern symbolPattern = Pattern
                                .compile(symbolPatternStr);
                        Matcher symbolMatcher = symbolPattern.matcher(s);
                        if (symbolMatcher.find()) {
                            this.separatorSymbol = symbolMatcher.group();
                        }

                        StringBuilder upperPatternString = new StringBuilder(
                                corePatternStr);
                        Pattern upperPattern = Pattern
                                .compile(upperPatternString.toString());
                        Matcher upperMatcher = upperPattern.matcher(s);
                        if (upperMatcher.find()
                                && (this.separatorSymbol != null || upperMatcher
                                        .find())) {
                            String match = upperMatcher.group();
                            this.upper = Float.valueOf(match);
                        }
                    }
                }
            }
        }

        public boolean hasRange() {
            return (hasLower() && hasUpper());
        }

        public boolean hasLower() {
            return lower != null;
        }

        public boolean hasUpper() {
            return upper != null;
        }

        public Float add(VelocityRange rhs) {
            if (this.hasRange() && rhs.hasRange()) {
                float upper = (this.upper - rhs.upper) / 2;
                float lower = (this.lower - rhs.lower) / 2;
                return lower + upper;
            } else if (this.hasUpper() && rhs.hasUpper()) {
                return (this.upper - rhs.upper) / 2;
            } else if (this.hasLower() && rhs.hasLower()) {
                return (this.lower - rhs.lower) / 2;
            } else {
                return null;
            }
        }

        public String pickSeparatorSymbol(VelocityRange other) {
            if (this.separatorSymbol != null) {
                return this.separatorSymbol;
            } else if (other.separatorSymbol != null) {
                return other.separatorSymbol;
            } else {
                return "";
            }
        }
    }

}
