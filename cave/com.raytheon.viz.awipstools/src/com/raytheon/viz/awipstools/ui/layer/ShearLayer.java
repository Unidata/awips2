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

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
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
import com.raytheon.viz.awipstools.common.ToolsUiUtil;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Interactive resource for rendering the Shear data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer       Description
 * ------------- -------- -------------- --------------------------
 * Mar 15, 2013  15693    mgamazaychikov Added magnification capability.
 * May 02, 2013  14587    D. Friedman    Use base velocity.
 * Aug 29, 2013  2281     bsteffen       Fix click distance calculations.
 * Sep 03, 2013  2310     bsteffen       Move MouseHandler from ShearAction to
 *                                       ShearLayer.
 * Mar  3, 2014  2804     mschenke       Set back up clipping pane
 * Jul 28, 2014  3430     mapeters       Updated the 'handleMouseUp' function 
 *                                       to prevent errors when MB3 clicking off 
 *                                       the map with tool in editable mode.
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
        this.mouseHandler = new MouseHandler(this);
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
        try {
            drawUpperLeftCornerLabel(target, paintProps, label);
        } finally {
            target.setupClippingPlane(paintProps.getClippingPane());
        }
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
     * Get the closest endpoint to the provided screen location.
     * 
     * @param refX
     *            x location in screen pixels
     * @param refY
     *            y location in screen pixels
     * @return T Coordinate of the endpoint, null if not found.
     */
    public Coordinate isInsideEndpoint(int x, int y) {
        IDisplayPaneContainer container = getResourceContainer();
        if (container == null) {
            return null;
        }
        Coordinate[] coords = getBaseline().getCoordinates();
        int idx = ToolsUiUtil.closeToCoordinate(container, coords, x, y, 9);
        if (idx < 0) {
            return null;
        } else {
            return coords[idx];
        }
    }

    /**
     * Return the index of the linestring the user clicked in (for move for
     * instance).
     * 
     * @param refX
     *            x location of reference point in screen pixels
     * @param refY
     *            y location of reference point in screen pixels
     * @return int Index of line they matched on.
     */
    public int isInsideLine(int x, int y) {
        IDisplayPaneContainer container = getResourceContainer();
        Coordinate[] coords = getBaseline().getCoordinates();
        if (container != null
                && ToolsUiUtil.closeToLine(container, coords, x, y, 15)) {
            return 0;
        } else {
            return -1;
        }
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
            if (map != null
                    && map.containsKey("Mnemonic")
                    && (map.containsKey(BASE_VELOCITY_VALUE_KEY) || map
                            .containsKey(VALUE_KEY))) {
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

    /**
     * Associated navigation modes:
     * <UL>
     * <LI>CREATE - Create the initial baseline
     * <LI>MOVE_LINE - Move an existing line within the baseline.
     * <LI>MOVE_POINT - Move one endpoint of an existing baseline linestring.
     * <LI>PAN - Allow other tools, such as pan, to have control
     */
    private static enum Mode {
        CREATE, MOVE_LINE, MOVE_POINT, PAN
    };

    private static class MouseHandler extends InputAdapter {

        private int indexOfMovedEndpoint;

        private ShearLayer shearLayer;

        /** The mode of the mouse. By default, create */
        private Mode mode = Mode.CREATE;

        /** The last mouse position - x */
        private int lastMouseX = -1;

        /** The last mouse position - y */
        private int lastMouseY = -1;

        /** The index of the line to be moved */
        private int lineToMove;

        /** The millisecond time of the right mouse button down event */
        private long rightMouseButtonDownTime;

        private Coordinate coordinateMoved;

        private Coordinate coordinateFound = null;

        public MouseHandler(ShearLayer shearLayer) {
            this.shearLayer = shearLayer;
        }

        public boolean handleMouseDown(int x, int y, int mouseButton) {
            lastMouseX = x;
            lastMouseY = y;

            if (shearLayer.isEditable()) {

                if (mouseButton == 1) {
                    lineToMove = -1;
                    coordinateFound = shearLayer.isInsideEndpoint(x, y);

                    if (coordinateFound != null) {
                        this.mode = Mode.MOVE_POINT;
                        coordinateMoved = coordinateFound;
                        return true;
                    }

                    if ((lineToMove = shearLayer.isInsideLine(x, y)) != -1) {
                        this.mode = Mode.MOVE_LINE;
                        return true;
                    }
                } else if (mouseButton == 3) {
                    // move prior unmoved end point
                    this.rightMouseButtonDownTime = System.currentTimeMillis();
                }
            } else {
                this.mode = Mode.PAN;
            }
            shearLayer.issueRefresh();
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.map.IMouseHandler#handleMouseDownMove(int,
         * int)
         */
        public boolean handleMouseDownMove(int x, int y, int button) {

            if (button != 1)
                return false;

            if (this.mode == Mode.PAN)
                return false;

            if (this.mode == Mode.MOVE_LINE || this.mode == Mode.MOVE_POINT) {
                IDisplayPaneContainer container = shearLayer
                        .getResourceContainer();
                if (container == null) {
                    return false;
                }
                Coordinate c = container.translateClick(lastMouseX, lastMouseY);
                Coordinate c2 = container.translateClick(x, y);

                Coordinate delta = new Coordinate(c2.x - c.x, c2.y - c.y);

                if (this.mode == Mode.MOVE_LINE) {
                    shearLayer.moveBaseline(delta, this.lineToMove);
                } else {
                    shearLayer.movePoint(delta, coordinateMoved);
                }

                lastMouseX = x;
                lastMouseY = y;
                shearLayer.issueRefresh();
                return true;
            }

            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.map.IMouseHandler#handleMouseUp(int, int)
         */
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 3) {
                if (System.currentTimeMillis() - this.rightMouseButtonDownTime < 275) {
                    IDisplayPaneContainer container = shearLayer
                            .getResourceContainer();
                    if (container == null) {
                        return false;
                    }
                    Coordinate c = container.translateClick(x, y);

                    if (c == null) {
                        return false;
                    }

                    // move prior unmoved end point
                    Coordinate[] coords = shearLayer.getBaseline()
                            .getCoordinates();
                    indexOfMovedEndpoint = (indexOfMovedEndpoint >= coords.length - 1) ? 0
                            : ++indexOfMovedEndpoint;
                    Coordinate coord = coords[indexOfMovedEndpoint];
                    Coordinate delta = new Coordinate(c.x - coord.x, c.y
                            - coord.y, c.z - coord.z);

                    shearLayer.movePoint(delta, coord);
                }
            } else if (this.mode == Mode.PAN)
                return false;

            // Default back to pan operation
            mode = Mode.PAN;
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.mouse.IMouseHandler#handleMouseMove(int,
         * int)
         */
        public boolean handleMouseMove(int x, int y) {
            if (shearLayer.isEditable()) {

                if (shearLayer.isInsideEndpoint(x, y) != null) {
                    // Change the cursor to a hand.
                    this.setCursorHand();
                    return true;
                }

                if (shearLayer.isInsideLine(x, y) != -1) {
                    // Change the cursor to crosshairs.
                    this.setCursorCross();
                    return true;
                }
            }

            this.changeCursorNormal();
            return false;
        }

        protected void changeCursorNormal() {
            this.updateCursorStandard(SWT.CURSOR_ARROW);
        }

        private void setCursorHand() {
            this.updateCursorStandard(SWT.CURSOR_HAND);
        }

        private void setCursorCross() {
            this.updateCursorStandard(SWT.CURSOR_SIZEALL);
        }

        private void updateCursorStandard(int cursorEnum) {
            IWorkbenchWindow window = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow();

            window.getShell().setCursor(
                    window.getShell().getDisplay().getSystemCursor(cursorEnum));
        }

    }

}
