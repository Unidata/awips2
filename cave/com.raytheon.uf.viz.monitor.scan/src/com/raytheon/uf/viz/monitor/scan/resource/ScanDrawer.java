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
package com.raytheon.uf.viz.monitor.scan.resource;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.TVSTableDataRow;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.DmdDisplayFilterConfig;
import com.raytheon.uf.common.monitor.scan.config.StormCellConfig;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.monitor.scan.config.ScanRunConfigMgr;
import com.raytheon.uf.viz.monitor.scan.xml.ScanVcpXML;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Draw Hexagons for SCAN
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2009   2307       dhladky     Initial creation
 * Apr 22, 2013   1926       njensen     Faster rendering
 * May 09, 2014   3145       mpduff      Add getter for font so it can be disposed, javadoc fix
 * Jul 22, 2014   3422       mapeters    Updated deprecated drawArc() call.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ScanDrawer {

    private static final int HEX_ANGLE = 60;

    private static final double SIN_HEX_ANGLE = Math.sin(HEX_ANGLE);

    public static final RGB red = new RGB(255, 0, 0);

    public static final RGB yellow = new RGB(255, 255, 0);

    public static final RGB white = new RGB(255, 255, 255);

    private RGB rscColor = null;

    private final GeometryFactory factory = new GeometryFactory();

    // defaults
    private double screenToWorldRatio = 0.0;

    public IFont font = null;

    private double side = 0.0;

    private double topY = 0.0;

    private double bottomY = 0.0;

    private double leftX = 0.0;

    private double rightX = 0.0;

    private double wLeftX = 0.0;

    private double wRightX = 0.0;

    private double hexRadius = 0.0;

    /** Cell's center values */
    private double[] center = null;

    private RGB color = null;

    /** Cell's center point */
    private Point centerPoint = null;

    private List<double[]> vertexes = null;

    protected StormCellConfig sdc = null;

    protected DmdDisplayFilterConfig ddfc = null;

    private GeodeticCalculator gc = null;

    private MapDescriptor descriptor = null;

    private IGraphicsTarget aTarget = null;

    private int outlineWidth = 1;

    private static final int outerSideFactor = 2;

    private static final int outerTopBottomFactor = 4;

    private int mesoType = 2;

    private boolean isExt = false;

    /**
     * public constructor CELL
     * 
     * @param stormCellConfig
     * @param gc
     */
    public ScanDrawer(StormCellConfig stormCellConfig, GeodeticCalculator gc) {
        this.sdc = stormCellConfig;
        this.gc = gc;
    }

    /**
     * public constructor DMD
     * 
     * @param sdc
     * @param gc
     */
    public ScanDrawer(DmdDisplayFilterConfig ddfc, GeodeticCalculator gc) {
        this.ddfc = ddfc;
        this.gc = gc;
    }

    public boolean isHexagonEnabled() {
        boolean enabled = false;
        if (color != null) {
            if (sdc.getSymsCircleLow() && color.equals(white)) {
                enabled = true;
            } else if (sdc.getSymsCircleMid() && color.equals(yellow)) {
                enabled = true;
            } else if (sdc.getSymsCircleHigh() && color.equals(red)) {
                enabled = true;
            }
        }

        return enabled;
    }

    public boolean isArrowEnabled() {
        boolean enabled = false;

        if (color != null) {
            if (sdc.getSymsArrowLow() && color.equals(white)) {
                enabled = true;
            } else if (sdc.getSymsArrowMid() && color.equals(yellow)) {
                enabled = true;
            } else if (sdc.getSymsArrowHigh() && color.equals(red)) {
                enabled = true;
            }
        }

        return enabled;
    }

    public boolean isIDEnabled() {
        boolean enabled = false;

        if (color != null) {
            if (sdc.getSymsIdsLow() && color.equals(white)) {
                enabled = true;
            } else if (sdc.getSymsIdsMid() && color.equals(yellow)) {
                enabled = true;
            } else if (sdc.getSymsIdsHigh() && color.equals(red)) {
                enabled = true;
            }
        }

        return enabled;
    }

    /**
     * Draw the Cell specific hexagons on the bundle
     * 
     * @param ctdr
     * @param aTarget
     * @param paintProps
     * @return boolean
     */
    public void drawHexagon(CellTableDataRow ctdr, MapDescriptor descriptor,
            IGraphicsTarget aTarget) throws VizException {

        this.descriptor = descriptor;
        this.aTarget = aTarget;
        this.centerPoint = factory.createPoint(new Coordinate(ctdr.getLon(),
                ctdr.getLat(), 0.0));

        double size = getSize(ctdr) * 2;

        // go ahead and draw
        center = descriptor.worldToPixel(new double[] {
                centerPoint.getCoordinate().x, centerPoint.getCoordinate().y });

        vertexes = getHexagonVertices(size);
        if (isHexagonEnabled()) {
            if (size > 0.0) {
                DrawableCircle circle = new DrawableCircle();
                circle.numberOfPoints = 6;
                circle.setCoordinates(center[0], center[1]);
                circle.radius = size * (SIN_HEX_ANGLE);
                circle.lineWidth = outlineWidth;
                circle.basics.color = color;

                if (isNew(ctdr)) {
                    DrawableCircle innerCircle = new DrawableCircle();
                    innerCircle.numberOfPoints = 6;
                    innerCircle.setCoordinates(center[0], center[1]);
                    innerCircle.radius = size / 2 * (SIN_HEX_ANGLE);
                    innerCircle.lineWidth = outlineWidth;
                    innerCircle.basics.color = color;
                    aTarget.drawCircle(circle, innerCircle);
                } else {
                    aTarget.drawCircle(circle);
                }
            }
        }

        if (isIDEnabled() && (size > 0.0)) {
            drawHexagonLabel(ctdr.getIdent());
        }

        if (isArrowEnabled() && (size > 0.0)) {
            drawArrow(ctdr.getDir(), ctdr.getSpd());
        }

        drawCellTrack(ctdr);
    }

    /**
     * Find the intersection of the arrow with the hexagonal cell itself.
     * 
     * @return intersection
     */
    private double[] findIntersectionWithHexagon(final double centerPtX,
            final double centerPtY, final double endPtX, final double endPtY) {
        double[] intersection = { centerPtX, centerPtY };
        double Bx, By, Cx, Cy, Dx, Dy;
        double distArrow, theCos, theSin, newX, arrowPos;

        for (int i = 0; vertexes.size() - 1 > i; i++) {
            double[] point = vertexes.get(i);
            double[] point2 = vertexes.get(i + 1);

            // (1) Translate the system so that center point 0 is on the origin.
            Bx = endPtX - centerPtX;
            By = endPtY - centerPtY;
            Cx = point[0] - centerPtX;
            Cy = point[1] - centerPtY;
            Dx = point2[0] - centerPtX;
            Dy = point2[1] - centerPtY;

            // Discover the length of the arrow.
            distArrow = Math.sqrt(Bx * Bx + By * By);

            // (2) Rotate the system so that the point B is on the positive X
            // axis.
            theCos = Bx / distArrow;
            theSin = By / distArrow;
            newX = Cx * theCos + Cy * theSin;
            Cy = Cy * theCos - Cx * theSin;
            Cx = newX;
            newX = Dx * theCos + Dy * theSin;
            Dy = Dy * theCos - Dx * theSin;
            Dx = newX;

            // Continue if hexagon segment doesn't cross the arrow.
            if (((Cy < 0.) && (Dy < 0.)) || ((Cy >= 0.) && (Dy >= 0.))) {
                continue;
            }

            // (3) Discover the position of the intersection point along the
            // arrow.

            arrowPos = Dx + (Cx - Dx) * Dy / (Dy - Cy);

            // (4) Apply the discovered position to the arrow in the original
            // coordinate system.

            intersection[0] = centerPtX + arrowPos * theCos;
            intersection[1] = centerPtY + arrowPos * theSin;

            // (5) Check that discovered position is nearer to the arrowhead
            // than an intersection on the opposite side of the hexagon.

            if ((Math.sqrt((Bx - arrowPos * theCos) * (Bx - arrowPos * theCos)
                    + (By - arrowPos * theSin) * (By - arrowPos * theSin))) > distArrow) {
                continue;
            }
            break;
        }

        return intersection;
    }

    /**
     * Draw the DMD circles on the bundle
     * 
     * @param dtdr
     * @param aTarget
     * @param paintProps
     * @return boolean
     */
    public void drawDMD(DMDTableDataRow dtdr, MapDescriptor descriptor,
            IGraphicsTarget aTarget) throws VizException {

        this.descriptor = descriptor;
        this.aTarget = aTarget;
        this.centerPoint = factory.createPoint(new Coordinate(dtdr.getLon(),
                dtdr.getLat(), 0.0));
        double size = getSize(dtdr);

        if ((size > 0) && !dtdr.getRank().equals("NONE")) {
            // go ahead and draw
            center = descriptor.worldToPixel(new double[] {
                    centerPoint.getCoordinate().x,
                    centerPoint.getCoordinate().y });
            // Set pixel coverage values
            wRightX = center[0] + (size);
            wLeftX = center[0] - (size);
            topY = center[1] - (size);
            bottomY = center[1] + (size);
            
            DrawableCircle circle = new DrawableCircle();
            circle.setCoordinates(center[0], center[1]);
            circle.radius = size;
            circle.basics.color = getResourceColor();
            
            if (mesoType == 0) {
                if (isExt) {
                    // draw a broken circle
                    circle.lineWidth = outlineWidth * 4;
                    circle.lineStyle = LineStyle.DASHED;
                    aTarget.drawCircle(circle);          
                } else {  
                    circle.lineWidth = outlineWidth * 4;
                    aTarget.drawCircle(circle);
                }
            } else if (mesoType == 2) {
                if (isExt) {
                    // draw a broken circle
                    circle.lineWidth = outlineWidth;
                    circle.lineStyle = LineStyle.DASHED;
                    aTarget.drawCircle(circle);   
                } else {
                    circle.lineWidth = outlineWidth;
                    aTarget.drawCircle(circle);
                }
            } else if (mesoType == 1) {
                if (isExt) {
                    // draw a broken circle                  
                    circle.lineWidth = outlineWidth * 4;
                    circle.lineStyle = LineStyle.DASHED;
                    aTarget.drawCircle(circle);
                } else {
                    circle.lineWidth = outlineWidth * 4;
                    aTarget.drawCircle(circle);
                }
                // top spike
                aTarget.drawLine(center[0], topY, 0.0, center[0], topY - size,
                        0.0, getResourceColor(), outlineWidth * 4);
                // bottom spike
                aTarget.drawLine(center[0], bottomY, 0.0, center[0], bottomY
                        + size, 0.0, getResourceColor(), outlineWidth * 4);
                // right spike
                aTarget.drawLine(wRightX, center[1], 0.0, wRightX + size,
                        center[1], 0.0, getResourceColor(), outlineWidth * 4);
                // left spike
                aTarget.drawLine(wLeftX, center[1], 0.0, wLeftX - size,
                        center[1], 0.0, getResourceColor(), outlineWidth * 4);
            }

            double zoomLevel = this.descriptor.getRenderableDisplay().getZoom();
            int mapWidth = this.descriptor.getMapWidth() / 1000;
            double km = zoomLevel * mapWidth;

            DrawableString ds = new DrawableString(dtdr.getIdent(),
                    getResourceColor());
            ds.horizontalAlignment = HorizontalAlignment.RIGHT;
            ds.verticallAlignment = VerticalAlignment.BOTTOM;
            ds.font = font;
            ds.textStyle = TextStyle.DROP_SHADOW;

            if (km < 50) {
                ds.setCoordinates(center[0] - 1, center[1] - 1);
                aTarget.drawStrings(ds);
            } else {
                ds.setCoordinates(wLeftX, center[1] - size);
                aTarget.drawStrings(ds);
            }

            if (ddfc.isTrack()) {
                // draws the center point/circle
                if ((dtdr.getPastLat() != null) && (dtdr.getPastLon() != null)) {
                    drawFilledCircle(center, getResourceColor());
                }
                drawDMDTrack(dtdr);
            }
        }

        // reset the Ext flag
        isExt = false;
    }

    /**
     * Draw the arrows for dir and spd
     * 
     * @param dir
     * @param speed
     * @param temp
     * @throws VizException
     */
    public void drawArrow(double dir, double speed) throws VizException {

        double[] intersect;
        double totalLength = 0.0;

        // translate degrees to +-180
        dir = correctWindDirection(dir);

        if (speed > 0.0) {
            totalLength = speed / sdc.getArrowConversion();

            double[] end = getPixelRelativeCoordinate(centerPoint, totalLength,
                    dir);
            double[] labelEnd = getPixelRelativeCoordinate(centerPoint,
                    totalLength + 1, dir);

            double[] endWorld = descriptor.pixelToWorld(new double[] { end[0],
                    end[1] });
            Point endPoint = factory.createPoint(new Coordinate(endWorld[0],
                    endWorld[1], 0.0));
            List<double[]> ears = getDogEarCoordinate(centerPoint, endPoint,
                    dir, totalLength / 1.25);

            // draw it
            if (sdc.getArrowMode()) {
                aTarget.drawLine(center[0], center[1], 0, end[0], end[1], 0,
                        getColor(), outlineWidth);
            } else {
                // Find the intersection to use instead of the center point.
                Point intersectPoint = null;
                if (totalLength > hexRadius) {
                    intersect = findIntersectionWithHexagon(center[0],
                            center[1], end[0], end[1]);
                    double[] intersectVals = descriptor
                            .pixelToWorld(new double[] { intersect[0],
                                    intersect[1] });
                    intersectPoint = factory.createPoint(new Coordinate(
                            intersectVals[0], intersectVals[1], 0.0));
                    end = getPixelRelativeCoordinate(intersectPoint,
                            totalLength, dir);
                } else {
                    end = getPixelRelativeCoordinate(centerPoint,
                            hexRadius + 2, dir);
                    intersect = findIntersectionWithHexagon(center[0],
                            center[1], end[0], end[1]);
                    double[] intersectVals = descriptor
                            .pixelToWorld(new double[] { intersect[0],
                                    intersect[1] });
                    intersectPoint = factory.createPoint(new Coordinate(
                            intersectVals[0], intersectVals[1], 0.0));
                    end = getPixelRelativeCoordinate(intersectPoint,
                            totalLength, dir);
                }
                endWorld = descriptor.pixelToWorld(new double[] { end[0],
                        end[1] });
                endPoint = factory.createPoint(new Coordinate(endWorld[0],
                        endWorld[1], 0.0));
                ears = getDogEarCoordinate(intersectPoint, endPoint, dir,
                        totalLength / 1.25);
                labelEnd = getPixelRelativeCoordinate(intersectPoint,
                        totalLength + 1, dir);

                aTarget.drawLine(intersect[0], intersect[1], 0, end[0], end[1],
                        0, getColor(), outlineWidth);
                aTarget.drawLine(center[0], center[1], 0, intersect[0],
                        intersect[1], 0, getColor(), outlineWidth);
            }
            for (double[] ear : ears) {
                aTarget.drawLine(end[0], end[1], 0, ear[0], ear[1], 0,
                        getColor(), outlineWidth);
            }

            drawArrowLabel(labelEnd[0], labelEnd[1],
                    new Integer((int) speed).toString());
        }
    }

    /**
     * Draw the ident of the cell
     * 
     * @param labelCenter
     * @param label
     * @param size
     * @param aTarget
     * @throws VizException
     */
    public void drawHexagonLabel(String label) throws VizException {
        double zoomLevel = this.descriptor.getRenderableDisplay().getZoom();
        int mapWidth = this.descriptor.getMapWidth() / 1000;
        double km = zoomLevel * mapWidth;

        if (km < 50) {
            aTarget.drawString(font, label, center[0] - 1, center[1] - 1, 0,
                    TextStyle.DROP_SHADOW, getColor(),
                    HorizontalAlignment.RIGHT, VerticalAlignment.BOTTOM, 0.0);
        } else {
            aTarget.drawString(font, label, wLeftX, topY, 0,
                    TextStyle.DROP_SHADOW, getColor(),
                    HorizontalAlignment.RIGHT, VerticalAlignment.BOTTOM, 0.0);
        }
    }

    /**
     * Draw the speed of movement for the cell at the end of the arrow.
     * 
     * @param arrowEndPoint
     * @param label
     * @param size
     * @param aTarget
     * @param paintProps
     * @throws VizException
     */
    public void drawArrowLabel(double x, double y, String label)
            throws VizException {

        aTarget.drawString(font, label, x, y, 0, TextStyle.DROP_SHADOW,
                getColor(), HorizontalAlignment.CENTER,
                VerticalAlignment.MIDDLE, 0.0);
    }

    /**
     * Gets the size and ostensibly the color of the feature.
     * 
     * @param CelltableDataRow
     * @return
     */
    private double getSize(ScanTableDataRow stdr) {
        double value = 0.0;
        double size = 0.0;
        if (stdr instanceof CellTableDataRow) {
            setCellColor(((CellTableDataRow) stdr).getValue(sdc.getAttrName()));

            value = getCellRadius(((CellTableDataRow) stdr).getValue(sdc
                    .getRadVar()));

        } else if (stdr instanceof DMDTableDataRow) {
            value = getDMDDrawingAttributes((DMDTableDataRow) stdr);
        } else if (stdr instanceof TVSTableDataRow) {
            value = getTVSSize((TVSTableDataRow) stdr);
        }

        if (value > 0.0) {
            size = getPixelRelativeLength(centerPoint, value + 1);
        }

        return size;
    }

    /**
     * Sets the color drawing params
     * 
     * @param value
     */
    private void setCellColor(double value) {
        color = null;
        if ((value >= sdc.getLowerVal()) && (value < sdc.getMidVal())) {
            color = white;
        } else if ((value >= sdc.getMidVal()) && (value < sdc.getUpperVal())) {
            color = yellow;
        } else if (value >= sdc.getUpperVal()) {
            color = red;
        }
    }

    /**
     * Gets the Color
     * 
     * @return
     */
    private RGB getColor() {
        return color;
    }

    /**
     * Gets the vertices of the hexagons
     * 
     * @param center
     * @return
     */
    private List<double[]> getHexagonVertices(double size) {

        side = size / outerSideFactor * (SIN_HEX_ANGLE);
        topY = center[1] - (size / outerTopBottomFactor);
        bottomY = center[1] + (size / outerTopBottomFactor);
        rightX = center[0] - (side);
        leftX = center[0] + (side);
        wRightX = center[0] - 2 * side;
        wLeftX = center[0] + 2 * side;

        List<double[]> vertexes = new ArrayList<double[]>();
        vertexes.add(new double[] { rightX, bottomY });
        vertexes.add(new double[] { wRightX, center[1] });
        vertexes.add(new double[] { rightX, topY });
        vertexes.add(new double[] { leftX, topY });
        vertexes.add(new double[] { wLeftX, center[1] });
        vertexes.add(new double[] { leftX, bottomY });
        // add first to the end, this way the hexagon will complete
        vertexes.add(new double[] { rightX, bottomY });

        return vertexes;
    }

    /**
     * sets the screen to World ratio;
     * 
     * @param screenToWorldRatio
     */
    public void setScreenToWorldRatio(double screenToWorldRatio) {
        this.screenToWorldRatio = screenToWorldRatio;
    }

    /**
     * Sets the font. The font must be disposed by class using this drawer
     * 
     * @param font
     */
    public void setFont(IFont font) {
        this.font = font;
    }

    /**
     * Get the font
     * 
     * @return font
     */
    public IFont getFont() {
        return font;
    }

    /**
     * Sets the Cell drawing config up
     * 
     * @param sdc
     */
    public void setCellDisplayConfig(StormCellConfig sdc) {
        this.sdc = sdc;
    }

    /**
     * Sets the DMD drawing config up
     * 
     * @param sdc
     */
    public void setDmdDisplayConfig(DmdDisplayFilterConfig ddfc) {
        this.ddfc = ddfc;
    }

    /**
     * Gets the scaled radius of the hexagon and sets the color
     * 
     * @param circelSize
     * @return
     */
    private double getCellRadius(double value) {
        hexRadius = 0.0;

        // Determine the radius of the storm cell.
        int minRadius = sdc.getMinRadius();
        int maxRadius = sdc.getMaxRadius();
        double iRadLow = sdc.getRadLow();
        double iRadHigh = sdc.getRadHigh();

        if (minRadius == maxRadius) {
            hexRadius = minRadius;
        } else if (value < iRadLow) {
            hexRadius = minRadius;
        } else if (value > iRadHigh) {
            hexRadius = maxRadius;
        } else {
            hexRadius = minRadius + (maxRadius - minRadius) * (value - iRadLow)
                    / (iRadHigh - iRadLow);
        }

        return hexRadius;
    }

    /**
     * Gets the color
     * 
     * @param value
     * @return
     */
    private double getDMDDrawingAttributes(DMDTableDataRow stdr) {
        double sizeval = 0.0;
        if (!stdr.getRank().equals("N/A")
                && (new Double(stdr.getRank()) >= ddfc.getLowerVal())) {
            sizeval = 2.0;
            if (new Double(stdr.getRank()) >= ddfc.getUpperVal()) {
                if (stdr.getLowestElev(stdr.getElev0()).equals("Y")
                        || (stdr.getBase() <= 1)) {
                    mesoType = 1; // with spikes, thick circle
                } else {
                    mesoType = 0; // sans spikes, thick circle
                }
            } else if (new Double(stdr.getRank()) >= ddfc.getMidVal()) {
                mesoType = 2; // thin circle
            } else if (new Double(stdr.getRank()) >= ddfc.getLowerVal()) {
                mesoType = 2; // thin circle
            } else {
                mesoType = 2; // no circle
                sizeval = 0.0;
                color = null;
            }

            if (stdr.getStatus().equals("EXT")) {
                isExt = true; // broken circle
            }
        }
        // use low level diameter as measure for drawing it
        if ((stdr.getLlDiam() > 0.0) && (sizeval > 0.0)) {
            sizeval = ((stdr.getLlDiam() * 1000.0) * ScanUtils.meterToNM);
            if (sizeval < 1.0) {
                sizeval = 1.0;
            }
        }

        return sizeval;
    }

    /**
     * Determine size of TVS to draw
     * 
     * @param stdr
     * @return
     */
    private double getTVSSize(TVSTableDataRow stdr) {
        double sizeval = 2.0;

        return sizeval;
    }

    /**
     * For some calculations we need the pixel relative size to pass the
     * drawCircle for instance. This is a convenience method for calculating
     * this.
     * 
     * @param nauticalMiles
     * @return
     */
    public double getPixelRelativeLength(Point point, double nauticalMiles) {
        if (gc != null) {
            gc.setStartingGeographicPoint(point.getCoordinate().x,
                    point.getCoordinate().y);
            gc.setDirection(90, ScanUtils.NMI_TO_KM * nauticalMiles * 1000);

            double[] p1 = descriptor.worldToPixel(new double[] {
                    gc.getStartingGeographicPoint().getX(),
                    gc.getStartingGeographicPoint().getY() });
            double[] p2 = descriptor.worldToPixel(new double[] {
                    gc.getDestinationGeographicPoint().getX(),
                    gc.getDestinationGeographicPoint().getY() });
            return Math.abs(p1[0] - p2[0]);
        }
        return 0;
    }

    /**
     * Gets the endpoint
     * 
     * @param nauticalMiles
     * @param dir
     * @return
     */
    public double[] getPixelRelativeCoordinate(Point point,
            double nauticalMiles, double dir) {
        gc.setStartingGeographicPoint(point.getCoordinate().x,
                point.getCoordinate().y);
        gc.setDirection(dir, ScanUtils.NMI_TO_KM * nauticalMiles * 1000);

        return descriptor.worldToPixel(new double[] {
                gc.getDestinationGeographicPoint().getX(),
                gc.getDestinationGeographicPoint().getY() });
    }

    /**
     * Gets the coordinates for the arrow dog ears
     * 
     * @param endPoint
     * @param dir
     * @return
     */
    private List<double[]> getDogEarCoordinate(Point refPoint, Point endPoint,
            double dir, double length) {

        double[] earAngles = new double[] { 10, -10 };
        List<double[]> coords = new ArrayList<double[]>(earAngles.length);

        for (double earAngle : earAngles) {

            double calcDir = 0.0;

            if (dir > 0) {
                if ((dir + earAngle) > 180.0) {
                    // offset should be positive
                    double offset = (dir + earAngle) - 180.0;
                    calcDir = -180.0 + offset;
                } else {
                    calcDir = dir + earAngle;
                }
            }
            if (dir < 0) {
                if ((dir - earAngle) < -180.0) {
                    // offset should be negative
                    double offset = (dir - earAngle) + 180.0;
                    calcDir = 180.0 + offset;
                } else {
                    calcDir = dir - earAngle;
                }
            }
            coords.add(getPixelRelativeCoordinate(refPoint, length, calcDir));
        }

        return coords;
    }

    /**
     * Sets the outline width
     * 
     * @param outlineWidth
     */
    public void setOutlineWidth(int outlineWidth) {
        this.outlineWidth = outlineWidth;

    }

    /**
     * Check newness
     * 
     * @param stdr
     * @return
     */
    private boolean isNew(ScanTableDataRow stdr) {
        if (stdr.getIsNew()) {
            return true;
        }
        return false;
    }

    /**
     * gets the pixel coverage for this Cell drawable
     * 
     * @return
     */
    public PixelCoverage getPixelCoverage() {

        Coordinate ul = new Coordinate(wLeftX, topY);
        Coordinate ur = new Coordinate(wRightX, topY);
        Coordinate lr = new Coordinate(wRightX, bottomY);
        Coordinate ll = new Coordinate(wLeftX, bottomY);

        return new PixelCoverage(ul, ur, lr, ll);
    }

    /**
     * Draw future DMD track
     * 
     * @param dtdr
     * @throws VizException
     */
    public void drawDMDTrack(DMDTableDataRow dtdr) throws VizException {

        if ((dtdr.getFcstLat() != null) && (dtdr.getFcstLon() != null)) {

            double[] futurePoint = null;
            double[] pastPoint = null;
            int count = Math.min(dtdr.getFcstLon().size(), dtdr.getFcstLat()
                    .size());

            for (int i = 0; i < count; i++) {
                futurePoint = descriptor.worldToPixel(new double[] {
                        dtdr.getFcstLon().get(i), dtdr.getFcstLat().get(i) });

                if (pastPoint == null) {
                    pastPoint = center;
                }

                aTarget.drawLine(pastPoint[0], pastPoint[1], 0.0,
                        futurePoint[0], futurePoint[1], 0.0,
                        getResourceColor(), outlineWidth);
                drawPlus(futurePoint, getResourceColor());
                pastPoint = futurePoint;
            }
        }

        if ((dtdr.getPastLat() != null) && (dtdr.getPastLon() != null)) {

            double[] futurePoint = null;
            double[] pastPoint = null;
            int count = Math.min(dtdr.getPastLon().size(), dtdr.getPastLat()
                    .size());

            for (int i = 0; i < count; i++) {
                try {
                    futurePoint = descriptor
                            .worldToPixel(new double[] {
                                    dtdr.getPastLon().get(i),
                                    dtdr.getPastLat().get(i) });

                    if (pastPoint == null) {
                        pastPoint = center;
                    }

                    aTarget.drawLine(pastPoint[0], pastPoint[1], 0.0,
                            futurePoint[0], futurePoint[1], 0.0,
                            getResourceColor(), outlineWidth);
                    drawFilledCircle(futurePoint, getResourceColor());
                    pastPoint = futurePoint;
                } catch (Exception e) {
                    // suppress exception here, bad data.
                }
            }
        }
    }

    public void drawCellTrack(CellTableDataRow ctdr) throws VizException {
        List<double[]> futurePoints = new ArrayList<double[]>();
        List<double[]> pastPoints = new ArrayList<double[]>();
        List<Date> dates = new ArrayList<Date>();

        int count = 0;

        if (sdc.getPastTracks()) {
            if (ctdr.getPastCoordinates() != null) {
                for (Date date : ctdr.getPastCoordinates().keySet()) {
                    dates.add(date);
                }
                Collections.sort(dates);
                count = dates.size();

                double[] point = null;
                for (int i = dates.size() - 1; i >= 0; i--) {
                    Coordinate coor = ctdr.getPastCoordinates().get(
                            dates.get(i));
                    point = descriptor.worldToPixel(new double[] { coor.x,
                            coor.y });
                    pastPoints.add(point);
                }
            }
        }

        if (sdc.getFutureTracks()) {
            if ((dates.size() > 0) && (count >= 1)) {
                if (count >= 6) {
                    count = 6;
                    if (ctdr.getVcp() == 21) {
                        count = 5;
                    }
                }
                int oneScanTimeIntvl = getVcpDuration(ctdr.getVcp());
                oneScanTimeIntvl /= 60;
                double dir = ctdr.getDir();
                double spd = ctdr.getSpd();

                // create future points
                for (int i = 0; i < count; i++) {
                    int dt = oneScanTimeIntvl * (i + 1);

                    double[] futurePt;
                    double dist = getDistance(dt, spd);
                    futurePt = getPixelRelativeCoordinate(centerPoint, dist,
                            correctWindDirection(dir));
                    futurePoints.add(futurePt);
                }
            }
        }

        // draw the past points
        if (pastPoints.size() > 0) {
            aTarget.drawPoints(pastPoints, getResourceColor(), PointStyle.DISC,
                    0.7f);
        }

        // draw the X
        aTarget.drawPoint(center[0], center[1], 0.0, getResourceColor(),
                PointStyle.X, 1.3f);

        // draw the future points
        if (futurePoints.size() > 0) {
            aTarget.drawPoints(futurePoints, getResourceColor(),
                    PointStyle.CROSS, 1.3f);
        }

        // draw the track line
        DrawableLine line = new DrawableLine();
        for (int i = pastPoints.size() - 1; i > -1; i--) {
            double[] pt = pastPoints.get(i);
            line.addPoint(pt[0], pt[1]);
        }
        line.addPoint(center[0], center[1]);
        for (double[] pt : futurePoints) {
            line.addPoint(pt[0], pt[1]);
        }

        if (line.points.size() > 1) {
            line.width = outlineWidth;
            line.basics.color = getResourceColor();
            aTarget.drawLine(line);
        }
    }

    /**
     * draws the plus sign
     * 
     * @param point
     * @throws VizException
     */
    public void drawPlus(double[] point, RGB color) throws VizException {
        // bottom to top
        aTarget.drawLine(point[0], (point[1] - 4.0 / screenToWorldRatio), 0.0,
                point[0], (point[1] + 4.0 / screenToWorldRatio), 0.0, color,
                outlineWidth);
        // left to right
        aTarget.drawLine((point[0] - 4.0 / screenToWorldRatio), point[1], 0.0,
                (point[0] + 4.0 / screenToWorldRatio), point[1], 0.0, color,
                outlineWidth);

    }

    /**
     * draws the X sign
     * 
     * @param point
     * @throws VizException
     */
    public void drawX(double[] point) throws VizException {
        // uppe left to lower right
        aTarget.drawLine((point[0] - 4.0 / screenToWorldRatio),
                (point[1] + 4.0 / screenToWorldRatio), 0.0,
                (point[0] + 4.0 / screenToWorldRatio),
                (point[1] - 4.0 / screenToWorldRatio), 0.0, getResourceColor(),
                outlineWidth);
        // lower left to upper right
        aTarget.drawLine((point[0] - 4.0 / screenToWorldRatio),
                (point[1] - 4.0 / screenToWorldRatio), 0.0,
                (point[0] + 4.0 / screenToWorldRatio),
                (point[1] + 4.0 / screenToWorldRatio), 0.0, getResourceColor(),
                outlineWidth);
    }

    /**
     * Draws the circle
     * 
     * @param point
     * @throws VizException
     */
    public void drawFilledCircle(double[] point, RGB color) throws VizException {
        DrawableCircle circle = new DrawableCircle();
        circle.setCoordinates(point[0], point[1], 0);
        circle.basics.color = color;
        circle.radius = 2.0 / screenToWorldRatio;
        circle.filled = true;
        aTarget.drawCircle(circle);
    }

    /**
     * resource color set
     * 
     * @param rscColor
     */
    public void setResourceColor(RGB rscColor) {
        this.rscColor = rscColor;
    }

    public RGB getResourceColor() {
        return rscColor;
    }

    /**
     * Get the distance extrapolation, this assumes speed is in NMI per hour
     * 
     * @param mins
     * @param ctdr
     * @return
     */
    private double getDistance(double mins, double speed) {

        return (mins / 60.0) * speed;
    }

    /** correct wind direction going it wrong direction **/
    private double correctWindDirection(double windDir) {
        // translate degrees to +-180
        if (windDir > 180.0) {
            windDir = windDir * (-1); // switch it to negative
            windDir = -180.0 - windDir;
        } else {
            windDir = (180 - windDir) * -1;// switch it to positive
        }

        return windDir;
    }

    /**
     * Get the duration of the VCP.
     * 
     * @param vcp
     *            The VCP number
     * @return the VCP duration, or 10 if not available
     */
    private int getVcpDuration(Integer vcp) {
        ScanRunConfigMgr configMgr = ScanRunConfigMgr.getInstance();
        ArrayList<ScanVcpXML> vcpList = configMgr.getScanVcp();

        for (ScanVcpXML item : vcpList) {
            if (item.getNumber() == vcp) {
                return item.getDuration();
            }
        }

        return 10;
    }

    /**
     * Sort by Date
     * 
     * @author dhladky
     * 
     */
    public class SortByDate implements Comparator<Date> {

        @Override
        public int compare(Date o1, Date o2) {

            return o1.compareTo(o2);
        }
    }

}
