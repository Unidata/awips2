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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.viz.core.DrawableBasics;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.tools.AbstractMovableToolLayer;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;

/**
 * Implements the distance bearing functionality.
 * 
 * Note: Implementing slightly different from WSE (D2D). In D2D, allows context
 * sensitive popup menu allowing moving of point or line. In currently
 * implementation in CAVE only allow delete of line through "rightclick" menu.
 * 
 * Move of point or line is accomplished by normal "left mouse button hold, and
 * drag".
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Sep272007    #455        ebabin      Initial Creation.
 *  26Oct2007    #504        ebabin      Update to use BaselineLoader class.  
 *  02Sept2008   #1516       dhladky     Updated Baseline code.
 *  10-21-09     #2490       bsteffen    Refactor to common MovableTool model
 *  06-08-10     #5620       bkowal      Whenever the user's cursor is positioned
 *                                       over an endpoint (selectedVertex != null),
 *                                       an indicator will be set that is used
 *                                       in the super class.
 *  06-09-10     #5620       bkowal      The tool will load in an editable state by
 *                                       default now.
 *  15Mar2013	15693	mgamazaychikov	 Added magnification capability.
 *  07-21-14    #3412        mapeters    Updated deprecated drawCircle call.
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public class DistanceBearingToolLayer extends
        AbstractMovableToolLayer<LineSegment> implements
        IContextMenuContributor {
	
	private IFont labelFont;

    public static final String DEFAULT_NAME = "Distance Bearing";

    private GeodeticCalculator gc;

    private AbstractRightClickAction deleteElementAction;

    private AbstractRightClickAction moveElementAction;

    private AbstractRightClickAction moveVertexAction;

    private Coordinate selectedVertex = null;

    public DistanceBearingToolLayer(
            GenericToolsResourceData<DistanceBearingToolLayer> resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, true);
        getCapabilities().addCapability(new OutlineCapability());
        // add magnification capability
        getCapabilities().addCapability(new MagnificationCapability());
        deleteElementAction = new AbstractRightClickAction() {
            public void run() {
                deleteSelected();
            }
        };
        deleteElementAction.setText("Delete Entire Element");
        moveElementAction = new AbstractRightClickAction() {
            public void run() {
                selectedVertex = null;
                makeSelectedLive();
            }
        };
        moveElementAction.setText("Move Entire Element");
        moveVertexAction = new AbstractRightClickAction() {
            public void run() {
                makeSelectedLive();
            }
        };
        moveVertexAction.setText("Move Vertex");
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        setObjects(ToolsDataManager.getInstance().getDistanceBearings());
        gc = new GeodeticCalculator(descriptor.getCRS());
        // initialize font for  magnification capability
        labelFont = target.initializeFont(
                target.getDefaultFont().getFontName(), 12.0f,
                new Style[] { Style.BOLD });
    }

    protected void paint(IGraphicsTarget target, PaintProperties paintProps,
            LineSegment line, SelectionStatus status) throws VizException {
        double radius = (MAGIC_CIRCLE_RADIUS * paintProps.getZoomLevel());
        RGB color = getCapability(ColorableCapability.class).getColor();
        float lineWidth = this.getCapability(OutlineCapability.class)
                .getOutlineWidth();
        LineStyle lineStyle = this.getCapability(OutlineCapability.class)
                .getLineStyle();
        if (status != SelectionStatus.NORMAL) {
            lineWidth = Math.max(1.5f, lineWidth);
            if (status == SelectionStatus.LIVE) {
                lineStyle = LineStyle.DASHED;
            } else {
                color = GRAY;
            }
        }
        IWireframeShape wireframeShape = target.createWireframeShape(false,
                descriptor);
        Coordinate[] ends = { line.p0, line.p1 };
        wireframeShape.addLineSegment(ends);
        if (isEditable()) {
            DrawableCircle circle0 = new DrawableCircle();
            DrawableCircle circle1 = new DrawableCircle();
            DrawableCircle[] circles = new DrawableCircle[] { circle0, circle1 };
            for (int i = 0; i <= 1; i++) {
                double[] center = descriptor.worldToPixel(new double[] {
                        ends[i].x, ends[i].y });
                circles[i].radius = radius;
                circles[i].basics.color = color;
                circles[i].setCoordinates(center[0], center[1]);
            }
            target.drawCircle(circles);
        }
        String label = computeRangeAndAzimuth(line);
        // set font for  magnification capability
        labelFont.setMagnification(getCapability(MagnificationCapability.class)
                .getMagnification().floatValue());
        double[] center = descriptor.worldToPixel(new double[] { line.p0.x,
                line.p0.y });
        double labelLoc[] = target.getPointOnCircle(center[0], center[1], 0.0,
                radius, 0);
        target.drawString(labelFont, label, labelLoc[0], labelLoc[1], 0.0,
                TextStyle.NORMAL, color, HorizontalAlignment.LEFT, null);

        target.drawWireframeShape(wireframeShape, color, lineWidth, lineStyle, labelFont);
        wireframeShape.dispose();

    }

    /**
     * Computes the range and azimuth, and location lat\long
     * 
     * @return A string
     */
    private String computeRangeAndAzimuth(LineSegment line) {

        String rangeAndAzimuth = "0mi@0";

        double azimuth = 000;

        gc.setStartingGeographicPoint(line.p1.x, line.p1.y);
        gc.setDestinationGeographicPoint(line.p0.x, line.p0.y);

        azimuth = gc.getAzimuth();

        if (azimuth < 0) {
            azimuth = 360 + azimuth;
        }

        UnitConverter conv = SI.METER.getConverterTo(NonSI.MILE);

        String s = String.valueOf((int) azimuth);
        if (s.length() == 2) {
            s = "0" + s;
        } else if (s.length() == 1) {
            s = "00" + s;
        }

        rangeAndAzimuth = (int) conv.convert(gc.getOrthodromicDistance())
                + "mi@" + s;

        return rangeAndAzimuth;
    }

    public String getDefaultName() {
        return DEFAULT_NAME;
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (isEditable() && selectedObject != null) {
            menuManager.add(deleteElementAction);
            menuManager.add(moveElementAction);
            if (selectedVertex != null) {
                menuManager.add(moveVertexAction);
            }
        }
    }

    protected boolean isClicked(IDisplayPaneContainer container,
            Coordinate clickPoint, LineSegment line) {
        double[] point0 = container.translateInverseClick(line.p0);
        double[] point1 = container.translateInverseClick(line.p1);
        LineSegment pointLine = new LineSegment(point0[0], point0[1],
                point1[0], point1[1]);
        if (pointLine.distance(clickPoint) <= MAGIC_CLICK_DISTANCE) {
            this.endpointClicked = true;

            if (clickPoint.distance(pointLine.p0) <= MAGIC_CLICK_DISTANCE) {
                selectedVertex = line.p0;
            } else if (clickPoint.distance(pointLine.p1) <= MAGIC_CLICK_DISTANCE) {
                selectedVertex = line.p1;
            } else {
                selectedVertex = null;
                this.endpointClicked = false;
            }

            return true;
        }
        return false;
    }

    protected LineSegment makeLive(LineSegment line) {
        return new LineSegment(new Coordinate(line.p0), new Coordinate(line.p1));
    }

    protected LineSegment move(Coordinate lastClickLoc, Coordinate clickLoc,
            LineSegment line) {

        if (selectedVertex != null) {
            if (line.p0.equals(selectedVertex)) {
                line.p0.x = clickLoc.x;
                line.p0.y = clickLoc.y;
                selectedVertex = line.p0;
            } else if (line.p1.equals(selectedVertex)) {
                line.p1.x = clickLoc.x;
                line.p1.y = clickLoc.y;
                selectedVertex = line.p1;
            }
            return line;
        } else {
            double[] lastClick = descriptor.worldToPixel(new double[] {
                    lastClickLoc.x, lastClickLoc.y });
            double[] newClick = descriptor.worldToPixel(new double[] {
                    clickLoc.x, clickLoc.y });
            double xDiff = lastClick[0] - newClick[0];
            double yDiff = lastClick[1] - newClick[1];
            double[] p0 = descriptor.worldToPixel(new double[] { line.p0.x,
                    line.p0.y });
            double[] p1 = descriptor.worldToPixel(new double[] { line.p1.x,
                    line.p1.y });
            p0[0] += xDiff;
            p0[1] += yDiff;
            p1[0] += xDiff;
            p1[1] += yDiff;
            p0 = descriptor.pixelToWorld(p0);
            p1 = descriptor.pixelToWorld(p1);
            line.p0 = new Coordinate(p0[0], p0[1]);
            line.p1 = new Coordinate(p1[0], p1[1]);
            return line;
        }
    }
}