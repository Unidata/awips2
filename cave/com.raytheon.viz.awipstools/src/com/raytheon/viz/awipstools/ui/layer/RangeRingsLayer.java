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

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.measure.UnitConverter;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.geotools.referencing.GeodeticCalculator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.tools.AbstractMovableToolLayer;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.RangeRing;
import com.raytheon.viz.awipstools.common.RangeRing.RangeRingType;
import com.raytheon.viz.awipstools.ui.dialog.RangeRingDialog;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.EditableManager;

import si.uom.SI;
import systems.uom.common.USCustomary;

/**
 * Layer for displaying a radar's range rings.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  10-21-09     #717       bsteffen    Refactor to common MovableTool model
 *  15Mar2013   15693       mgamazay    Added magnification capability.
 *  07-21-14    #3412       mapeters    Updated deprecated drawCircle call.
 *  07-29-14    #3465       mapeters    Updated deprecated drawString() calls.
 *  08-13-14    #3467       mapeters    ringDialog notifies this of changes.
 *  05-11-2015  #5070       randerso    Adjust font sizes for dpi scaling
 *  09-08-16    #5871       njensen     Don't recreate unchanged IWireframeShapes
 * 
 * </pre>
 * 
 * @author ebabin
 */

public class RangeRingsLayer extends AbstractMovableToolLayer<RangeRing>
        implements IContextMenuContributor {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RangeRingsLayer.class);

    protected static final UnitConverter NM_TO_METERS = USCustomary.NAUTICAL_MILE
            .getConverterTo(SI.METRE);

    public static final String DEFAULT_NAME = "Range Rings";

    private static final int NUM_VERTICES = 50;

    private static final double ANGLE_STEP = 360.0 / NUM_VERTICES;

    private static final int LABEL_INDEX = (NUM_VERTICES / 8) * 5;

    private static GeometryFactory gf = new GeometryFactory();

    private static GeodeticCalculator gc = new GeodeticCalculator();

    private static RangeRingDialog ringDialog = null;

    private ToolsDataManager dataManager = ToolsDataManager.getInstance();

    private AbstractRightClickAction moveElementAction;

    private IFont labelFont;

    private Map<RangeRing, IWireframeShape> shapeMap = new HashMap<>();

    public RangeRingsLayer(
            GenericToolsResourceData<RangeRingsLayer> resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getCapabilities().addCapability(new OutlineCapability());
        getCapabilities().addCapability(new MagnificationCapability());
        moveElementAction = new AbstractRightClickAction() {
            @Override
            public void run() {
                makeSelectedLive();
            }
        };
        moveElementAction.setText("Move Entire Element");
        resourceData.addChangeListener(this);
    }

    @Override
    protected void disposeInternal() {
        synchronized (shapeMap) {
            for (IWireframeShape shape : shapeMap.values()) {
                shape.dispose();
            }
            shapeMap.clear();
        }
        resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, false);
        super.disposeInternal();
        resourceData.removeChangeListener(this);
        ringDialog.removeChangeListenerFromResourceData(this);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        if (ringDialog != null) {
            ringDialog.addChangeListenerToResourceData(this);
        }
        resourceData.addChangeListener(this);
        resetRings();
        displayDialog();
        // initialize font for magnification capability
        labelFont = target.initializeFont(
                target.getDefaultFont().getFontName(), 10,
                new Style[] { Style.BOLD });
    }

    private void resetRings() {
        setObjects(dataManager.getRangeRings());
    }

    public void displayDialog() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                try {
                    Shell shell = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell();
                    if (ringDialog == null || ringDialog.getReturnCode() != 0) {
                        ringDialog = new RangeRingDialog(shell, resourceData);
                        ringDialog.setBlockOnOpen(false);
                        ringDialog.open();
                    } else {
                        resourceData.addChangeListener(ringDialog);
                        ringDialog.open();
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

        });
    }

    @Override
    protected void paint(IGraphicsTarget target, PaintProperties paintProps,
            RangeRing ring, SelectionStatus status) throws VizException {
        // set font for magnification capability
        labelFont.setMagnification(getCapability(MagnificationCapability.class)
                .getMagnification().floatValue());
        if (ring.isVisible()) {
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
            String label = ring.getLabel();
            Coordinate center = ring.getCenterCoordinate();

            boolean newShape = false;
            IWireframeShape shape = null;
            synchronized (shapeMap) {
                // cleanup old shapes
                Iterator<RangeRing> itr = shapeMap.keySet().iterator();
                while (itr.hasNext()) {
                    RangeRing rr = itr.next();
                    if (!objects.contains(rr)) {
                        IWireframeShape oldShape = shapeMap.get(rr);
                        oldShape.dispose();
                        itr.remove();
                    }
                }

                // find existing shape or create a new one
                shape = shapeMap.get(ring);
                if (shape == null) {
                    newShape = true;
                    shape = target.createWireframeShape(false, descriptor);
                    shapeMap.put(ring, shape);
                }
            }

            int[] radii = ring.getRadii();
            List<DrawableString> strings = new ArrayList<>();
            for (int i = 0; i < radii.length; i++) {
                int radius = radii[i];
                if (radius != 0) {
                    Coordinate[] coords = getRing(center, radius);
                    if (newShape) {
                        shape.addLineSegment(coords);
                    }
                    if (label.contains(String.valueOf(i + 1))) {
                        double[] labelLoc = descriptor
                                .worldToPixel(new double[] {
                                        coords[LABEL_INDEX].x,
                                        coords[LABEL_INDEX].y });
                        DrawableString string = new DrawableString(radius
                                + " nm", color);
                        string.font = labelFont;
                        string.setCoordinates(labelLoc[0], labelLoc[1]);
                        string.horizontalAlignment = HorizontalAlignment.CENTER;
                        string.verticallAlignment = VerticalAlignment.TOP;
                        strings.add(string);
                    }
                }
            }
            target.drawStrings(strings);
            target.drawWireframeShape(shape, color, lineWidth, lineStyle);
            double radius = (MAGIC_CIRCLE_RADIUS * paintProps.getZoomLevel());
            double[] centerPixel = descriptor.worldToPixel(new double[] {
                    center.x, center.y });
            DrawableCircle circle = new DrawableCircle();
            circle.setCoordinates(centerPixel[0], centerPixel[1]);
            circle.radius = radius;
            circle.basics.color = color;
            target.drawCircle(circle);

            if (label.contains("C")) {
                double labelLoc[] = target.getPointOnCircle(centerPixel[0],
                        centerPixel[1], 0.0, radius, 0);
                DrawableString string = new DrawableString(ring.getId(), color);
                string.font = labelFont;
                string.setCoordinates(labelLoc[0], labelLoc[1]);
                target.drawStrings(string);
            }
        }
    }

    protected Coordinate[] getRing(Coordinate center, double radiusInNm) {
        double radius = NM_TO_METERS.convert(radiusInNm);
        gc.setStartingGeographicPoint(center.x, center.y);
        Coordinate ring[] = new Coordinate[NUM_VERTICES + 1];
        for (int i = 0; i < NUM_VERTICES; i++) {
            double azimuth = -180.0 + i * ANGLE_STEP;
            gc.setDirection(azimuth, radius);
            Point2D p = gc.getDestinationGeographicPoint();
            ring[i] = new Coordinate(p.getX(), p.getY());
        }
        ring[NUM_VERTICES] = ring[0];
        return ring;
    }

    @Override
    public String getDefaultName() {
        return DEFAULT_NAME;
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (isEditable() && selectedObject != null) {
            menuManager.add(moveElementAction);
        }
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (object instanceof EditableCapability) {
            EditableCapability cap = (EditableCapability) object;
            if (cap.isEditable()) {
                displayDialog();
            }
        } else if (object instanceof Boolean) {
            if ((Boolean) object) {
                // need to tell the resource it is no longer editable
                EditableManager.makeEditable(this, false);
            }
        }
        resetRings();
        issueRefresh();
    }

    @Override
    protected boolean isClicked(IDisplayPaneContainer container,
            Coordinate mouseLoc, RangeRing ring) {

        if (ring.getType() == RangeRingType.FIXED) {
            return false;
        }
        double[] pixelLoc = container.translateInverseClick(ring
                .getCenterCoordinate());
        Point pixelPoint = gf.createPoint(new Coordinate(pixelLoc[0],
                pixelLoc[1]));
        Point clickPoint = gf.createPoint(mouseLoc);
        if (pixelPoint.isWithinDistance(clickPoint, MAGIC_CLICK_DISTANCE)) {
            return true;
        }
        return false;

    }

    @Override
    protected RangeRing makeLive(RangeRing object) {
        return object.clone();
    }

    @Override
    protected RangeRing move(Coordinate lastClickedCoordinate,
            Coordinate clickLoc, RangeRing ring) {
        ring.setCenterCoordinate(clickLoc);
        return ring;
    }

    @Override
    protected void save(RangeRing oldRing, RangeRing newRing) {
        Collection<RangeRing> rings = dataManager.getRangeRings();
        rings.remove(oldRing);
        rings.add(newRing);
        dataManager.setRangeRings(rings);
        resourceData.fireChangeListeners(ChangeType.DATA_UPDATE,
                new RangeRing[] { oldRing, newRing });
    }
}
