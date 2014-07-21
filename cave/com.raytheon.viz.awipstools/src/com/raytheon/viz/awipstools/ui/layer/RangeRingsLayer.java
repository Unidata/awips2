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
import java.util.Collection;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
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
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Layer for displaying a radar's range rings.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  10-21-09     #717       bsteffen    Refactor to common MovableTool model
 *  15Mar2013	15693	mgamazaychikov	Added magnification capability.
 *  07-21-14    #3412       mapeters    Updated deprecated drawCircle call.
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class RangeRingsLayer extends AbstractMovableToolLayer<RangeRing>
        implements IContextMenuContributor, IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RangeRingsLayer.class);

    private IFont labelFont;

    protected static final UnitConverter NM_TO_METERS = NonSI.NAUTICAL_MILE
            .getConverterTo(SI.METER);

    public static final String DEFAULT_NAME = "Range Rings";

    private static final int NUM_VERTICES = 50;

    private static final double ANGLE_STEP = 360.0 / NUM_VERTICES;

    private static final int LABEL_INDEX = (NUM_VERTICES / 8) * 5;

    private AbstractRightClickAction moveElementAction;

    private ToolsDataManager dataManager = ToolsDataManager.getInstance();

    private static GeometryFactory gf = new GeometryFactory();

    private static GeodeticCalculator gc = new GeodeticCalculator();

    private static RangeRingDialog ringDialog = null;

    public RangeRingsLayer(
            GenericToolsResourceData<RangeRingsLayer> resourceData,
            LoadProperties loadProperties) throws VizException {
        super(resourceData, loadProperties);
        getCapabilities().addCapability(new OutlineCapability());
        // add magnification capability
        getCapabilities().addCapability(new MagnificationCapability());
        moveElementAction = new AbstractRightClickAction() {
            public void run() {
                makeSelectedLive();
            }
        };
        moveElementAction.setText("Move Entire Element");
        // displayDialog();
        resourceData.addChangeListener(this);
    }

    @Override
    protected void disposeInternal() {
        resourceData.fireChangeListeners(ChangeType.DATA_UPDATE, false);
        super.disposeInternal();
        resourceData.removeChangeListener(this);
    }

    @Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		super.initInternal(target);
		resourceData.addChangeListener(this);
		resetRings();
		displayDialog();
		// initialize font for magnification capability
		labelFont = target.initializeFont(
				target.getDefaultFont().getFontName(), 12.0f,
				new Style[] { Style.BOLD });
	}

    private void resetRings() {
        setObjects(dataManager.getRangeRings());
    }

    public void displayDialog() throws VizException {

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
                        ringDialog.open();
                        resourceData.addChangeListener(ringDialog);
                    }

                } catch (VizException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

        });
    }

    protected void paint(IGraphicsTarget target, PaintProperties paintProps,
            RangeRing ring, SelectionStatus status) throws VizException {
    	// set font for  magnification capability
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
            IWireframeShape shape = target.createWireframeShape(false,
                    descriptor);
            int[] radii = ring.getRadii();
            for (int i = 0; i < radii.length; i++) {
                int radius = radii[i];
                if (radius != 0) {
                    Coordinate[] coords = getRing(center, radius);
                    shape.addLineSegment(coords);
                    if (label.contains(String.valueOf(i + 1))) {
                        double[] labelLoc = descriptor
                                .worldToPixel(new double[] {
                                        coords[LABEL_INDEX].x,
                                        coords[LABEL_INDEX].y });
                        target.drawString(labelFont, radius + " nm", labelLoc[0],
                                labelLoc[1], 0.0, TextStyle.NORMAL, color,
                                HorizontalAlignment.CENTER,
                                VerticalAlignment.TOP, null);
                    }
                }
            }
            target.drawWireframeShape(shape, color, lineWidth, lineStyle);
            shape.dispose();
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
                target.drawString(labelFont, ring.getId(), labelLoc[0], labelLoc[1],
                        0.0, TextStyle.NORMAL, color, HorizontalAlignment.LEFT,
                        null);
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
                try {
                    displayDialog();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
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

    protected RangeRing makeLive(RangeRing object) {
        return object.clone();
    }

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
