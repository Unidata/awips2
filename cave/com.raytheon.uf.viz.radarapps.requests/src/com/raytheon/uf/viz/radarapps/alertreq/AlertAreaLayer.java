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
package com.raytheon.uf.viz.radarapps.alertreq;

import java.util.BitSet;

import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.geometry.GeneralEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.uf.viz.radarapps.alertreq.impl.AlertRequestDoc;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

public class AlertAreaLayer extends
        AbstractVizResource<AlertAreaResourceData, MapDescriptor> implements
        IContextMenuContributor, IChangeListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertAreaLayer.class);

    private AlertRequestDoc alertRequest;

    private IWireframeShape gridWF;

    private IShadedShape gridSO;

    private float[] radarLocation;

    private MouseHandler mouseHandler;

    private GeneralGridGeometry gridGeometry;

    private MathTransform latLonToGrid;

    private RGB lastColor;

    public AlertAreaLayer(AlertAreaResourceData data, LoadProperties props) {
        super(data, props);
    }

    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        clearGeometry();
        clearShapes();
        super.setDescriptor(descriptor);
    }

    private GeneralGridGeometry getGridGeometry() {
        if (gridGeometry == null) {
            if (radarLocation != null) {
                ProjectedCRS radarProjCRS = MapUtil.constructStereographic(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        radarLocation[0], radarLocation[1]);

                GeneralEnvelope generalEnvelope = new GeneralEnvelope(2);
                generalEnvelope.setCoordinateReferenceSystem(radarProjCRS);

                // 58 x 58 cells of 16km x 16km
                /*
                 * GeneralGridGeometry y-coordinate is flipped by default? But
                 * changing any of the ranges results in an error?
                 */
                generalEnvelope
                        .setRange(0, -(58 / 2) * 16000, (58 / 2) * 16000);
                generalEnvelope
                        .setRange(1, -(58 / 2) * 16000, (58 / 2) * 16000);

                gridGeometry = new GeneralGridGeometry(new GeneralGridEnvelope(
                        new int[] { 0, 0 }, new int[] { 58, 58 }, false),
                        generalEnvelope);
            }
        }
        return gridGeometry;
    }

    private MathTransform getLatLonToGrid() {
        if (latLonToGrid == null) {
            try {
                GeneralGridGeometry gg = getGridGeometry();
                if (gg != null) {
                    latLonToGrid = TransformFactory.latLonToGrid(gg,
                            PixelInCell.CELL_CORNER);
                }
            } catch (TransformException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (FactoryException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return latLonToGrid;
    }

    public void setRPG(String radarID) {
        radarLocation = null;
        clearGeometry();

        if (radarID != null) {
            float[] loc = RadarApps.getRadarLocation(radarID);
            if (loc != null) {
                radarLocation = loc;
            }
        }

        redrawGrid();
    }

    public void setAlertRequest(AlertRequestDoc alertRequest) {
        /*
         * Want to use IObservable for box bits, but it seems that
         * AbstractVisResource.dispose() is not always called. This will
         * probably cause a memory leak.
         */

        if (this.alertRequest != null) {
            this.alertRequest.removeChangeListener(this);
        }
        this.alertRequest = alertRequest;
        if (this.alertRequest != null) {
            this.alertRequest.addChangeListener(this);
        }
        redrawGrid();
    }

    @Override
    protected void disposeInternal() {
        clearShapes();

        if (alertRequest != null) {
            alertRequest.removeChangeListener(this);
            alertRequest = null;
        }
        getResourceContainer().unregisterMouseHandler(mouseHandler);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        setEditable(false);
        mouseHandler = new MouseHandler();
        getResourceContainer().registerMouseHandler(mouseHandler);
    }

    public boolean isEditable() {
        return getCapability(EditableCapability.class).isEditable();
    }

    public void setEditable(boolean editable) {
        EditableManager.makeEditable(this, editable);
    }

    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (isEditable()) {
            menuManager.add(selectLocationAction);
            menuManager.add(selectAreaAction);
        }
    }

    private SelectAreaAction selectAreaAction = new SelectAreaAction();

    private SelectLocationAction selectLocationAction = new SelectLocationAction();

    class SelectAreaAction extends AbstractRightClickAction {
        public SelectAreaAction() {
            super("Select Area");
        }

        @Override
        public void run() {
            mouseHandler.startAreaSelection();
        }
    }

    class SelectLocationAction extends AbstractRightClickAction {
        public SelectLocationAction() {
            super("Select Location");
        }

        @Override
        public void run() {
            mouseHandler.selectLocation();
        }
    }

    private void buildShapes(IGraphicsTarget target, RGB color) {
        GeneralGridGeometry gg = getGridGeometry();

        if (gg != null) {
            gridSO = target.createShadedShape(false, this.descriptor, true);
            gridWF = target.createWireframeShape(false, this.descriptor);

            GeometryFactory geomFactory = new GeometryFactory();

            if (alertRequest != null) {
                // TransformFactory....?
                // MathTransform mt = gg.getGridToCRS(PixelInCell.CELL_CORNER);
                MathTransform mt;
                try {
                    mt = TransformFactory.gridToLatLon(gg,
                            PixelInCell.CELL_CORNER);
                } catch (FactoryException e) {
                    return;
                } catch (TransformException e) {
                    return;
                }

                assert mt.getSourceDimensions() == 2;
                assert mt.getSourceDimensions() == 2;
                double[] src = new double[4 * 2];
                double[] dst = new double[4 * 2];
                BitSet bb = alertRequest.getBoxBits();
                for (int y = 0; y < 58; ++y) {
                    for (int x = 0; x < 58; ++x) {
                        if (bb.get(y * 58 + x)) {
                            src[0] = src[6] = x;
                            src[1] = src[3] = y;
                            src[2] = src[4] = x + 1;
                            src[7] = src[5] = y + 1;
                            try {
                                mt.transform(src, 0, dst, 0, 4);
                            } catch (TransformException e) {
                                continue;
                            }

                            Coordinate[] coords = new Coordinate[5];
                            for (int i = 0; i < 4; ++i) {
                                coords[i] = new Coordinate(dst[i * 2],
                                        dst[i * 2 + 1]);
                            }
                            coords[4] = coords[0];
                            LineString l2 = geomFactory
                                    .createLineString(coords);
                            gridSO.addPolygon(new LineString[] { l2 }, color);
                            gridWF.addLineSegment(coords);
                        }
                    }
                }
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        RGB color = this.getCapability(ColorableCapability.class).getColor();

        if (lastColor == null || !lastColor.equals(color)) {
            lastColor = color;
            clearShapes();
        }

        if (gridWF == null) {
            buildShapes(target, color);
        }

        if (gridSO != null) {
            target.drawShadedShape(gridSO, 0.33f * paintProps.getAlpha());
        }

        if (gridWF != null) {
            target.drawWireframeShape(gridWF, color, 1);
        }

        if (mouseHandler.isSelectingArea()) {
            double xRatio = paintProps.getView().getExtent().getWidth()
                    / paintProps.getCanvasBounds().width;
            double yRatio = paintProps.getView().getExtent().getHeight()
                    / paintProps.getCanvasBounds().height;
            Rectangle r = mouseHandler.getScreenSelectionArea();
            IExtent ext = paintProps.getView().getExtent();
            PixelExtent pe = new PixelExtent(ext.getMinX() + r.x * xRatio,
                    ext.getMinX() + (r.x + r.width) * xRatio, ext.getMinY()
                            + r.y * yRatio, ext.getMinY() + (r.y + r.height)
                            * yRatio);
            target.drawRect(pe, new RGB(255, 255, 255), 3, 1);
        }
    }

    @Override
    public String getName() {
        return String
                .format("Alert Area #%d", getResourceData().getAreaIndex());
    }

    private class MouseHandler extends InputAdapter {

        private static final int MOUSE_BUTTON_TO_USE = 3;

        private boolean selectingArea;

        private boolean clicked = false;

        private Point clickCell = new Point(-1, -1);

        private Point screenClickPoint = new Point(0, 0);

        private Point endCell = new Point(-1, -1);

        private Point screenEndPoint = new Point(0, 0);

        public boolean isSelectingArea() {
            return selectingArea;
        }

        public Rectangle getScreenSelectionArea() {
            return new Rectangle(
                    Math.min(screenClickPoint.x, screenEndPoint.x), Math.min(
                            screenClickPoint.y, screenEndPoint.y),
                    Math.abs(screenClickPoint.x - screenEndPoint.x) + 1,
                    Math.abs(screenClickPoint.y - screenEndPoint.y) + 1);
        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            if (selectingArea) {
                finishSelectingArea(x, y);
            } else if (isEditable()) {
                screenClickPoint = new Point(x, y);
                if (mouseButton == MOUSE_BUTTON_TO_USE) {
                    clicked = true;
                    clickCell = screenToGridCell(x, y);
                }
            }
            return false;
        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            maybeHandleSelectArea(x, y);
            return false;
        }

        public void selectLocation() {
            if (isValidGridCell(clickCell)) {
                if (alertRequest != null) {
                    alertRequest.getBoxBits().flip(
                            clickCell.y * 58 + clickCell.x);
                    alertRequest.handleBoxBitsChanged();
                }
            }
            clicked = false;
        }

        public void startAreaSelection() {
            // TODO: if (! clicked), get current mouse coordinates?
            clicked = false;
            selectingArea = true;
            issueRefresh();
            screenEndPoint = new Point(screenClickPoint.x, screenClickPoint.y);
        }

        private void maybeHandleSelectArea(int x, int y) {
            if (selectingArea) {
                screenEndPoint = new Point(x, y);
                endCell = screenToGridCell(x, y);
                issueRefresh();
            }
        }

        private void finishSelectingArea(int x, int y) {

            maybeHandleSelectArea(x, y);
            selectingArea = false;

            if (alertRequest != null) {
                BitSet b = alertRequest.getBoxBits();

                int mnX = Math.min(clickCell.x, endCell.x);
                int mnY = Math.min(clickCell.y, endCell.y);
                int mxX = Math.max(clickCell.x, endCell.x);
                int mxY = Math.max(clickCell.y, endCell.y);
                if (mxX < 0 || mnX >= 58 || mxY < 0 || mnY >= 58) {
                    // nothing
                } else {
                    mnX = Math.max(mnX, 0);
                    mnY = Math.max(mnY, 0);
                    mxX = Math.min(mxX, 57);
                    mxY = Math.min(mxY, 57);

                    boolean sel = true;
                    if (isValidGridCell(clickCell)) {
                        sel = !b.get(clickCell.y * 58 + clickCell.x);
                    }
                    for (int yi = mnY; yi <= mxY; ++yi) {
                        if (sel) {
                            b.set(58 * yi + mnX, 58 * yi + mxX + 1);
                        } else {
                            b.clear(58 * yi + mnX, 58 * yi + mxX + 1);
                        }
                    }
                    alertRequest.handleBoxBitsChanged();
                }
            }

            redrawGrid();
        }

        @Override
        public boolean handleMouseMove(int x, int y) {
            maybeHandleSelectArea(x, y);
            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (selectingArea) {
                finishSelectingArea(x, y);
            } else if (clicked) {
                selectLocation();
            }
            return false;
        }

        boolean isValidGridCell(Point p) {
            return (p.x >= 0 && p.x < 58 && p.y >= 0 && p.y < 58);
        }

        Point validateGridCell(Point p) {
            if (isValidGridCell(p)) {
                return p;
            } else {
                return null;
            }
        }

        Point screenToGridCell(int x, int y) {
            Coordinate c = getResourceContainer().translateClick(x, y);
            double[] din = { c.x, c.y };
            double[] dout = new double[2];
            try {
                MathTransform mt = getLatLonToGrid();
                if (mt != null) {
                    mt.transform(din, 0, dout, 0, 1);
                    return new Point((int) Math.floor(dout[0]),
                            (int) Math.floor(dout[1]));
                }
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transforming coordinate", e);
            }
            return new Point(-1, -1);
        }

    }

    protected void clearGeometry() {
        gridGeometry = null;
        latLonToGrid = null;
    }

    protected void clearShapes() {
        if (gridWF != null) {
            gridWF.dispose();
            gridWF = null;
        }
        if (gridSO != null) {
            gridSO.dispose();
            gridSO = null;
        }
    }

    protected void redrawGrid() {
        clearShapes();
        issueRefresh();
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        clearShapes();
    }

    @Override
    public void handleChange(ChangeEvent event) {
        redrawGrid();
    }

}
