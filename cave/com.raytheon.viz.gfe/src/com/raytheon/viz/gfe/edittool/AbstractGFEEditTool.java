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
package com.raytheon.viz.gfe.edittool;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData.EditOp;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.rsc.GFESystemResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.tools.AbstractModalTool;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements the base edit tool architecture.
 * 
 * This class should be subclassed by those who want to implement GFE edit
 * tools. It provides a method to hook into user interactions (handleEvent) and
 * a mechanism to provide rendering capabilities. Users can subclass IRenderable
 * and return that renderable as part of the renderable list in this class.
 * These renderables will be contributed to the screen through the GFE System
 * Resource which will render them.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/20/2008              chammack    Initial Creation.
 * 04/16/2009   2262       rjpeter     Updated to handle mouse movement off the extent.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public abstract class AbstractGFEEditTool extends AbstractModalTool implements
        IContextMenuContributor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGFEEditTool.class);

    protected static enum EventType {
        START_DRAG, IN_DRAG, END_DRAG, MOUSE_CLICK
    };

    protected static enum ToolType {
        PARM_BASED, GENERAL
    };

    protected GeneralGridGeometry lastGridGeometry;

    protected MathTransform mathTransform;

    protected IInputHandler handler;

    /**
     * Handle a mouse event
     * 
     * @param button
     *            the mouse button used
     * @param type
     *            the event type
     * @param point2D
     *            the point in grid space (else null if outside grid)
     * @param coordinate
     *            the point in coordinate space
     */
    protected abstract void handleEvent(int button, EventType type,
            Point point2D, Coordinate coordinate);

    protected FreeformRenderable freeformRenderable;

    protected List<IRenderable> renderables;

    protected List<IRenderable> persistentRenderables;

    protected DataManager dataManager;

    /**
     * Constructor
     * 
     */
    protected AbstractGFEEditTool() {
        super();
        handler = new DelegateAction();
        freeformRenderable = new FreeformRenderable();
        renderables = new ArrayList<IRenderable>();
        persistentRenderables = new ArrayList<IRenderable>();
        renderables.add(freeformRenderable);
    }

    /**
     * Returns the grid geometry, if available
     * 
     * @return the grid geometry, or null if not available
     */
    private GeneralGridGeometry getGridGeometry() {
        return MapUtil.getGridGeometry(dataManager.getParmManager()
                .compositeGridLocation());
    }

    /**
     * Indicates whether the operation is allowed
     * 
     * If this method returns null, the operation is valid, otherwise it returns
     * a string indicating why the operation is not allowed.
     * 
     * @return a string indicating why the operation is not allowed, otherwise
     *         null.
     */
    protected String isOperationAllowed() {
        // the default implementation just returns null
        return null;
    }

    protected boolean isValidEditOp(EditOp editOp) {
        IGridData grid = getGrid();
        return (grid != null) && grid.isSupportedEditOp(editOp);
    }

    protected boolean isEditStateOK(boolean sendUserMessage) {
        String errmsg = isOperationAllowed();
        if (errmsg == null) {
            return true;
        }

        if (sendUserMessage && (errmsg != "")) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Editing is currently not allowed.  Reason: " + errmsg);

        }

        return false;

    }

    protected void processDrawEvent(EventType type, Coordinate[] coords) {
        switch (type) {
        case START_DRAG:
            freeformRenderable.clear();
            freeformRenderable.addAll(coords);
            break;
        case IN_DRAG:
            freeformRenderable.addAll(coords);
            break;
        case END_DRAG:
            freeformRenderable.clear();
            break;
        }
    }

    /**
     * Return the type of tool represented by this class
     * 
     * @return
     */
    protected abstract ToolType getToolType();

    /**
     * Return the list of renderables that is contributed to the display
     * 
     * These renderables are only visible when the tool is activated.
     * 
     * @return
     */
    public IRenderable[] getRenderables() {
        return renderables.toArray(new IRenderable[renderables.size()]);
    }

    /**
     * Return the list of persistent renderables that is contributed to the
     * display
     * 
     * These renderables are visible at all times (not just when the tool is
     * activated)
     * 
     * @return
     */
    public IRenderable[] getPersistentRenderables() {
        return persistentRenderables
                .toArray(new IRenderable[persistentRenderables.size()]);
    }

    /**
     * Return the math transform
     * 
     * @return
     */
    protected MathTransform getTransform() {
        GeneralGridGeometry gridGeometry = getGridGeometry();
        if ((gridGeometry != null) && !gridGeometry.equals(lastGridGeometry)) {
            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            try {
                MathTransform mathTransform = gridGeometry.getGridToCRS(
                        PixelInCell.CELL_CENTER).inverse();
                MathTransform fromLatLon = CRSCache.getInstance()
                        .getTransformFromLatLon(
                                gridGeometry.getCoordinateReferenceSystem());
                this.mathTransform = dmtf.createConcatenatedTransform(
                        fromLatLon, mathTransform);
                lastGridGeometry = gridGeometry;
            } catch (Exception e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "The mathtransform could not be computed for the editTool",
                                e);
                return null;
            }
        }

        return mathTransform;
    }

    protected String gridsUnlocked() {
        Parm parm = dataManager.getSpatialDisplayManager().getActivatedParm();

        if (!parm.isMutable()) {
            return "You have attempted to edit a grid from a model";
        }

        IGridData gridData = getGrid();

        TimeRange validTime = gridData.getGridTime();

        if (!parm.isOkToEdit(validTime)) {
            return "You have attempted to edit a grid that is locked";
        }

        return null;

    }

    protected IGridData getGrid() {
        if (dataManager == null) {
            return null;
        }
        Parm parm = dataManager.getSpatialDisplayManager().getActivatedParm();
        if (parm == null) {
            return null;
        }

        Date date = dataManager.getSpatialDisplayManager()
                .getSpatialEditorTime();

        if (date == null) {
            return null;
        }

        IGridData gridData = parm.overlappingGrid(date);

        return gridData;
    }

    @Override
    protected void activateTool() {
        editor.registerMouseHandler(handler);
        dataManager = DataManager.getCurrentInstance();

        try {
            dataManager.getSpatialDisplayManager().addEditTool(this);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Error activating gfe tool",
                    e);
        }

    }

    protected boolean startParmEdit() {
        Parm parm = dataManager.getSpatialDisplayManager().getActivatedParm();
        Date date = dataManager.getSpatialDisplayManager()
                .getSpatialEditorTime();

        dataManager.getParmOp().clearUndoParmList();

        try {
            parm.startParmEdit(new Date[] { date });
        } catch (GFEOperationFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "An error occurred when trying to start parm editing.", e);
            return false;
        }

        return true;

    }

    protected void endParmEdit() {
        Parm parm = dataManager.getSpatialDisplayManager().getActivatedParm();
        parm.endParmEdit();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractModalTool#deactivateTool()
     */
    @Override
    public void deactivateTool() {
        if (editor != null) {
            editor.unregisterMouseHandler(handler);
        }

        if (dataManager != null) {
            try {
                dataManager.getSpatialDisplayManager().removeEditTool(this);
            } catch (VizException e) {
                Activator
                        .getDefault()
                        .getLog()
                        .log(new Status(Status.ERROR, Activator.PLUGIN_ID,
                                "Error removing editTool", e));
            }
        }
    }

    protected class DelegateAction extends InputAdapter {

        private Coordinate mouseDownCoordinate;

        private Point mouseDownPoint;

        private boolean inDrag = false;

        /** Shift flag, do not process events if shift is held */
        private boolean shiftDown = false;

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.InputAdapter#handleKeyDown(int)
         */
        @Override
        public boolean handleKeyDown(int keyCode) {
            if ((keyCode & SWT.SHIFT) != 0) {
                shiftDown = true;
            }
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.InputAdapter#handleKeyUp(int)
         */
        @Override
        public boolean handleKeyUp(int keyCode) {
            if ((keyCode & SWT.SHIFT) != 0) {
                shiftDown = false;
            }
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            mouseDownPoint = null;
            mouseDownCoordinate = null;

            if (mouseButton == 3) {
                return false;
            }

            if (shiftDown) {
                return false;
            }

            if (!isEditStateOK(true)) {
                return true;
            }

            Coordinate coord = getCoordinate(x, y);
            Point point = transform(coord);
            if ((point == null) && (getToolType() == ToolType.PARM_BASED)) {
                return true;
            }

            mouseDownPoint = point;
            mouseDownCoordinate = coord;
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            if (shiftDown && !inDrag) {
                return false;
            }

            if (!isEditStateOK(true)) {
                return true;
            }

            Coordinate coord = getCoordinate(x, y);
            Point point = transform(coord);
            if ((point == null) && (getToolType() == ToolType.PARM_BASED)) {
                return true;
            }

            // did mouse movement start off the extent
            if (mouseDownPoint == null) {
                mouseDownPoint = point;
            }

            if (mouseDownCoordinate == null) {
                mouseDownCoordinate = coord;
            }

            if (!inDrag) {
                handleEvent(mouseButton, EventType.START_DRAG, mouseDownPoint,
                        mouseDownCoordinate);
                inDrag = true;
            }

            handleEvent(mouseButton, EventType.IN_DRAG, point, coord);

            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (shiftDown && !inDrag) {
                return false;
            }
            Coordinate coord = getCoordinate(x, y);
            Point point = transform(coord);

            if (!inDrag) {
                if ((point == null) && (getToolType() == ToolType.PARM_BASED)) {
                    return false;
                }

                handleEvent(mouseButton, EventType.MOUSE_CLICK, point, coord);
            } else {
                // always need to end the drag event, user may have started on
                // map and drug off map
                inDrag = false;
                handleEvent(mouseButton, EventType.END_DRAG, point, coord);
            }

            return true;
        }

        private Coordinate getCoordinate(int x, int y) {
            IDisplayPaneContainer container = EditorUtil
                    .getActiveVizContainer();
            if (container != null) {
                return container.translateClick(x, y);
            }
            return null;
        }

        private Point transform(Coordinate coord) {
            if (getToolType() == ToolType.GENERAL) {
                return null;
            }

            MathTransform mt = getTransform();
            if (mt == null) {
                return null;
            }

            if (coord != null) {
                double[] out = new double[2];
                try {
                    mt.transform(new double[] { coord.x, coord.y }, 0, out, 0,
                            1);
                } catch (TransformException e) {
                    return null;
                }
                return new Point((int) Math.round(out[0]),
                        (int) Math.round(out[1]));
            }

            return null;

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#refresh()
     */
    @Override
    protected void refresh() {
        ResourceList resourceList = this.editor.getActiveDisplayPane()
                .getDescriptor().getResourceList();
        for (ResourcePair rp : resourceList) {
            if (rp.getResource() instanceof GFESystemResource) {
                rp.getProperties().setVisible(true);
                rp.getResource().issueRefresh();
                break;
            }
        }
    }

}
