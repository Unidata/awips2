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
package com.raytheon.viz.gfe.edittool.contour;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.geotools.geometry.jts.JTS;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory.OriginType;
import com.raytheon.uf.common.dataplugin.gfe.RemapGrid;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockStatus;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PreferenceInitializer;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.contours.ContourAnalyzer;
import com.raytheon.viz.gfe.contours.SIRSGrid;
import com.raytheon.viz.gfe.contours.util.CLine;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData.EditOp;
import com.raytheon.viz.gfe.core.griddata.ScalarGridData;
import com.raytheon.viz.gfe.core.msgs.ContourServerMsg;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.msgs.ILockTableChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.AbstractFreeformTool;
import com.raytheon.viz.gfe.edittool.JTSRenderable;
import com.raytheon.viz.gfe.ui.ContourServerAlgorithmMenu.ContourServerAlgorithm;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

/**
 * The Contour Tool allows users to add new contours and delete old contours
 * from the display. Once the drawing phase is complete, users may then create a
 * new grids based on the edited contours using one of two algorithms.
 * 
 * -- implementation ---------------------------------------------------------
 * 
 * The tool keeps track of a current contour list. Operators allow the user to
 * add, delete and modify this list at will. When the user is done, a command to
 * recalculate the grid is performed via button3 pop-up menu. Once the data
 * changes, the contour list is regenerated and redisplayed for further editing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2009            randerso    Initial creation
 * Jul 02, 2010 6285       mpduff      Fixed contours to update after 
 *                                     drawing and calculating new grid.
 * Aug 08, 2012 #621       dgilling    Fix ConcurrentModificationException
 *                                     in handling of renderables field.
 * May 15, 2014 #3069      randerso    Changed to compute contour label spacing variable
 *                                     based on subsample setting
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ContourTool extends AbstractFreeformTool implements
        IActivatedParmChangedListener, ISpatialEditorTimeChangedListener,
        IGridDataChangedListener, IParmInventoryChangedListener,
        IDisplayedParmListChangedListener, ILockTableChangedListener,
        IMessageClient {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContourTool.class);

    private static final double MIN_CONTOUR_LENGTH = 3.0;

    private static RGB contourColor = new RGB(255, 255, 255);

    private static int subSample;

    ContourServerAlgorithm algorithm;

    private float[] currentContourValues;

    private boolean modifiedContours;

    private List<CLine> contours;

    private List<CLine> undoContours;

    private float contourValue;

    private IGridData currentGrid;

    private Parm parm;

    private GridLocation lowResGloc;

    private RemapGrid toLowRes;

    private RemapGrid toHiRes;

    private static int[] influences = new int[] { 1, 2, 5, 10, 15 };

    static {
        new PreferenceInitializer() {
            @Override
            public void init() {
                PythonPreferenceStore prefs = Activator.getDefault()
                        .getPreferenceStore();

                if (prefs.contains("ContourToolDrawing_color")) {
                    String color = prefs.getString("ContourToolDrawing_color");
                    contourColor = RGBColors.getRGBColor(color);
                }

                Integer[] ia = null;
                if (prefs.contains("PencilToolInfluence_list")) {
                    ia = prefs.getIntArray("PencilToolInfluence_list");
                    if (ia.length > 0) {
                        influences = new int[ia.length];
                        // convert Integer[] to int[]
                        for (int i = 0; i < ia.length; i++) {
                            influences[i] = ia[i];
                        }
                    }
                }

                subSample = 4;
                if (prefs.contains("ContourSubSample")) {
                    subSample = prefs.getInt("ContourSubSample");
                    if (subSample <= 0) {
                        subSample = 4;
                    }
                }
            }
        }.run();
    }

    /**
     * 
     */
    public ContourTool() {
        super();

    }

    private GridLocation getLowResGLoc(GridLocation gloc) {
        if (lowResGloc == null) {
            // new way to make grids from contours
            int subFactor = subSample;
            Point gridSize = gloc.gridSize();
            int newX = ((gridSize.x - 1) / subFactor) + 1;
            int newY = ((gridSize.y - 1) / subFactor) + 1;
            if ((newX <= 5) || (newY <= 5)) {
                int tmpX = gridSize.x / 5;
                int tmpY = gridSize.y / 5;
                subFactor = Math.min(tmpX, tmpY);
                newX = ((gridSize.x - 1) / subFactor) + 1;
                newY = ((gridSize.y - 1) / subFactor) + 1;
            }

            lowResGloc = new GridLocation("LowRes", gloc.getProjection(),
                    new Point(newX, newY), gloc.getOrigin(), gloc.getExtent(),
                    gloc.getTimeZone());
        }
        return lowResGloc;
    }

    private RemapGrid getToLowRes() {
        if ((toLowRes == null) && (currentGrid != null)) {
            computeRemaps();
        }
        return toLowRes;
    }

    private RemapGrid getToHiRes() {
        if ((toHiRes == null) && (currentGrid != null)) {
            computeRemaps();
        }
        return toHiRes;
    }

    private void computeRemaps() {
        GridLocation gloc = currentGrid.getParm().getGridInfo().getGridLoc();
        GridLocation lowRes = getLowResGLoc(gloc);
        toLowRes = new RemapGrid(gloc, lowRes);
        toHiRes = new RemapGrid(lowRes, gloc);
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void activateTool() {
        super.activateTool();

        Message.registerInterest(this, ContourServerMsg.class);
        receiveMessage(Message.inquireLastMessage(ContourServerMsg.class));

        currentGrid = getGrid();

        initializeContourData(currentGrid);

        // Get and draw the contours
        contours = getContours(currentContourValues);
        removeOldContours();

        // initialize undo with same
        undoContours = new ArrayList<CLine>(contours);

        replaceCLines(contours);

        dataManager.getSpatialDisplayManager().addActivatedParmChangedListener(
                this);
        dataManager.getSpatialDisplayManager()
                .addSpatialEditorTimeChangedListener(this);
        dataManager.getParmManager().addDisplayedParmListChangedListener(this);

        if (currentGrid != null) {
            currentGrid.getParm().getListeners().addGridChangedListener(this);
            currentGrid.getParm().getListeners()
                    .addParmInventoryChangedListener(this);
            currentGrid.getParm().getListeners()
                    .addLockTableChangedListener(this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.edittool.AbstractGFEEditTool#deactivateTool()
     */
    @SuppressWarnings("unchecked")
    @Override
    public void deactivateTool() {
        if (modifiedContours) {
            popRecalcDialog("Recalculate based on edited contours before switching tools?");
        }

        Message.unregisterInterest(this, ContourServerMsg.class);

        dataManager.getSpatialDisplayManager()
                .removeActivatedParmChangedListener(this);
        dataManager.getSpatialDisplayManager()
                .removeSpatialEditorTimeChangedListener(this);
        dataManager.getParmManager().removeDisplayedParmListChangedListener(
                this);

        if (currentGrid != null) {
            currentGrid.getParm().getListeners()
                    .removeGridChangedListener(this);
            currentGrid.getParm().getListeners()
                    .removeParmInventoryChangedListener(this);
            currentGrid.getParm().getListeners()
                    .removeLockTableChangedListener(this);
        }

        disposeRenderables();
        super.deactivateTool();
        refresh();
    }

    private void replaceCLines(List<CLine> contours) {
        clearRenderables();

        List<IRenderable> renderables = new ArrayList<IRenderable>(
                this.renderables);
        renderables.add(freeformRenderable);

        if (currentGrid != null) {
            MathTransform mt = MapUtil.getTransformToLatLon(
                    PixelOrientation.CENTER, this.lowResGloc);
            JTSRenderable renderable = new JTSRenderable();
            renderable.setLineWidth(2.0f);
            renderable.setColor(contourColor);
            renderable.setLabelSpacing(200 / subSample);
            for (CLine contour : contours) {
                LineString ls = contour.getLineString();
                if (ls == null) {
                    continue;
                }
                ScalarWxValue wxValue = new ScalarWxValue(
                        contour.getContourLevel(), currentGrid.getParm());
                String label = wxValue.toString();
                try {
                    Geometry geom = JTS.transform(contour.getLineString(), mt);
                    geom.setUserData(label);
                    renderable.addGeometry(geom);
                    // int numCoords = geom.getNumPoints();
                    // for (int j = 0; j < numCoords; j += 50) {
                    // renderable.addLabel(label, geom.getCoordinates()[j]);
                    // }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error creating contour lines", e);
                }
            }
            renderables.add(renderable);
        }

        this.renderables = renderables;
        refresh();
    }

    private void disposeRenderables() {
        List<IRenderable> renderables = new ArrayList<IRenderable>(
                this.renderables);
        for (IRenderable renderable : renderables) {
            if (renderable instanceof JTSRenderable) {
                ((JTSRenderable) renderable).dispose();
            }
        }
        renderables.clear();
        this.renderables = renderables;
    }

    private void clearRenderables() {
        List<IRenderable> renderables = new ArrayList<IRenderable>(
                this.renderables);
        for (IRenderable renderable : renderables) {
            if (renderable instanceof JTSRenderable) {
                ((JTSRenderable) renderable).clear();
            }
        }
        renderables.clear();
        this.renderables = renderables;
    }

    private void initializeContourData(IGridData grid) {

        // Clear out the old contours
        clearRenderables();

        // contours.clear();
        modifiedContours = false;

        // Get the active parm
        // Parm *parm = (Parm *)(dataMgr()->parmOp()->activeParm());
        parm = grid == null ? null : grid.getParm();

        if ((parm == null)
                || !parm.getGridInfo().getGridType().equals(GridType.SCALAR)
                || !parm.isMutable()) {
            // if (_gridID.parm() != NULL)
            // {
            // unregisterPC(_gridID.parm());
            // }
            // // set cached IDs to NULL
            currentGrid = null;
            // _parmID = ParmID();
            return;
        } else // parm != NULL
        {
            // if (parm->parmID() != _parmID) // reregister with new parm
            // {
            // if (_gridID.parm() != NULL)
            // unregisterPC(_gridID.parm());
            // registerPC(parm);
            // _parmID = parm->parmID();
            // _CLineVis->setPrecision(parm->precision());
            // }
        }

        // Save the gridID
        currentGrid = grid;

        // update the contour values
        currentContourValues = parm.getDisplayAttributes().getContourValues();

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.edittool.AbstractFreeformTool#handleEndDrag(int,
     * java.awt.geom.Point2D, com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected void handleEndDrag(int button, Point2D point2D,
            Coordinate coordinate) {

        if (button == 1) {
            drag1Event();
        } else if (button == 2) {
            drag2Event();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.edittool.AbstractFreeformTool#handleMouseClick(int,
     * java.awt.Point, com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected void handleMouseClick(int button, Point point2D,
            Coordinate coordinate) {
        if (button == 1) {
            clickButton1(coordinate);
        } else if (button == 2) {
            clickButton2(coordinate);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.edittool.AbstractGFEEditTool#getToolType()
     */
    @Override
    protected ToolType getToolType() {
        return ToolType.PARM_BASED;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.edittool.AbstractGFEEditTool#isOperationAllowed()
     */
    @Override
    protected String isOperationAllowed() {
        String reasonWhyNot = null;

        if (parm == null) {
            reasonWhyNot = "The contour tool only works when a grid is editable";
            return reasonWhyNot;
        }

        // We don't contour vector, weather, or discrete grids,
        // but we don't want to throw an error either
        GridType type = parm.getGridInfo().getGridType();
        if (type.equals(GridType.VECTOR) || type.equals(GridType.DISCRETE)
                || type.equals(GridType.WEATHER)) {
            reasonWhyNot = "";
        } else if (currentGrid == null) {
            reasonWhyNot = "The Contour Tool works only on one grid at a time.";
        }

        if (reasonWhyNot == null) {
            reasonWhyNot = gridsUnlocked();
        }

        if (reasonWhyNot == null) {
            if (!isValidEditOp(EditOp.CONTOUR)) {
                reasonWhyNot = "The Contour Tool is not allowed on grids of this type.";
            }
        }

        return reasonWhyNot;
    }

    /**
     * This function is called before the grid is calculated to remove any old
     * contours that conflict with any new contours before the grid is
     * recalculated.
     * 
     * Creates a Grid2DBit where the set points indicate the areas of new
     * contours inside which no other old contours may exist. Then calls another
     * function that removes any old contour segments that cross into this area.
     * Sets the results to the current list contours.
     * 
     */
    private void removeOldContours() {
        if (currentGrid == null) {
            return;
        }

        // no reason to bother unless there are modified contours
        if (!modifiedContours) {
            return;
        }

        // Make a Grid2DBit that covers the areas occupied by the new contours
        GridLocation gridLoc = currentGrid.getParm().getGridInfo().getGridLoc();
        Grid2DBit newArea = new Grid2DBit(gridLoc.gridSize().x,
                gridLoc.gridSize().y);
        // Coordinate cellSize = gridLoc.gridCellSize();
        // double swathSize = (cellSize.x + cellSize.y) / 2 * 5; // 5 gridCells
        double swathSize = 5;

        for (CLine contour : contours) {
            if (contour.isModified()) {
                newArea.orEquals(gridLoc.gridCellSwath(contour.getLineString()
                        .getCoordinates(), swathSize, false));
            }
        }

        // Find out which segments in the old contours pass thru these areas
        ArrayList<CLine> newContours = new ArrayList<CLine>();
        for (CLine contour : contours) {
            if (contour.isModified()) {
                newContours.add(contour);
            } else // see if the old contour crosses into the newArea
            {
                // Find the parts outside the newArea and save them
                List<CLine> newCLines = findCLinesOutside(newArea, contour);

                newContours.addAll(newCLines);
            }
        }

        // Replace the old set of contours with this newly processed set
        contours = newContours;
    }

    /**
     * Determines which points lie outside the specified area and returns a new
     * set of contours that consist of these outside points.
     * 
     * Check if any point is inside the specified area. If not, save the point.
     * Return a new set of contours all of which lie completely outside this
     * area.
     * 
     * @param area
     * @param contour
     * @return
     */
    private List<CLine> findCLinesOutside(final Grid2DBit newArea,
            final CLine cline) {
        // Create a list to hold the answer
        LineString line = cline.getLineString();
        List<CLine> result = new ArrayList<CLine>(line.getNumPoints() / 2);

        // Create a list to hold points outside newArea
        ArrayList<Coordinate> subLine = new ArrayList<Coordinate>(
                line.getNumPoints());
        Coordinate coordCopy;
        for (Coordinate coord : line.getCoordinates()) {
            if ((byte) 1 == newArea.get((int) coord.x, (int) coord.y)) {
                // This point is in newArea, so we can't keep it.
                if (longEnough(subLine)) {
                    // Convert subLine to a CLine and store in result
                    CLine newLine = new CLine(
                            subLine.toArray(new Coordinate[subLine.size()]),
                            cline.getContourLevel(), false);
                    result.add(newLine);
                }
                subLine.clear();
            } else {
                // copy the coordinate to eliminate reference-sharing bugs
                coordCopy = new Coordinate(coord);
                subLine.add(coordCopy);
            }
        }
        // We're out of points.
        // Clean up any points in subLine
        if (longEnough(subLine)) {
            CLine newLine = new CLine(subLine.toArray(new Coordinate[subLine
                    .size()]), cline.getContourLevel(), false);
            result.add(newLine);
        }
        return result;
    }

    /**
     * Determines whether a contour is at least MIN_CONTOUR_LENGTH units long.
     * 
     * @param subLine
     * @return
     */
    private boolean longEnough(List<Coordinate> subLine) {
        if (subLine.size() > 0) {
            double totalLen = 0.0;
            Coordinate prev = subLine.get(0);
            for (Coordinate cur : subLine) {
                totalLen += cur.distance(prev);
                prev = cur;
            }
            if (totalLen > MIN_CONTOUR_LENGTH) {
                return true;
            }
        }
        return false;
    }

    // -- private
    // ----------------------------------------------------------------
    // ContourTool::getContours
    //
    // Calls a function that returns the set of contours for the current grid.
    // These contours are generally used for the current set for the user to
    // edit.
    // -- implementation
    // ---------------------------------------------------------
    //
    // ---------------------------------------------------------------------------
    private List<CLine> getContours(float[] contourValues) {
        // Make cure we have a current grid
        if (currentGrid != null) {

            try {
                GridParmInfo gridInfo = currentGrid.getParm().getGridInfo();
                GridLocation gridLoc = gridInfo.getGridLoc();
                Coordinate origin = gridLoc.getOrigin();
                Coordinate extent = gridLoc.getExtent();
                Grid2DFloat lowres = getToLowRes()
                        .remap(((ScalarGridSlice) currentGrid.getGridSlice())
                                .getScalarGrid(),
                                gridInfo.getMinValue(), gridInfo.getMaxValue(),
                                gridInfo.getMinValue(), gridInfo.getMinValue());

                return ContourGrid.createContours(lowres, contourValues);
                // ContourGrid2 cGrid = new ContourGrid2(lowres, contourValues,
                // (float) origin.x, (float) origin.y,
                // (float) ((lowres.getXdim() - 1) / extent.x),
                // (float) ((lowres.getYdim() - 1) / extent.y));
                // return cGrid.makeContoursFromGrid(contourValues);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error computing contours", e);
            }
        }

        return new ArrayList<CLine>();
    }

    // -- public
    // -----------------------------------------------------------------
    // ContourTool::newActiveGrid()
    //
    // Called by the EditToolManager receives a SPATIAL_EDITOR_TIME_CHANGED msg
    // or a MAKE_ONLY_ACTIVE msg. Reinitialize the contour data and its visuals.
    // -- implementation
    // ---------------------------------------------------------
    //
    // ---------------------------------------------------------------------------
    public void newActiveGrid(final IGridData grid) {
        // initEventData();

        if (modifiedContours) {
            popRecalcDialog("Recalculate based on edited contours before switching grids?");
        }

        if (currentGrid != null) {
            currentGrid.getParm().getListeners()
                    .removeGridChangedListener(this);
            currentGrid.getParm().getListeners()
                    .removeParmInventoryChangedListener(this);
            currentGrid.getParm().getListeners()
                    .removeLockTableChangedListener(this);
        }

        initializeContourData(grid);

        // See if it's safe to make new contours
        Parm parm = grid == null ? null : grid.getParm();
        if ((grid == null) || (parm == null)
                || !parm.getGridInfo().getGridType().equals(GridType.SCALAR)
                || !parm.isMutable()) {
            return;
        }

        // Make new contours for this grid and paint them
        contours = getContours(currentContourValues);
        replaceCLines(contours);

        currentGrid.getParm().getListeners().addGridChangedListener(this);
        currentGrid.getParm().getListeners()
                .addParmInventoryChangedListener(this);
        currentGrid.getParm().getListeners().addLockTableChangedListener(this);
    }

    private void popRecalcDialog(String prompt) {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        if (MessageDialog.openQuestion(shell, "Recalculate Grid?", prompt)) {
            updateGridBasedOnContours();
        }
        modifiedContours = false;
    }

    @Override
    public void activatedParmChanged(Parm newParm) {
        newActiveGrid(getGrid());
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener#
     * spatialEditorTimeChanged(java.util.Date)
     */
    @Override
    public void spatialEditorTimeChanged(Date date) {
        newActiveGrid(getGrid());
    }

    /**
     * Adds a new contour to the existing contour set.
     * 
     * Just call do drag. At end drag, check the edit state, convert the
     * mapcoords to floatCoords, make a CLine and add it to the current list of
     * contours.
     * 
     */
    protected void drag1Event() {
        // bool start = (dragState == EditTool::StartDrag);
        //
        // if (currentParm() == NULL || currentGrid() == NULL ||
        // !editStateOK(start))
        // return;
        //
        // if (dragState == EditTool::StartDrag)
        contourValue = ((ScalarWxValue) currentGrid.getParm().getParmState()
                .getPickUpValue()).getValue();
        //
        // doDrag(dragState, mapCoords);
        //
        // if (dragState == EditTool::EndDrag)
        // {
        if ((currentGrid != null) && isEditStateOK(false)) {
            // Convert latLons to gridCoords
            List<Coordinate> gridCoordinates = getGridCoordinates();

            // Make a contour and prepend it to the master array
            CLine cLine = new CLine(
                    gridCoordinates.toArray(new Coordinate[gridCoordinates
                            .size()]), contourValue, true);
            undoContours = new ArrayList<CLine>(contours);
            contours.add(0, cLine);
            modifiedContours = true;
            removeOldContours();
            // logUse << "Drag1: draw new contour. Val=" << _contourValue
            // << " Pts: " << floatCoords.length() << " NumContours: "
            // << _contours.length() << "GridID=" << gridID() << std::endl;
            replaceCLines(contours); // causes a repaint

            forceGridLock();
        }
        // }
    }

    /**
     * convert currentCoordinates from lat/lon to grid cell and fill in any gaps
     * 
     * @return
     */
    private List<Coordinate> getGridCoordinates() {
        GridLocation lowResGloc = this.lowResGloc;

        List<Coordinate> gridCoordinates = new ArrayList<Coordinate>(
                currentCoordinates.size());

        Coordinate prev = MapUtil.latLonToGridCoordinate(
                currentCoordinates.get(0), PixelOrientation.CENTER, lowResGloc);
        gridCoordinates.add(prev);
        for (int i = 1; i < currentCoordinates.size(); i++) {
            // convert to grid coordinate
            Coordinate gridCoord = MapUtil.latLonToGridCoordinate(
                    currentCoordinates.get(i), PixelOrientation.CENTER,
                    lowResGloc);

            // eliminate gaps
            double d = prev.distance(gridCoord);
            double dx = (gridCoord.x - prev.x) / d;
            double dy = (gridCoord.y - prev.y) / d;
            while (d > Math.sqrt(2)) {
                Coordinate c = new Coordinate(prev.x + dx, prev.y + dy);

                gridCoordinates.add(c);
                prev = c;
                d = prev.distance(gridCoord);
            }

            gridCoordinates.add(gridCoord);
            prev = gridCoord;
        }
        return gridCoordinates;
    }

    /**
     * Forces the grid to be locked, to prevent another user from editing the
     * data before the actual GRID data changes.
     * 
     * @return true if successful
     */
    public boolean forceGridLock() {
        if (currentGrid != null) {
            return currentGrid.lockGrid();
        }
        return false;
    }

    /**
     * Adjusts the closest contour to a new location. Similar to add a new
     * contours, but uses the value closest to the start point. Inserts the
     * adjustment into the existing contour by calling adjustContour().
     * 
     */
    protected void drag2Event() {
        // bool start = (dragState == EditTool::StartDrag);
        // if (currentParm() == NULL || currentGrid() == NULL ||
        // !editStateOK(start))
        // return;
        //
        // if (dragState == EditTool::StartDrag)
        List<Coordinate> gridCoords = getGridCoordinates();

        setAdjustContourValue(gridCoords.get(0)); // defines _contourValue
        //
        // // draw the line
        // doDrag(dragState, mapCoords);
        //
        // if (dragState == EditTool::EndDrag)
        // {
        if ((currentGrid != null) && isEditStateOK(false)) {
            // Save the old state of the contours
            undoContours = new ArrayList<CLine>(contours);

            adjustContour(gridCoords); // replaces old part with new part of
            // contour
            WxValue wxValue = new ScalarWxValue(contourValue,
                    currentGrid.getParm());

            // Convert mapCoords to screenCoords for pencilStretch()
            // SeqOf<CartCoord2D<float> > floatCoords;
            // for (int i = 0; i < _coords.length(); i++)
            // floatCoords.append(_coords[i]);

            startParmEdit();

            currentGrid.getParm()
                    .pencilStretch(
                            currentGrid.getGridTime().getStart(),
                            wxValue,
                            currentCoordinates
                                    .toArray(new Coordinate[currentCoordinates
                                            .size()]), false);

            // logUse << "Drag2: modify contour. Val=" << _contourValue
            // << " Pts: " << floatCoords.length() << " NumContours: "
            // << _contours.length() << " Grid=" << gridID() << std::endl;

            endParmEdit();
        }
        // }
    }

    private void setAdjustContourValue(Coordinate gridCoord) {
        int contourIndex = findClosestContour(contours, gridCoord);

        if ((contourIndex >= 0) && (contourIndex < contours.size())) {
            contourValue = contours.get(contourIndex).getContourLevel();
        } else {
            contourValue = ((ScalarWxValue) currentGrid.getParm()
                    .getParmState().getPickUpValue()).getValue();
        }
    }

    /**
     * This function adds a new contour to the current list of contours. Users
     * may then adjust that contour thereby affecting the data inbetween the
     * standard contour values.
     * 
     * Get the value under the cursor. Add that value to the list of contour
     * values being careful to keep the list in ascending order. Finally, redraw
     * the contours.
     * 
     * @param mapCoord
     */
    protected void clickButton1(Coordinate mapCoord) {
        if ((currentGrid == null) || (currentGrid.getParm() == null)
                || !isEditStateOK(true)) {
            return;
        }

        // Get the grid coordinate
        float newContourValue;
        Coordinate gridCoord = MapUtil.latLonToGridCoordinate(mapCoord,
                PixelOrientation.CENTER, currentGrid.getParm().getGridInfo()
                        .getGridLoc());
        int x = (int) gridCoord.x;
        int y = (int) gridCoord.y;
        Grid2DFloat scalarGrid = ((ScalarGridSlice) currentGrid.getGridSlice())
                .getScalarGrid();
        if (scalarGrid.isValid(x, y)) {
            newContourValue = scalarGrid.get(x, y);
        } else {
            return;
        }

        // Add the new value to the list of contours
        float[] temp = new float[currentContourValues.length + 1];
        System.arraycopy(currentContourValues, 0, temp, 0,
                currentContourValues.length);
        temp[currentContourValues.length] = newContourValue;
        currentContourValues = temp;

        // Get and draw the contours
        float[] contourValue = new float[] { newContourValue };
        List<CLine> contours = getContours(contourValue);

        // We're only interested in the one contour that is closest
        Coordinate lowResCoord = MapUtil.latLonToGridCoordinate(mapCoord,
                PixelOrientation.CENTER, this.lowResGloc);
        int closestContour = findClosestContour(contours, lowResCoord);
        if (closestContour == -1) {
            statusHandler
                    .handle(Priority.SIGNIFICANT,
                            "ContourTool: Can't Click1 to add new contour on flat field");
            return;
        }

        // Append this contour to the list of contours
        CLine newContour = contours.get(closestContour);
        newContour.setModified(true);
        undoContours = new ArrayList<CLine>(contours);
        this.contours.add(newContour);

        // logUse << "Click1: add new contour. Val=" << newContourValue
        // << "GridID=" << gridID() << std::endl;

        // Re-paint the set of contours
        removeOldContours();
        replaceCLines(this.contours);

        modifiedContours = true;
        forceGridLock();
    }

    /**
     * Deletes or removes a contour from the current list of contours and
     * repaints the display.
     * 
     * @param mapCoord
     */
    protected void clickButton2(Coordinate mapCoord) {
        if ((currentGrid == null) || (currentGrid.getParm() == null)
                || !isEditStateOK(true)) {
            return;
        }

        // logUse << "Click2: Delete existing contour. GridID=" << gridID()
        // << std::endl;
        Coordinate gridCoord = MapUtil.latLonToGridCoordinate(mapCoord,
                PixelOrientation.CENTER, this.lowResGloc);
        deleteContour(gridCoord);

        modifiedContours = true;
        forceGridLock();
    }

    /**
     * Removes a contours from the display as specified by location and redraws
     * the display.
     * 
     * @param mapCoord
     */
    private void deleteContour(Coordinate location) {
        // Find the contour that's closest to location.
        int closest = findClosestContour(contours, location);
        if (closest < 0) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "ContourTool: Couldn't find any contours to delete");
            return;
        }

        // Save the previous state
        undoContours = new ArrayList<CLine>(contours);

        // Delete the contour
        contours.remove(closest);

        int nearestContour = findClosestContour(contours, location);
        if (nearestContour >= 0) {
            contours.get(nearestContour).setModified(true);
        }

        replaceCLines(contours);
    }

    /**
     * Inserts the segment (coords) into the contour that lies closest to the
     * start point of the segment. Directly modifies the _contours list of
     * contours and returns. No repainting or grid recalculations are done here.
     */
    protected void adjustContour(List<Coordinate> coords) {
        // Convert the latest edit operation to floatCoords
        // List<Coordinate> floatCoords;
        // for (int i = 0; i < _coords.length(); i++)
        // floatCoords.append(_coords[i]);

        // find the contour we are adjusting
        Coordinate startPoint = coords.get(0);
        Coordinate endPoint = coords.get(coords.size() - 1);
        int contourIndex = findClosestContour(contours, startPoint);
        // Check for valid contour
        if (contourIndex < 0) {
            return;
        }

        // make a copy of the contour
        CLine contour = contours.get(contourIndex);
        contour = new CLine(contour.getLineString().getCoordinates(),
                contour.getContourLevel(), contour.isModified());

        // Find the starting and ending point for the section we will remove
        int startIndex = findClosestCLineIndex(contour, startPoint);
        int endIndex = findClosestCLineIndex(contour, endPoint);

        boolean replaceMiddle = false;
        if (!closedContour(contour)) {
            replaceMiddle = true;
        } else // it's a closed, see which section is closer to new contour
        {
            // Determine which is closer, the middle segment or the two ends
            Coordinate newLoc = avgLocation(coords, 0, coords.size() - 1);
            Coordinate middleLoc = avgLocation(contour, startIndex, endIndex);
            // note the indices are reversed
            Coordinate endsLoc = avgLocation(contour, endIndex, startIndex);
            if (newLoc.distance(middleLoc) < newLoc.distance(endsLoc)) {
                replaceMiddle = true;
            }
        }
        List<Coordinate> insert = new ArrayList<Coordinate>(coords);
        if (replaceMiddle) // replace middle section of contour
        {
            if (startIndex > endIndex) // reverse the order of coords
            {
                Collections.reverse(insert);
                contour.replace(endIndex, startIndex, insert);
                contour.setModified(true);
            } else // startIndex < endIndex
            {
                contour.replace(startIndex, endIndex, insert);
                contour.setModified(true);
            }
        } else // replace the ends
        {
            if (startIndex < endIndex) // reverse the order of coords
            {
                Collections.reverse(insert);
                contour.remove(0, startIndex);
                contour.replace(endIndex, contour.getLineString()
                        .getNumPoints(), insert);
            } else // startIndex > endIndex
            {
                contour.replace(startIndex, contour.getLineString()
                        .getNumPoints(), insert);
                contour.remove(0, endIndex);
            }
            contour.setModified(true);
        }
        contours.set(contourIndex, contour);
        replaceCLines(contours);
    }

    /**
     * Returns the average of the location of the specified coordinates and
     * start, end locations. Utility function used by adjustContour().
     * 
     * @param coords
     * @param start
     * @param end
     * @return
     */
    private Coordinate avgLocation(CLine cline, int start, int end) {
        return avgLocation(
                Arrays.asList(cline.getLineString().getCoordinates()), start,
                end);
    }

    /**
     * Returns the average of the location of the specified coordinates and
     * start, end locations. Utility function used by adjustContour().
     * 
     * @param coords
     * @param start
     * @param end
     * @return
     */
    private Coordinate avgLocation(List<Coordinate> coords, int start, int end) {
        Coordinate sumLoc = new Coordinate(0.0, 0.0);
        if (start < end) {
            for (int i = start; i <= end; i++) {
                sumLoc.x += coords.get(i).x;
                sumLoc.y += coords.get(i).y;
            }
            sumLoc.x = sumLoc.x / ((end - start) + 1);
            sumLoc.y = sumLoc.y / ((end - start) + 1);
            return sumLoc;
        } else {
            int i;
            for (i = end; i < coords.size(); i++) {
                sumLoc.x += coords.get(i).x;
                sumLoc.y += coords.get(i).y;
            }
            for (i = 0; i <= start; i++) {
                sumLoc.x += coords.get(i).x;
                sumLoc.y += coords.get(i).y;
            }
            sumLoc.x = sumLoc.x / ((start - end) + 1);
            sumLoc.y = sumLoc.y / ((start - end) + 1);
            return sumLoc;
        }
    }

    /**
     * Returns true if the specified contour is closed.
     * 
     * If the distance between the first and the last point in the contour is
     * less than 3 gridCells, the contour is defined as closed.
     * 
     * @param line
     * @return
     */
    private boolean closedContour(CLine cline) {
        if (currentGrid == null) {
            return false;
        }

        // get the size of a grid cell
        Coordinate cellSize = currentGrid.getParm().getGridInfo().getGridLoc()
                .gridCellSize();

        LineString line = cline.getLineString();
        double distance = line.getCoordinateN(0).distance(
                line.getCoordinateN(line.getNumPoints() - 1));

        if (distance < ((3 * (cellSize.x + cellSize.y)) / 2)) {
            return true;
        }

        return false;
    }

    /**
     * Returns the index of the point that is closest to the specified point.
     * 
     * @param cline
     * @param point
     * @return
     */
    private int findClosestCLineIndex(CLine cline, Coordinate point) {
        LineString line = cline.getLineString();
        if (line.getNumPoints() < 1) {
            statusHandler.handle(Priority.PROBLEM,
                    "CLine is empty in findClosestCLineIndex.");
            return -1;
        }

        int index = 0;
        // Find the closest index
        double minDist = point.distance(line.getCoordinateN(0));
        for (int i = 1; i < line.getNumPoints(); i++) {
            double dist = point.distance(line.getCoordinateN(i));
            if (dist < minDist) {
                minDist = dist;
                index = i;
            }
        }

        return index;
    }

    /**
     * This functions looks for the contour that is closest to the specified
     * point and returns the index of this contour.
     * 
     * @param contours2
     * @param location
     * @return
     */
    protected int findClosestContour(List<CLine> contours, Coordinate location) {
        if (contours.size() == 0) {
            return -1;
        }

        double minDist = location.distance(contours.get(0).getLineString()
                .getCoordinateN(0));
        int closestContour = 0;
        // loop through each contour and calculate the distance to each point
        for (int i = 0; i < contours.size(); i++) {
            LineString line = contours.get(i).getLineString();
            for (int j = 0; j < line.getNumPoints(); j++) {
                double dist = location.distance(line.getCoordinateN(j));
                if (dist < minDist) {
                    minDist = dist;
                    closestContour = i;
                }
            }
        }

        return closestContour;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IRightClickCapableResource#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        Parm activeParm = dataManager.getSpatialDisplayManager()
                .getActivatedParm();
        if ((activeParm != null)
                && activeParm.getGridInfo().getGridType()
                        .equals(GridType.SCALAR)) {

            Coordinate sz;
            sz = activeParm.getGridInfo().getGridLoc().gridCellSize();

            if (sz != null) {
                menuManager.add(new ContourMenuAction(sz.x));
            }
        }

    }

    private class ContourMenuAction extends AbstractRightClickAction implements
            IMenuCreator {

        private Menu menu;

        private final double resolution;

        public ContourMenuAction(double resolution) {
            super(SWT.DROP_DOWN);
            this.resolution = resolution;
        }

        @Override
        public IMenuCreator getMenuCreator() {
            return this;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.IMenuCreator#dispose()
         */
        @Override
        public void dispose() {
            if (menu != null) {
                menu.dispose();
            }
        }

        @Override
        public Menu getMenu(Control parent) {
            if (menu != null) {
                menu.dispose();
            }

            menu = new Menu(parent);
            fillMenu();
            return menu;
        }

        @Override
        public Menu getMenu(Menu parent) {
            if (menu != null) {
                menu.dispose();
            }

            menu = new Menu(parent);
            fillMenu();
            return menu;
        }

        @Override
        public String getText() {
            return "Contours";
        }

        private void fillMenu() {
            ActionContributionItem aci;
            aci = new ActionContributionItem(new CalculateNewGridAction());
            aci.fill(menu, -1);
            aci = new ActionContributionItem(new UndoLastContourEditAction());
            aci.fill(menu, -1);
            aci = new ActionContributionItem(new DeleteAllContoursAction());
            aci.fill(menu, -1);
            aci = new ActionContributionItem(new ContourSubmenuAction(
                    resolution));
            aci.fill(menu, -1);
        }
    }

    private class ContourSubmenuAction extends AbstractRightClickAction
            implements IMenuCreator {

        private Menu menu;

        private final double resolution;

        public ContourSubmenuAction(double resolution) {
            super(SWT.DROP_DOWN);
            this.resolution = resolution;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getMenuCreator()
         */
        @Override
        public IMenuCreator getMenuCreator() {
            return this;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.IMenuCreator#dispose()
         */
        @Override
        public void dispose() {
            if (menu != null) {
                menu.dispose();
            }
        }

        @Override
        public Menu getMenu(Control parent) {
            if (menu != null) {
                menu.dispose();
            }

            menu = new Menu(parent);
            createInfluences();
            return menu;
        }

        @Override
        public Menu getMenu(Menu parent) {
            if (menu != null) {
                menu.dispose();
            }

            menu = new Menu(parent);
            createInfluences();
            return menu;
        }

        private void createInfluences() {
            // AWIPS1 has this interesting bug when it populates the
            // "Contour Adjust Influence" menu. It calculates all of the
            // influence values (in km), but does not add the last in the list
            // to the sub-menu.
            // See ContourTool.C, lines 1309-1319
            // However, this off-by-one looping error has not been ported
            // forward.
            for (int influence : influences) {
                ActionContributionItem aci = new ActionContributionItem(
                        new PencilToolAction(resolution, influence));
                aci.fill(menu, -1);
            }

        }

        @Override
        public String getText() {
            return "Contour Adjust Influence";
        }

    }

    private class CalculateNewGridAction extends Action {

        @Override
        public void run() {
            updateGridBasedOnContours();
        }

        @Override
        public String getText() {
            return "Calculate New Grid";
        }
    }

    private class UndoLastContourEditAction extends Action {

        @Override
        public void run() {
            if ((currentGrid == null) || (currentGrid.getParm() == null)) {
                return;
            }

            contours = undoContours;

            // Since these contours were added back, mark them as new.
            for (CLine contour : contours) {
                contour.setModified(true);
            }

            // Redraw the contours
            replaceCLines(contours);
        }

        @Override
        public String getText() {
            return "Undo Last Contour Edit";
        }
    }

    private class DeleteAllContoursAction extends Action {

        @Override
        public void run() {
            contours.clear();

            replaceCLines(contours);
            forceGridLock();
        }

        @Override
        public String getText() {
            return "Delete All Contours";
        }
    }

    private class PencilToolAction extends Action {

        private final double resolution;

        private final int influence;

        public PencilToolAction(double resolution, int influence) {
            this.resolution = resolution;
            this.influence = influence;
            Parm activatedParm = dataManager.getSpatialDisplayManager()
                    .getActivatedParm();
            if (activatedParm.getParmState().getPencilWidth() == influence) {
                setChecked(true);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            DecimalFormat df = new DecimalFormat();
            df.setMaximumFractionDigits(1);
            return df.format(resolution * influence) + " km (" + influence
                    + ")";
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            Parm p = dataManager.getSpatialDisplayManager().getActivatedParm();
            if (p != null) {
                p.getParmState().setPencilWidth(influence);
            }
        }

    }

    /**
     * Calls the algorithm to calculate a new grid based on the current set of
     * contours. Then the old grid is replaced by the new one.
     * 
     */
    private void updateGridBasedOnContours() {
        if ((currentGrid == null) || (currentGrid.getParm() == null)) {
            return;
        }

        // If no contours are defined send error message
        if (contours.size() == 0) {
            statusHandler
                    .handle(Priority.SIGNIFICANT,
                            "ContourTool: Please define some contours before attempting to recalculate");
            return;
        }

        // wait cursor on
        // setCursor(0);

        // Remove any conflicting contours
        removeOldContours();

        // Run SIRS and make a new GridData.
        IGridData newGrid = makeSirsGrid();

        // The new grid data must NOT be deleted after calling
        // replaceGriddedData
        // The parm just copies the pointer and takes ownership of the grid.
        // The function also calls startParmEdit() and endParmEdit().
        if (newGrid != null) {
            currentGrid.getParm().replaceGriddedData(currentGrid.getGridTime(),
                    newGrid);
        }

        // Get latest grid and initialize contour data
        currentGrid = getGrid();
        initializeContourData(currentGrid);

        // Get and draw the contours
        contours = getContours(currentContourValues);
        replaceCLines(contours);

        // wait cursor off
        // setCursor(1);

        modifiedContours = false;
    }

    // -- private
    // ----------------------------------------------------------------
    // ContourTool::makeSirsGrid()
    //
    // Calls the algorithm to calculate a new grid based on the current
    // set of contours. Returns a brand new grid as GridData pointer.
    //
    // -- implementation
    // ---------------------------------------------------------
    //
    // ---------------------------------------------------------------------------
    private IGridData makeSirsGrid() {
        // Make sure the current grid is valid
        if (currentGrid == null) {
            return null;
        }

        try {
            Grid2DFloat dataGrid = recomputeGrid();
            if (!dataGrid.isValid()) {
                return null;
            }

            GridParmInfo gridInfo = currentGrid.getParm().getGridInfo();
            dataGrid = getToHiRes().remap(dataGrid, gridInfo.getMinValue(),
                    gridInfo.getMaxValue(), gridInfo.getMinValue(),
                    gridInfo.getMinValue());

            IGridSlice gridSlice = new ScalarGridSlice(
                    currentGrid.getGridTime(), currentGrid.getParm()
                            .getGridInfo(), currentGrid.getHistory(), dataGrid);

            IGridData grid = ScalarGridData.makeGridData(currentGrid.getParm(),
                    gridSlice);
            grid.updateHistory(new GridDataHistory(OriginType.CALCULATED,
                    currentGrid.getParm().getParmID(), currentGrid
                            .getGridTime()));
            grid.updateHistoryToModified(dataManager.getWsId());

            return grid;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error recomputing grid", e);
        }
        return null;
    }

    private Grid2DFloat recomputeGrid() throws FactoryException,
            TransformException {

        Grid2DFloat dataGrid = null;
        GridParmInfo gridInfo = currentGrid.getParm().getGridInfo();
        Grid2DFloat lowres = getToLowRes().remap(
                ((ScalarGridSlice) currentGrid.getGridSlice()).getScalarGrid(),
                gridInfo.getMinValue(), gridInfo.getMaxValue(),
                gridInfo.getMinValue(), gridInfo.getMinValue());
        if (algorithm.equals(ContourServerAlgorithm.CONTOUR_ANALYZER)) {
            // new way to make grids from contours
            ContourAnalyzer analyzer = new ContourAnalyzer(lowres, contours,
                    0.0f, 0.0f, 1.0f, 1.0f, 0, lowres.getXdim() - 1, 0,
                    lowres.getYdim() - 1, true, gridInfo.getMaxValue(),
                    gridInfo.getMinValue());
            dataGrid = analyzer.recomputeGrid();
        } else if (algorithm.equals(ContourServerAlgorithm.SIRS_SERVER)) {
            // alternate: FSL version of SIRS
            SIRSGrid sirsGrid = new SIRSGrid(lowres, contours, 0.0f, 0.0f,
                    1.0f, 1.0f, 0, lowres.getXdim() - 1, 0,
                    lowres.getYdim() - 1, false, true, gridInfo.getMaxValue(),
                    gridInfo.getMinValue());
            dataGrid = sirsGrid.recomputeGrid();
        } else {
            statusHandler.handle(Priority.PROBLEM, "Algorithm: \"" + algorithm
                    + "\" not supported in ContourTool.");
        }
        return dataGrid;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.Message.IMessageClient#receiveMessage(
     * com.raytheon.viz.gfe.core.msgs.Message)
     */
    @Override
    public void receiveMessage(Message message) {
        if (message instanceof ContourServerMsg) {
            algorithm = ((ContourServerMsg) message).getAlgorithm();
        } else {
            statusHandler.handle(Priority.PROBLEM,
                    "Unexpected message type recieved: "
                            + message.getClass().getName());
        }

    }

    @Override
    public void gridDataChanged(ParmID parmId, TimeRange validTime) {
        if ((currentGrid != null)
                && currentGrid.getParm().getParmID().equals(parmId)
                && currentGrid.getGridTime().equals(validTime)) {
            initializeContourData(getGrid());

            // Get and draw the contours
            contours = getContours(currentContourValues);
            replaceCLines(contours);
        }
    }

    @Override
    public void parmInventoryChanged(Parm parm, TimeRange affectedTimeRange) {
        if ((currentGrid != null)
                && parm.getParmID().equals(currentGrid.getParm().getParmID())
                && affectedTimeRange.contains(currentGrid.getGridTime())) {
            initializeContourData(getGrid());

            // Get and draw the contours
            contours = getContours(currentContourValues);
            replaceCLines(contours);
        }
    }

    @Override
    public void displayedParmListChanged(Parm[] parms, Parm[] deletions,
            Parm[] additions) {
        if (currentGrid != null) {
            for (Parm parm : deletions) {
                if (parm.equals(currentGrid.getParm())) {
                    newActiveGrid(null);
                    break;
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * Looks specifically for no lock for the current grid, and if so,
     * reinitializes the contour data and resets things. This scenario will
     * occur if this grid is being edited, but not yet recalculated, and a save
     * occurs (manual or autosave). Note that this is not called when break lock
     * occurs, since we will get a parmInventoryChanged() message first, which
     * cleans out modified contours.
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.ILockTableChangedListener#lockTableChanged
     * (com.raytheon.viz.gfe.core.parm.Parm,
     * com.raytheon.edex.plugin.gfe.server.lock.LockTable)
     */
    @Override
    public void lockTableChanged(Parm parm, LockTable lockTable) {
        if (currentGrid == null) {
            return;
        }

        TimeRange timeRange = currentGrid.getGridTime();

        WsId wsId = parm.getDataManager().getWsId();
        if (parm.getParmID().equals(currentGrid.getParm().getParmID())
                && lockTable.checkLock(timeRange, wsId).equals(
                        LockStatus.LOCKABLE)) {
            if (modifiedContours) {
                popRecalcDialog("Recalculate based on edited contours before saving/autosave?");
            }

            // the recalc will make the grid modified again, so we go ahead and
            // issue a separate save command
            if (parm.getLockTable().checkLock(timeRange, wsId)
                    .equals(LockStatus.LOCKED_BY_ME)) {
                parm.saveParameter(timeRange);
                currentGrid.depopulate();
            }

            initializeContourData(currentGrid);

            // Get and draw the contours
            contours = getContours(currentContourValues);
            replaceCLines(contours);
        }
    }
}
