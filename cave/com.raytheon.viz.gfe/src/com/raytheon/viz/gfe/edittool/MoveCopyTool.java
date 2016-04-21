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

import org.eclipse.jface.action.IMenuManager;
import org.geotools.geometry.jts.JTS;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.IISCDataAccess;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData.EditOp;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;

/**
 * GridPoint tool that moves/copies a spatial area from one place to another.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 5, 2008				chammack	Initial creation
 * 04/16/2009   2262        rjpeter     Updated to handle events off the extent
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class MoveCopyTool extends AbstractGFEEditTool {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MoveCopyTool.class);

    private boolean editingData = false;

    private JTSRenderable jtsRenderable;

    private Grid2DBit moveCopyEditInfluence;

    private int button;

    private GridLocation gridLocation;

    private Point startGridCoord;

    private Point lastGridCoord;

    private Geometry polyline;

    private boolean copyOp;

    /**
     * Constructor
     */
    public MoveCopyTool() {
        super();
        this.jtsRenderable = new JTSRenderable();
        this.renderables.add(this.jtsRenderable);
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
     * @see com.raytheon.viz.gfe.edittool.AbstractGFEEditTool#handleEvent(int,
     * com.raytheon.viz.gfe.edittool.AbstractGFEEditTool.EventType,
     * java.awt.Point, com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected void handleEvent(int button, EventType type, Point gridCoord,
            Coordinate mapCoord) {
        switch (type) {
        case START_DRAG:
            if (!this.isEditStateOK(true)) {
                return;
            }

            // Convert the map coord to a grid coord and save
            this.startGridCoord = gridCoord;

            // Save it in _lastGridCoord too
            this.lastGridCoord = this.startGridCoord;

            // Get the area of influence
            moveCopyEditInfluence = getEditInfluence(gridCoord);
            if (moveCopyEditInfluence.isAnyBitsSet()) {
                this.dataManager.getParmOp().clearUndoParmList();
                if (!this.startParmEdit()) {
                    return;
                }

                this.editingData = true;

                initVisual();

                copyOp = this.button == 2 ? false : true;
            }

            break;

        case IN_DRAG:
            if (!this.isEditStateOK(false) || !this.editingData) {
                return;
            }

            // Compare the last and current coords to see if we need to move
            if (gridCoord.equals(lastGridCoord)) {
                return;
            }

            lastGridCoord = gridCoord;

            updateVisual(gridCoord);
            break;
        case END_DRAG:
            if (!this.isEditStateOK(false) || !this.editingData) {
                return;
            }

            jtsRenderable.clear();

            // handle user releasing mouse button off the extent
            if (gridCoord != null) {
                int diffX = gridCoord.x - this.startGridCoord.x;
                int diffY = gridCoord.y - this.startGridCoord.y;

                this.getGrid()
                        .getParm()
                        .moveCopyArea(getGrid().getGridTime().getStart(),
                                moveCopyEditInfluence, new Point(diffX, diffY),
                                copyOp);
            }

            this.endParmEdit();
            this.editingData = false;
            refresh();
            break;
        }
    }

    private void initVisual() {
        this.jtsRenderable.clear();

        if (!this.moveCopyEditInfluence.isAnyBitsSet()) {
            return;
        }

        IGridData grid = this.getGrid();
        if (grid != null) {
            // Calculate the start location
            ReferenceData refData = this.gridLocation
                    .convertToReferenceData(moveCopyEditInfluence);

            MultiPolygon polygons = refData.getPolygons(CoordinateType.GRID);
            if (polygons.getNumGeometries() > 0) {
                this.polyline = polygons.getGeometryN(0);
            }
        }
    }

    private void updateVisual(Point gridCoord) {

        if (this.polyline == null) {
            return;
        }

        double diffX = gridCoord.x - this.startGridCoord.x;
        double diffY = gridCoord.y - this.startGridCoord.y;

        this.jtsRenderable.clear();

        Geometry polyLine = (Geometry) this.polyline.clone();
        for (Coordinate c : polyLine.getCoordinates()) {
            c.x += diffX;
            c.y += diffY;
        }

        try {
            polyLine = JTS.transform(polyLine, MapUtil.getTransformToLatLon(
                    PixelInCell.CELL_CENTER, this.gridLocation));
            this.jtsRenderable.clear();
            this.jtsRenderable.addGeometry(polyLine);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Execption transforming geometry to lat/lon", e);
        }

        refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.edittool.AbstractGFEEditTool#isOperationAllowed()
     */
    @Override
    protected String isOperationAllowed() {
        if (getGrid() == null) {
            return "The move/copy tool only works when a grid is editable";
        }

        String reasonWhyNot = this.gridsUnlocked();

        if (reasonWhyNot == null) {
            if (!isValidEditOp(EditOp.MOVE_COPY)) {
                reasonWhyNot = "The MoveCopy Tool is not allowed on grids of this type. ";
            }
        }

        return reasonWhyNot;
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
        // no items to add
    }

    // -- private
    // ----------------------------------------------------------------
    // MoveCopyTool::getEditInfluence()
    // Returns the edit influence as a Grid2DBit.
    // -- implementation
    // ---------------------------------------------------------
    // Edit Visual is restricted to valid data areas when in ISC mode.
    // ---------------------------------------------------------------------------
    private Grid2DBit getEditInfluence(Point gridCoord) {
        // Get the current refSet
        ReferenceData refData = this.dataManager.getRefManager()
                .getActiveRefSet();
        this.gridLocation = refData.getGloc();
        Grid2DBit influence = refData.getGrid();

        if (influence.isAnyBitsSet()) {

            influence = influence.contiguousBitArray(gridCoord);

            // special case, get valid data ISC/Fcst area, when in ISC mode
            if (this.dataManager.getParmManager().iscMode()) {
                IGridData grid = getGrid();
                IISCDataAccess iscDA = this.dataManager.getIscDataAccess();
                GridID gid = new GridID(grid.getParm(), grid.getGridTime()
                        .getStart());
                GridID iscGID = iscDA.getISCGridID(gid, true);
                IGridData iscGrid = iscGID.grid();
                Grid2DBit sitePoints = this.dataManager.getRefManager()
                        .mySiteGridpoints();
                if (iscGrid != null) {
                    Grid2DBit iscSitePoints = this.dataManager.getRefManager()
                            .siteGridpoints(iscGrid.getHistorySites(), false);
                    sitePoints.orEquals(iscSitePoints);
                }
                influence.andEquals(sitePoints); // reduce to valid data
            }
        }

        return influence;
    }

}
