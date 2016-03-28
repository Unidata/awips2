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
import java.awt.geom.Point2D;
import java.text.DecimalFormat;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PreferenceInitializer;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData.EditOp;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements the Pencil Tool
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/01/2008              chammack    Added Pencil Influence
 * 04/16/2009   2262       rjpeter     Updated to handle mouse movements off the extent
 * 01/28/2016   5295       dgilling    Fix handling of ISC grids with different
 *                                     time constraints from Fcst grid.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PencilTool extends AbstractFreeformTool {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PencilTool.class);

    /**
     * Pencil tool influence list.
     */
    private static int[] influences = new int[] { 1, 2, 5, 10, 15 };

    static {
        new PreferenceInitializer() {
            @Override
            public void init() {
                PythonPreferenceStore prefs = Activator.getDefault()
                        .getPreferenceStore();
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
            }
        }.run();
    }

    /**
     * Constructor
     */
    public PencilTool() {
        super();
        handler = new PencilToolDelegateAction();
    }

    /**
     * Returns the contour value based on the the first coordinate in _coords.
     * If in ISC mode, then use the data value of the ISC grid instead. --
     * implementation
     * 
     * @param grid
     *            the grid
     */
    private WxValue contourValue(final IGridData grid) {

        Parm parm = dataManager.getSpatialDisplayManager().getActivatedParm();
        GridParmInfo gpi = parm.getGridInfo();
        Coordinate gridCoord = MapUtil.latLonToGridCoordinate(
                currentCoordinates.get(0), PixelOrientation.UPPER_LEFT,
                gpi.getGridLoc());

        if ((gridCoord.x < 0)
                || (gridCoord.x >= MapUtil
                        .getGridGeometry(parm.getGridInfo().getGridLoc())
                        .getGridRange().getSpan(0))
                || (gridCoord.y < 0)
                || (gridCoord.y >= MapUtil
                        .getGridGeometry(parm.getGridInfo().getGridLoc())
                        .getGridRange().getSpan(1))) {
            return null;
        }

        boolean iscMode = this.dataManager.getParmManager().iscMode();
        boolean fcstGrid = (grid.getParm().getParmID().getDbId()
                .equals(this.dataManager.getParmManager().getMutableDatabase()));

        // isc mode, and fcst grid
        if (iscMode && fcstGrid) {
            GridID gid = new GridID(grid.getParm(), dataManager
                    .getSpatialDisplayManager().getSpatialEditorTime());
            return iscDataPoint(gid, new Point((int) gridCoord.x,
                    (int) gridCoord.y));
        }
        // normal mode
        else {
            return grid.getWxValue((int) gridCoord.x, (int) gridCoord.y);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.edittool.AbstractFreeformTool#handleMouseUp(java
     * .awt.geom.Point2D, com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    protected void handleEndDrag(int button, Point2D point2D,
            Coordinate coordinate) {
        boolean started = false;
        try {
            if (!startParmEdit()) {
                return;
            }

            started = true;
            IGridData grid = getGrid();
            WxValue wxValue = contourValue(grid);
            if (wxValue == null) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Pencil Operation must start over a valid grid point");
                return;
            }

            if ((grid.getParm().getGridInfo().getGridType() != GridType.WEATHER)
                    && (grid.getParm().getGridInfo().getGridType() != GridType.DISCRETE)) {
                Grid2DBit changedBits = grid.getParm().pencilStretch(
                        dataManager.getSpatialDisplayManager()
                                .getSpatialEditorTime(),
                        wxValue,
                        currentCoordinates
                                .toArray(new Coordinate[currentCoordinates
                                        .size()]));

                if (!changedBits.isAnyBitsSet()) {
                    statusHandler
                            .handle(Priority.SIGNIFICANT,
                                    "Pencil influence does not intersect active edit area.");
                }

                return;

            }

            // Have to make sure we start/end in same area for Weather
            // and Discrete
            GridLocation gloc = grid.getParm().getGridInfo().getGridLoc();
            Coordinate temp = MapUtil.latLonToGridCoordinate(
                    currentCoordinates.get(0), PixelOrientation.UPPER_LEFT,
                    gloc);
            Point gcStart = new Point((int) temp.x, (int) temp.y);

            temp = MapUtil.latLonToGridCoordinate(
                    currentCoordinates.get(currentCoordinates.size() - 1),
                    PixelOrientation.UPPER_LEFT, gloc);
            Point gcEnd = new Point((int) temp.x, (int) temp.y);

            boolean inGridStart = (gcStart.x >= 0) && (gcStart.y >= 0)
                    && (gcStart.x < gloc.getNx()) && (gcStart.y < gloc.getNy());
            boolean inGridEnd = (gcEnd.x >= 0) && (gcEnd.y >= 0)
                    && (gcEnd.x < gloc.getNx()) && (gcEnd.y < gloc.getNy());

            if (inGridStart && inGridEnd) {
                Grid2DBit contigArea = grid.getContiguousArea(dataManager
                        .getSpatialDisplayManager().getSpatialEditorTime(),
                        gcStart);

                if (contigArea.getAsBoolean(gcEnd.x, gcEnd.y)) {
                    // ensure path is closed
                    if (!currentCoordinates.get(0)
                            .equals(currentCoordinates.get(currentCoordinates
                                    .size() - 1))) {
                        currentCoordinates.add(currentCoordinates.get(0));
                    }

                    Grid2DBit changedBits = grid.getParm().pencilStretch(
                            dataManager.getSpatialDisplayManager()
                                    .getSpatialEditorTime(),
                            wxValue,
                            currentCoordinates
                                    .toArray(new Coordinate[currentCoordinates
                                            .size()]));

                    if (!changedBits.isAnyBitsSet()) {
                        statusHandler
                                .handle(Priority.SIGNIFICANT,
                                        "Pencil influence does not intersect active edit area.");
                    }

                    return;
                }
            }
            statusHandler
                    .handle(Priority.SIGNIFICANT,
                            "Editing is currently not allowed.  Reason: "
                                    + "Pencil Operation for WEATHER/DISCRETE must start/end in same area");
        } finally {
            if (started) {
                endParmEdit();
            }

            refresh();
        }
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
            return "The pencil tool only works when a grid is editable";
        }

        String gridsUnlockedReason = gridsUnlocked();
        if (gridsUnlockedReason != null) {
            return gridsUnlockedReason;
        }

        if (!isValidEditOp(EditOp.PENCILSTRETCH)) {
            return "The Pencil Tool is not allowed on grids of this type.";
        }

        return null;
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
     * com.raytheon.viz.ui.cmenu.IRightClickCapableResource#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        Parm activeParm = dataManager.getSpatialDisplayManager()
                .getActivatedParm();
        if (activeParm != null) {

            Coordinate sz = activeParm.getGridInfo().getGridLoc()
                    .gridCellSize();

            if (sz != null) {
                menuManager.add(new PencilInfluenceAction(sz.y));
            }
        }
    }

    private class PencilInfluenceAction extends AbstractRightClickAction
            implements IMenuCreator {

        private Menu menu;

        private final double resolution;

        public PencilInfluenceAction(double resolution) {
            super("Pencil Tool Influence", SWT.DROP_DOWN);
            this.resolution = resolution;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            // no op
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

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
         * .Control)
         */
        @Override
        public Menu getMenu(Control parent) {
            if (menu != null) {
                menu.dispose();
            }

            menu = new Menu(parent);

            fillMenu(menu);

            return menu;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets
         * .Menu)
         */
        @Override
        public Menu getMenu(Menu parent) {
            if (menu != null) {
                menu.dispose();
            }

            menu = new Menu(parent);

            fillMenu(menu);

            return menu;
        }

        /**
         * Populate the menu.
         * 
         * @param menu
         *            The menu to populate
         */
        private void fillMenu(Menu menu) {
            for (int influence : influences) {
                ActionContributionItem aci = new ActionContributionItem(
                        new PencilToolAction(resolution, influence));
                aci.fill(menu, -1);
            }
        }
    }

    private class PencilToolAction extends Action {

        private final double resolution;

        private final int influence;

        public PencilToolAction(double resolution, int influence) {
            super("", Action.AS_RADIO_BUTTON);
            this.resolution = resolution;
            this.influence = influence;
            Parm activatedParm = dataManager.getSpatialDisplayManager()
                    .getActivatedParm();
            setChecked(activatedParm.getParmState().getPencilWidth() == influence);
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
     * Custom DelegateAction implementation that will ignore mouse click actions
     * from all but mouse button 1.
     * 
     */
    private class PencilToolDelegateAction extends DelegateAction {

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.gfe.edittool.AbstractGFEEditTool.DelegateAction#
         * handleMouseDownMove(int, int, int)
         */
        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            if (mouseButton == 1) {
                return super.handleMouseDownMove(x, y, mouseButton);
            } else {
                return false;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.gfe.edittool.AbstractGFEEditTool.DelegateAction#
         * handleMouseDown(int, int, int)
         */
        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            if (mouseButton == 1) {
                return super.handleMouseDown(x, y, mouseButton);
            } else {
                return false;
            }
        }
    }

    // -- private
    // ----------------------------------------------------------------
    // PencilTool::iscDataPoint()
    // Returns the data value when in isc mode.
    // -- implementation
    // ---------------------------------------------------------
    // ---------------------------------------------------------------------------
    private WxValue iscDataPoint(GridID gid, final Point gridCoord) {
        // are we in our own site's domain or not?
        String site = this.dataManager.getIscDataAccess().getISCSite(gridCoord,
                gid);
        if (site.equals(this.dataManager.getSiteID())
                && !gid.getParm().isIscParm()) {
            IGridData grid = gid.grid();
            if (grid != null) {
                return grid.getWxValue(gridCoord.x, gridCoord.y);
            } else {
                return null;
            }
        }

        // in isc domain, or isc gridID
        else {
            GridID iscGid = this.dataManager.getIscDataAccess().getISCGridID(
                    gid, true);
            if (iscGid == null) {
                return null;
            } // no corresponding parm
            IGridData grid = iscGid.grid();
            if (grid == null) {
                return null;
            } // no corresponding grid

            if (!grid.getHistorySites().contains(site)) {
                return null;
            } // no data in isc grid from corresponding site

            return grid.getWxValue(gridCoord.x, gridCoord.y);
        }
    }
}
