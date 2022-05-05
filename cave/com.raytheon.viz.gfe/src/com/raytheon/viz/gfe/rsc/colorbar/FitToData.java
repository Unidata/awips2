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
package com.raytheon.viz.gfe.rsc.colorbar;

import java.util.Arrays;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.griddata.ScalarDataObject;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.rsc.GFEResource;

/**
 * Ported from ContColorTable.C
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 08, 2009           njensen   Initial creation
 * Jan 13, 2015  3955     randerso  Fixed NullPointerException when no resource
 *                                  matching the current parm is found
 * Jan 03, 2018  7178     randerso  Code cleanup
 *
 * </pre>
 *
 * @author njensen
 */

public class FitToData {

    /**
     * Fit-To-Data modes
     */
    public enum FitToDataMode {
        /** All Grids */
        ALL_GRIDS("All Grids"),

        /** All Grids over Area */
        ALL_GRIDS_OVER_AREA("All Grids over Area"),

        /** Single Grid */
        SINGLE_GRID("Single Grid"),

        /** Single Grid over Area */
        SINGLE_GRID_OVER_AREA("Single Grid over Area");

        protected final String menu;

        FitToDataMode(String menu) {
            this.menu = menu;
        }
    };

    private DataManager dataMgr;

    private Parm parm;

    /**
     * Constructor
     *
     * @param dataManager
     * @param parm
     */
    public FitToData(DataManager dataManager, Parm parm) {
        dataMgr = dataManager;
        this.parm = parm;
    }

    /**
     * Command to fit (remap) the color table to the data values associated with
     * this parm over the entire domain.
     *
     * @throws GFEOperationFailedException
     */
    public void fitToData() throws GFEOperationFailedException {
        ReferenceData ref = dataMgr.getRefManager().fullRefSet();
        doFitToData(ref);
    }

    /**
     * Command to fit (remap) the color table to the data values associated with
     * this parm over the given reference data.
     *
     * @param refData
     * @throws GFEOperationFailedException
     */
    public void fitToData(ReferenceData refData)
            throws GFEOperationFailedException {
        doFitToData(refData);
    }

    /**
     * Command to fit (remap) the color table to the data values associated with
     * just this one grid.
     *
     * @param gid
     * @throws GFEOperationFailedException
     */
    public void fitToData(GridID gid) throws GFEOperationFailedException {
        fitToData(gid, dataMgr.getRefManager().fullRefSet());
    }

    /**
     * Command to fit (remap) the color table to the data values associated with
     * just this one grid and reference data.
     *
     * @param gid
     * @param refData
     * @throws GFEOperationFailedException
     */
    public void fitToData(GridID gid, ReferenceData refData)
            throws GFEOperationFailedException {
        if ((gid == null) || (gid.getParm() == null)) {
            throw new GFEOperationFailedException(
                    "fitToData: no grid is selected.");
        }

        // verify grid is associated with this parm
        if (!gid.getParm().equals(parm)) {
            throw new GFEOperationFailedException(
                    "fitToData grid's parm does not match color table's parm");
        }

        doFitToData(gid, refData);
    }

    private void doFitToData(GridID gid, ReferenceData refData)
            throws GFEOperationFailedException {
        if (parm == null) {
            throw new GFEOperationFailedException("No parm is selected.");
        }

        // convert reference data to valid Grid2DBit.
        Grid2DBit bits = refData.getGrid();

        // need to get the max/min data values from all grids.

        // YES, these are reversed intentionally
        float maximum = parm.getGridInfo().getMinValue();
        float minimum = parm.getGridInfo().getMaxValue();

        IGridData grid = gid.grid();
        // process the grid
        if (grid != null) {
            ScalarDataObject sGrid = null;

            // normal mode
            if (!dataMgr.getParmManager().iscMode()) {
                sGrid = (ScalarDataObject) grid.getDataObject();
            }
            // isc mode
            else {
                Pair<Grid2DBit, IGridData> p = dataMgr.getIscDataAccess()
                        .getCompositeGrid(gid, true);
                Grid2DBit valid = p.getFirst();
                sGrid = (ScalarDataObject) p.getSecond().getDataObject();

                // for isc, mask valid bits with reference
                bits = bits.and(valid);
            }

            if (bits.isAnyBitsSet()) {
                float[] result = calcMaxMin(sGrid, bits);
                minimum = Math.min(minimum, result[0]);
                maximum = Math.max(maximum, result[1]);
            }
        }

        finishFitToData(minimum, maximum);
    }

    /**
     * Utility routine to fit (remap) the color table to the data values
     *
     * associated with the parm, and the given Grid2DBit. This version is for
     * the entire parm.
     *
     * ISC mode complicates this. In ISC mode, first run through the regular
     * parm for just the site area ANDd with the refData, then run through the
     * isc parm for its valid areas, without the primary site, ANDd with the
     * refData.
     */
    private void doFitToData(ReferenceData refData)
            throws GFEOperationFailedException {
        if (parm == null) {
            throw new GFEOperationFailedException("No parm is selected.");
        }

        // need to get the max/min data values from all grids.

        // YES, these are reversed intentionally
        float maximum = parm.getGridInfo().getMinValue();
        float minimum = parm.getGridInfo().getMaxValue();

        // determine the primary grids to be processed
        IGridData[] inv = parm.getGridInventory();

        // convert reference data to valid Grid2DBit.
        Grid2DBit bits = refData.getGrid();
        Grid2DBit procMask = bits;
        if (dataMgr.getParmManager().iscMode()) {
            // proc only this site's area with the refData - 1st pass
            procMask = bits.and(dataMgr.getRefManager().mySiteGridpoints());
        }

        for (int g = 0; g < inv.length; g++) {
            IGridData grid = inv[g];

            ScalarDataObject sGrid = (ScalarDataObject) grid.getDataObject();
            if (procMask.isAnyBitsSet()) {
                float[] result = calcMaxMin(sGrid, procMask);
                minimum = Math.min(minimum, result[0]);
                maximum = Math.max(maximum, result[1]);
            }
        }
        // process grids (isc mode) - 2nd pass
        if (dataMgr.getParmManager().iscMode()) {
            Parm iscP = this.dataMgr.getIscDataAccess().getISCParm(this.parm);
            if (iscP != null) {
                inv = iscP.getGridInventory();
                for (int g = 0; g < inv.length; g++) {
                    IGridData grid = inv[g];
                    ScalarDataObject sGrid = (ScalarDataObject) grid
                            .getDataObject();
                    procMask = bits.and(bits.and(dataMgr.getRefManager()
                            .siteGridpoints(Arrays.asList(dataMgr.getSiteID()),
                                    false)));
                    if (procMask.isAnyBitsSet()) {
                        float[] result = calcMaxMin(sGrid, procMask);
                        minimum = Math.min(minimum, result[0]);
                        maximum = Math.max(maximum, result[1]);
                    }
                }
            }
        }
        finishFitToData(minimum, maximum);
    }

    /**
     * Calculates the max/min data values in the given grid for the given
     * Grid2DBit. Returns false if there are no points identified.
     *
     */
    private float[] calcMaxMin(ScalarDataObject grid, Grid2DBit bits) {
        // need to get the max/min data values for the grid.

        // YES, these are reversed intentionally
        float maximum = parm.getGridInfo().getMinValue();
        float minimum = parm.getGridInfo().getMaxValue();

        int xdim = grid.getScalarGrid().getXdim();
        int ydim = grid.getScalarGrid().getYdim();
        for (int x = 0; x < xdim; x++) {
            for (int y = 0; y < ydim; y++) {
                if (bits.getAsBoolean(x, y)) {
                    float v = grid.getScalarGrid().get(x, y);
                    if (v > maximum) {
                        maximum = v;
                    }
                    if (v < minimum) {
                        minimum = v;
                    }
                }
            }
        }
        return new float[] { minimum, maximum };
    }

    private void finishFitToData(float minimum, float maximum) {
        // check for special case when no data was found (no valid grid points)
        // OR if all grids are the same value, reset them to the parm limits
        if (((maximum == parm.getGridInfo().getMinValue())
                && (minimum == parm.getGridInfo().getMaxValue()))
                || (minimum == maximum)) {
            maximum = parm.getGridInfo().getMaxValue();
            minimum = parm.getGridInfo().getMinValue();
        }

        // set the new ranges in the base class
        ISpatialDisplayManager spatialMgr = dataMgr.getSpatialDisplayManager();
        ResourcePair rp = spatialMgr.getResourcePair(parm);
        if (rp != null) {
            GFEResource rsc = (GFEResource) rp.getResource();
            ColorMapParameters params = rsc
                    .getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            params.setColorMapMax(maximum);
            params.setColorMapMin(minimum);
            parm.getListeners().fireColorTableModified(parm);
        }
    }
}
