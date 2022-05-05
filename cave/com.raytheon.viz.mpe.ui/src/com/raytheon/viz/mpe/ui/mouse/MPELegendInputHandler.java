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
package com.raytheon.viz.mpe.ui.mouse;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.actions.DrawDQCStations;
import com.raytheon.viz.mpe.ui.rsc.MPELegendResource;
import com.raytheon.viz.ui.input.InputAdapter;

/**
 * Handles the mouse interactions for the colorbar
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 28, 2009           snaples   Initial Creation.
 * Jan 15, 2019  7103     randerso  Don't pass on mouse down move event when
 *                                  dragging in colorbar.
 *
 * </pre>
 *
 * @author snaples
 */
public class MPELegendInputHandler extends InputAdapter {

    private MPELegendResource rsc;

    private int[] lastPoint;

    private PixelExtent pe;

    /**
     * Constructor
     *
     * @param rsc
     *            the associated MPELegendResource
     */
    public MPELegendInputHandler(MPELegendResource rsc) {
        this.rsc = rsc;
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        pe = rsc.getExtent();
        if (pe == null) {
            return false;
        }

        double[] v = rsc.getResourceContainer().getActiveDisplayPane()
                .screenToGrid(x, y, 0);
        if ((mouseButton == 1 || mouseButton == 3)
                && (!MPEDisplayManager.getCurrent().isGroupedt())
                && pe.contains(v[0], v[1]) && (DrawDQCStations.grids_flag == 1
                        || DrawDQCStations.map_flag == 1)) {
            return execute(x, y, true);

        }
        return false;
    }

    private boolean execute(int x, int y, boolean setLastPoint) {
        pe = rsc.getExtent();

        if (pe == null) {
            return false;
        }

        double[] v = rsc.getResourceContainer().getActiveDisplayPane()
                .screenToGrid(x, y, 0);
        if (pe.contains(v[0], v[1])) {
            if (setLastPoint) {
                lastPoint = new int[] { x, y };
            }
            rsc.getValueAt(v);
            rsc.issueRefresh();
            return true;
        } else if (setLastPoint) {
            lastPoint = null;
        }
        return false;
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {

        if (lastPoint != null) {
            if (mouseButton == 1) {
                // execute(x, y, false);
            }
            return true;
        }

        return false;
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        if (lastPoint != null) {
            lastPoint = null;
            return true;
        }
        return false;
    }

}
