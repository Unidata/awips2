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

package com.raytheon.viz.pointdata.ui.cmenu;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.capabilities.IPlotDataResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Enable/Disable metar data within a layer
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Nov 28, 2006             brockwoo    Initial Creation.
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */

public class EnableDisableMetarAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        IPlotDataResource rsc = (IPlotDataResource) getSelectedRsc();
        boolean isEnabled = rsc.isMetarEnabled();
        rsc.setMetarMode(!isEnabled);
        this.setChecked(!isEnabled);
        if (isEnabled && !rsc.isMesowestEnabled()) {
            rsc.setMesowestMode(true);
        }
    }

    @Override
    public void setSelectedRsc(ResourcePair selectedRsc) {
        super.setSelectedRsc(selectedRsc);
        IPlotDataResource rsc = (IPlotDataResource) getSelectedRsc();

        boolean curState = rsc.isMetarEnabled();
        this.setChecked(curState);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Enable METAR";
    }

}
