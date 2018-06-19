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
package com.raytheon.viz.hydro.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydro.resource.DamLocationResource;
import com.raytheon.viz.hydro.resource.MultiPointResource;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.ui.EditorUtil;

/**
 * Action for the Clear Map menu item. Clears the map.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2009 2637       mpduff      Initial creation
 * Jan 28, 2010 5274       bkowal      Clearing the data will now cancel any
 *                                     PointDataControlManager jobs.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ClearDataAction extends AbstractHandler {

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        HydroDisplayManager displayManager = HydroDisplayManager.getInstance();

        // Clear what's in the display manager
        displayManager.clearDisplay();
        PointDataControlManager pdcManager = PointDataControlManager
                .getInstance();
        pdcManager.cancelRunningJobs();
        MultiPointResource mpr = pdcManager.getMultiPointResource();
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            ResourceList rl = container.getActiveDisplayPane().getDescriptor()
                    .getResourceList();

            // Clear MultiPointResource
            if (mpr != null) {
                rl.removeRsc(mpr);
                mpr.dispose();
            }

            // Clear DamLocationResource
            DamLocationResource dlr = pdcManager.getDamLocationResource();
            if (dlr != null) {
                rl.removeRsc(dlr);
                dlr.dispose();
            }
        }
        return null;
    }

}
