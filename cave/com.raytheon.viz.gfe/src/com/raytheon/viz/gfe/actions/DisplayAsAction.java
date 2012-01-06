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
package com.raytheon.viz.gfe.actions;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.rsc.GFEResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Provides contextual menu support for loading other types of visualizations.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 11, 2008	#1233	    chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DisplayAsAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        GFEResource rsc = ((GFEResource) this.getSelectedRsc());

        Parm parm = rsc.getParm();

        VisMode newMode = getNextMode(parm);
        parm.getDataManager().getSpatialDisplayManager().setDisplayMode(parm,
                newMode);
    }

    private VisMode getNextMode(Parm parm) {
        VisMode curMode = parm.getDisplayAttributes().getVisMode();

        int ordinal = curMode.ordinal() + 1;
        if (ordinal >= VisMode.values().length) {
            ordinal = 0;
        }

        VisMode newMode = VisMode.values()[ordinal];

        return newMode;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        GFEResource rsc = ((GFEResource) this.getSelectedRsc());

        Parm parm = rsc.getParm();

        return "Display as " + getNextMode(parm);
    }

}
