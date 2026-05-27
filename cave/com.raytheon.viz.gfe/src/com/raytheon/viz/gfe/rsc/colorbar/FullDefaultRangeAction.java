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
/**
 * 
 */
package com.raytheon.viz.gfe.rsc.colorbar;

import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * @author wldougher
 * 
 */
public class FullDefaultRangeAction extends AbstractRightClickAction {

    private Parm parm;

    public FullDefaultRangeAction(Parm parm) {
        super("Full Default Range");
        this.parm = parm;
    }

    @Override
    public void run() {
        ColorMapCapability cap = getSelectedRsc().getCapability(
                ColorMapCapability.class);
        cap.getColorMapParameters().setColorMapMax(
                parm.getGridInfo().getMaxValue());
        cap.getColorMapParameters().setColorMapMin(
                parm.getGridInfo().getMinValue());
        cap.notifyResources();
    }
}
