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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.smarttool.SmartToolConstants;
import com.raytheon.viz.gfe.smarttool.SmartUtil;

/**
 * Assigns the Default Value to the selected grid
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2007            dfitch      Initial creation 
 * 31Jul2008    #1351       ebabin      Update to use SmartUtil assignments.
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */
public class AssignDefaultValue extends AbstractHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {

        Parm parms[] = DataManager.getCurrentInstance().getParmManager()
                .getAllParms();

        for (Parm parm : parms) {
            if (parm.getParmState().isSelected() && parm.isMutable()) {
                WxValue wxValue;
                wxValue = WxValue.defaultValue(parm);
                // TODO this is bad, should not be changing the pickUpValue
                parm.getParmState().setPickUpValue(wxValue);
                SmartUtil.runTool(SmartToolConstants.ASSIGN);
            }

        }

        return null;

    }
}
