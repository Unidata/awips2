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
import com.raytheon.viz.gfe.smarttool.SmartToolConstants;
import com.raytheon.viz.gfe.smarttool.SmartUtil;

/**
 * Used by the right click menu on a WEATHER grid type, to set a discrete
 * weather element.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ------------- --------------------------
 * Jun 24, 2008            Eric Babin    Initial Creation.
 * May 28, 2009 #2159      Richard Peter Extracted to super class.
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class SetDiscretePickupAction extends AbstractSetDiscretePickupAction {
    public SetDiscretePickupAction(String title, String[] menuitemValues,
            Parm parm) {
        super(title, parm, menuitemValues);
    }

    protected void process() {
        SmartUtil.runTool(SmartToolConstants.ASSIGN);
    }
}
