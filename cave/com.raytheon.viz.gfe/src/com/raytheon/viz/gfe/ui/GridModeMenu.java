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
package com.raytheon.viz.gfe.ui;

import com.raytheon.viz.gfe.core.msgs.GMDisplayModeMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.gridmanager.GridMode;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GridModeMenu extends EnumMenu {
    private static final String COMMAND_ID = "com.raytheon.viz.gfe.actions.SetGridMode";

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.EnumMenu#getCategoryId()
     */
    @Override
    protected String getCategoryId() {
        return "GFE";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.EnumMenu#getCommandId()
     */
    @Override
    protected String getCommandId() {
        return COMMAND_ID;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.EnumMenu#getCommandName()
     */
    @Override
    protected String getCommandName() {
        return "Set Grid Display Mode";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.EnumMenu#getCurrentValue()
     */
    @Override
    protected GridMode getCurrentValue() {
        return Message.inquireLastMessage(GMDisplayModeMsg.class).getGridMode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.ui.EnumMenu#setCurrentValue(java.lang.Enum)
     */
    @Override
    protected void setCurrentValue(Enum<?> value) {
        new GMDisplayModeMsg((GridMode) value).send();
    }

}
