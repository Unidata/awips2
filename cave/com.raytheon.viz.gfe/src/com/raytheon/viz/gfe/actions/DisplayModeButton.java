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

import java.util.Map;

import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.viz.gfe.core.msgs.GMDisplayModeMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.gridmanager.GridMode;
import com.raytheon.viz.ui.UiUtil;
import com.raytheon.viz.ui.actions.AbstractDropDownMenuHandler;

/**
 * Display Mode Button
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 9, 2008		    	dfitch  	Initial creation
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class DisplayModeButton extends AbstractDropDownMenuHandler implements
        IElementUpdater {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.actions.AbstractButtonHandler#updateElement(org.eclipse
     * .ui.menus.UIElement, java.util.Map)
     */
    @SuppressWarnings("rawtypes")
    @Override
    public void updateElement(UIElement element, Map parameters) {
        GridMode mode = Message.inquireLastMessage(GMDisplayModeMsg.class)
                .getGridMode();
        element.setText(mode.toString());
        UiUtil.updateWindowCoolBar((IWorkbenchWindow) parameters
                .get("org.eclipse.ui.IWorkbenchWindow"));
    }
}
