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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.core.msgs.EnableDisableTopoMsg;
import com.raytheon.viz.gfe.core.msgs.EnableDisableTopoMsg.Action;
import com.raytheon.viz.gfe.core.msgs.Message;

/**
 * Handle the GFE Topography menu item
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------ ----------  ----------- --------------------------
 * Jul  2, 2008      #1160  randerso    Initial creation
 * Nov 20, 2013      #2331  randerso    Re-implemented using message
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TopoHandler extends AbstractHandler implements IElementUpdater {
    private IUFStatusHandler statusHandler = UFStatus
            .getHandler(TopoHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Action lastAction = Message.inquireLastMessage(
                EnableDisableTopoMsg.class).getAction();

        Action newAction;
        if (lastAction.equals(Action.ENABLE)) {
            newAction = Action.DISABLE;
        } else {
            newAction = Action.ENABLE;
        }

        statusHandler.debug("Maps->Topography " + newAction);

        new EnableDisableTopoMsg(newAction, true).send();
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.
     * menus.UIElement, java.util.Map)
     */
    @SuppressWarnings("rawtypes")
    @Override
    public void updateElement(final UIElement element, Map parameters) {
        element.setChecked(Message
                .inquireLastMessage(EnableDisableTopoMsg.class).getAction()
                .equals(EnableDisableTopoMsg.Action.ENABLE));
    }
}
