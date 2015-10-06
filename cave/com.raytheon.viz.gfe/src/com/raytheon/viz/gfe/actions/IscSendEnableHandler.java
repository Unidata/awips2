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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.msgs.ISCSendStatusChangedMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeProhibitedOpException;

/**
 * Menu handler for enabling and disabling send of ISC grids from GFE client.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2010            randerso     Initial creation
 * Sep 15, 2015  #4858     dgilling     Add isEnabled.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class IscSendEnableHandler extends AbstractHandler implements
        IElementUpdater {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            SimulatedTimeOperations.displayFeatureLevelWarning(shell,
                    "ISC Send Enable");
            return null;
        }

        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        if (dm != null) {
            boolean newState = !Message.inquireLastMessage(
                    ISCSendStatusChangedMsg.class).isEnabled();
            try {
                dm.enableISCsend(newState);
            } catch (SimulatedTimeProhibitedOpException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }

        return null;
    }

    @Override
    public boolean isEnabled() {
        DataManager dm = DataManagerUIFactory.getCurrentInstance();
        return (dm != null) ? CAVEMode.getMode().equals(CAVEMode.OPERATIONAL)
                && dm.requestISC() : false;
    }

    @Override
    public void updateElement(UIElement element, Map parameters) {
        element.setChecked(Message.inquireLastMessage(
                ISCSendStatusChangedMsg.class).isEnabled());
    }
}
