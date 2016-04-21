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
package com.raytheon.uf.viz.collaboration.ui;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PartInitException;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.event.ServerDisconnectEvent;
import com.raytheon.uf.viz.collaboration.ui.actions.LogoutAction;
import com.raytheon.uf.viz.collaboration.ui.login.LoginDialog;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Handles xmpp server disconnect events
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2014 2903       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DisconnectHandler {

    private static final String TITLE = "Disconnected from Collaboration Server";

    private static final String DEFAULT_MESSAGE = "Click OK to re-login to the collaboration server";

    /**
     * 
     */
    public DisconnectHandler() {
    }

    /**
     * Handle a disconnect from the xmpp server
     * 
     * @param e
     */
    @Subscribe
    public void serverDisconnected(final ServerDisconnectEvent e) {
        CollaborationConnection conn = CollaborationConnection.getConnection();
        if (conn == null) {
            // we aren't logged in
            return;
        }
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                /* close out existing collaboration components */
                LogoutAction.closeCollaboration();
                Shell shell = new Shell(Display.getCurrent());
                StringBuilder msg = new StringBuilder();
                String reason = e.getReason();
                if (reason != null && !reason.isEmpty()) {
                    msg.append("Reason: ").append(reason).append('\n');
                }
                msg.append(DEFAULT_MESSAGE);
                /* inform the user of disconnect then re-login if requested */
                if (MessageDialog.openConfirm(shell, TITLE, msg.toString())
                        && new LoginDialog(shell).login()) {
                    try {
                        /* user has logged back in, put the view back */
                        CollaborationGroupView.showView(true);
                    } catch (PartInitException e) {
                        Activator.statusHandler.error(
                                "Problem restoring collaboration view", e);
                    }
                }
            }
        });
    }

}
