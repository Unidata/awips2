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
package com.raytheon.uf.viz.collaboration.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Display;
import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.login.ChangeStatusDialog;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;

/**
 * Change the status message for the current user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 6, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ChangeStatusMessageAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ChangeStatusMessageAction.class);

    public ChangeStatusMessageAction() {
        super("Change Status Message...");
        setEnabled(CollaborationConnection.getConnection() != null);
    }

    @Override
    public void run() {
        ChangeStatusDialog dialog = new ChangeStatusDialog(Display.getCurrent()
                .getActiveShell());
        dialog.open();
        // I would expect the dialog to do this
        String msg = Activator.getDefault().getPreferenceStore()
                .getString(CollabPrefConstants.P_MESSAGE);
        CollaborationConnection connection = CollaborationConnection
                .getConnection();

        Presence presence = connection.getPresence();
        Presence newPresence = new Presence(presence.getType(), msg,
                presence.getPriority(), presence.getMode());
        Tools.copyProperties(presence, newPresence);

        try {
            connection.getAccountManager().sendPresence(newPresence);
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM, "Error sending presence", e);
        }

    }

}
