package com.raytheon.uf.viz.collaboration.ui.actions;

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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PartInitException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CollaborationGroupView;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Action to open the group view, as well as the default chat room
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012             rferrel     Initial creation
 * Mar 05, 2014   2798     mpduff      Don't create a new DisplayFeedAction
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class CollaborationGroupAction extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationGroupAction.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        // this opens the collaboration group view
        try {
            // If connection is null then user has not logged in
            boolean initialExecutionFlag = CollaborationConnection
                    .getConnection() == null;
            new LoginAction().run();
            CollaborationConnection connection = CollaborationConnection
                    .getConnection();
            if (connection == null) {
                // user cancelled login
                return event;
            }

            CollaborationGroupView view = (CollaborationGroupView) CaveWorkbenchPageManager
                    .getActiveInstance().showView(CollaborationGroupView.ID);

            // Is this is the first log in
            if (initialExecutionFlag) {
                // if autojoin is selected (to join the default room)
                if (Activator.getDefault().getPreferenceStore()
                        .getBoolean(CollabPrefConstants.AUTO_JOIN)) {
                    DisplayFeedAction action = view.getDisplayFeedAction();
                    action.setChecked(true);
                    action.run();
                }
            }
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to open collaboration contact list", e);
        }
        return event;
    }

}
