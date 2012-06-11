package com.raytheon.uf.viz.collaboration.ui;

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
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.session.SessionFeedView;

/**
 * Action to open the group view, as well as the default chat room
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
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
            PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                    .getActivePage().showView(CollaborationGroupView.ID);

            // if autojoin is selected (to join the default room)
            if (Activator.getDefault().getPreferenceStore()
                    .getBoolean("autojoin")) {
                CollaborationConnection connection = CollaborationConnection
                        .getConnection();
                try {
                    // TODO, make this configurable?
                    IVenueSession session = connection
                            .joinTextOnlyVenue("nws-collaboration");
                    PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(SessionFeedView.ID,
                                    session.getSessionId(),
                                    IWorkbenchPage.VIEW_ACTIVATE);
                } catch (CollaborationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to join the collaboration feed", e);
                }
            }
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to open collaboration contact list", e);
        }
        return event;
    }

}
