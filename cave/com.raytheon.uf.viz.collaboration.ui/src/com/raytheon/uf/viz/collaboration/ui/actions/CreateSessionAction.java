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
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.CreateSessionData;
import com.raytheon.uf.viz.collaboration.ui.CreateSessionDialog;
import com.raytheon.uf.viz.collaboration.ui.IUserSelector;
import com.raytheon.uf.viz.collaboration.ui.session.CollaborationSessionView;
import com.raytheon.uf.viz.collaboration.ui.session.SessionView;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Create a new session, if users are provided they will be invited to the
 * session.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CreateSessionAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CreateSessionAction.class);

    private IUserSelector userSelection;

    public CreateSessionAction() {
        this(null);
    }

    public CreateSessionAction(IUserSelector userSelection) {
        super("Create Session...", getCreateSessionImageDescriptor());
        this.userSelection = userSelection;
    }

    @Override
    public void run() {
        CollaborationConnection sessionManager = CollaborationConnection
                .getConnection();
        if (sessionManager == null) {
            return;
        }

        CreateSessionDialog dialog = new CreateSessionDialog(Display
                .getCurrent().getActiveShell());
        dialog.open();

        CreateSessionData result = (CreateSessionData) dialog.getReturnValue();
        if (result != null) {
            String sessionId = result.getSessionId();
            if (result.isCollaborationSession()) {
                try {
                    CaveWorkbenchPageManager.getActiveInstance().showView(
                            CollaborationSessionView.ID, sessionId,
                            IWorkbenchPage.VIEW_ACTIVATE);
                } catch (PartInitException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to open collaboration sesson", e);
                }
            } else {
                try {
                    CaveWorkbenchPageManager.getActiveInstance().showView(
                            SessionView.ID, sessionId,
                            IWorkbenchPage.VIEW_ACTIVATE);
                } catch (PartInitException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to open text only chat session", e);
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Unable to open chat room view", e);
                }
            }

            try {
                if (result.isInviteUsers() && userSelection != null) {
                    UserId[] users = userSelection.getSelectedUsers();

                    if (users.length > 0) {
                        IVenueSession session = (IVenueSession) CollaborationConnection
                                .getConnection().getSession(sessionId);
                        IVenueInfo info = session.getVenue().getInfo();
                        InviteAction invite = new InviteAction(session,
                                info.getVenueDescription(), users);
                        invite.setInviteMessage(result.getInviteMessage());
                        invite.run();
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error sending invitation", e);
            }
        }
    }

    public static ImageDescriptor getCreateSessionImageDescriptor() {
        return IconUtil.getImageDescriptor(Activator.getDefault().getBundle(),
                "add_collaborate.gif");
    }

}
