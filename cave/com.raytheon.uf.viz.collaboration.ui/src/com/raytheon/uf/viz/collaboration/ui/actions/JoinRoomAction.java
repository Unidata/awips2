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
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.session.VenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;
import com.raytheon.uf.viz.collaboration.ui.prefs.HandleUtil;
import com.raytheon.uf.viz.collaboration.ui.session.SessionView;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Action for joining a public chat room on server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct  8, 2014  3705      bclement    Initial creation
 * Nov 12, 2014  3705      bclement    fixed empty participant list problem
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class JoinRoomAction extends Action {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(JoinRoomAction.class);

    private final boolean promptForHandle;

    private final VenueId room;

    /**
     * 
     */
    public JoinRoomAction(VenueId room, boolean promptForHandle) {
        super(promptForHandle ? "Join with Custom Handle"
                : "Join with Default Handle");
        this.promptForHandle = promptForHandle;
        this.room = room;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        IVenueSession joinedVenueSession = connection
                .getJoinedVenueSession(room);
        if (joinedVenueSession != null) {
            openExistingSession(joinedVenueSession);
            return;
        }
        String handle = HandleUtil.getDefaultHandle();
        if (promptForHandle) {
            Shell shell = new Shell(Display.getCurrent());
            InputDialog dlg = new InputDialog(shell, "Join Room",
                    "Enter handle for room: " + room.getName(), handle, null);
            if (dlg.open() == Window.OK) {
                handle = dlg.getValue();
            } else {
                /* user cancelled the dialog, abort joining the room */
                return;
            }
        }

        if (connection != null) {
            try {
                VenueSession session = connection.joinTextOnlyVenue(room,
                        handle);
                /*
                 * connect to room before UI initializes so it gets the
                 * participant list
                 */
                session.connectToRoom();
                CaveWorkbenchPageManager page = CaveWorkbenchPageManager
                        .getActiveInstance();
                page.showView(SessionView.ID, session.getSessionId(),
                        IWorkbenchPage.VIEW_ACTIVATE);
            } catch (CollaborationException | PartInitException e) {
                log.error("Unable to join room " + room.getFQName(), e);
            }
        }
    }

    private void openExistingSession(IVenueSession session) {
        if (promptForHandle) {
            Shell shell = new Shell(Display.getCurrent());
            /* we are already in the room, prompt user before continuing */
            if (!MessageDialog.openQuestion(shell, "Already In Room",
                    "Already joined to room '" + room.getName()
                            + "' with handle '"
                            + session.getUserID().getHandle()
                            + "'. Open Session?")) {
                /* user aborted */
                return;
            }
        }
        new ShowVenueAction(session).run();
    }

}
