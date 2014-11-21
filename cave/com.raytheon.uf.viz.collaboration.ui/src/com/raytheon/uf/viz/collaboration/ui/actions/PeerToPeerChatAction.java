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

import java.util.Collection;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Type;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.session.PeerToPeerView;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * Create a new chat session with a user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012            bsteffen     Initial creation
 * Jun 17, 2014 3078      bclement     changed user type to IUser, added isAvailable()
 * Jun 20, 2014 3281      bclement     fixed secondary id bug by using user.getClientIndependentId()
 * Nov 14, 2014 3709      mapeters     upon creation of p2p chat, add color change menu actions
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PeerToPeerChatAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PeerToPeerChatAction.class);

    private final IUser user;

    public PeerToPeerChatAction(IUser user) {
        super("Chat", IconUtil.getImageDescriptor(Activator.getDefault()
                .getBundle(), "chats.gif"));
        this.user = user;
        updateEnabled();
    }

    @Override
    public void run() {
        if (isAvailable(user)) {
            createP2PChat(IWorkbenchPage.VIEW_ACTIVATE);
        }
    }

    /**
     * @param user
     * @return true if user is available for chat
     */
    private boolean isAvailable(IUser user) {
        boolean rval = false;
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (user instanceof UserId) {
            Presence presence = connection.getContactsManager().getPresence(
                    (UserId) user);
            if (presence.getType() != Type.unavailable) {
                UserId loginUserId = CollaborationConnection.getConnection()
                        .getUser();
                rval = !loginUserId.isSameUser(user);
            }
        } else if (user instanceof VenueParticipant) {
            VenueParticipant participant = (VenueParticipant) user;
            Collection<IVenueSession> sessions = connection
                    .getJoinedVenueSessions();
            for (IVenueSession sesh : sessions) {
                String venueName = sesh.getVenueName();
                if (venueName.equals(participant.getRoom())) {
                    Presence presence = sesh.getVenue().getPresence(
                            (VenueParticipant) user);
                    if (presence.getType() != Type.unavailable) {
                        rval = true;
                        break;
                    }
                }
            }
        }
        return rval;
    }

    /**
     * Set the enabled status of this action to be determined based off what
     * users are available.
     */
    public void updateEnabled() {
        setEnabled(isAvailable(user));
    }

    /**
     * For creating a peer to peer chat, or adding to it.
     * 
     * @param viewMode
     *            IWorkbenchPage.VIEW_CREATE or IWorkbenchPage.VIEW_ACTIVATE
     *            where create will not pop up the view in front of others and
     *            activate will.
     * @return
     */
    public PeerToPeerView createP2PChat(Integer viewMode) {
        try {
            String id = user.getClientIndependentId();
            PeerToPeerView p2pView = (PeerToPeerView) CaveWorkbenchPageManager
                    .getActiveInstance().showView(PeerToPeerView.ID, id,
                            viewMode);
            if (p2pView.getPeer() == null) {
                p2pView.setPeer(user);
                /*
                 * add color change actions to P2P right click menu upon first
                 * creation of P2P chat.
                 */
                p2pView.addChangeUserColorActions();
            }
            return p2pView;
        } catch (PartInitException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to open chat", e);
        }
        return null;
    }

}
