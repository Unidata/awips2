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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.SharedDisplayVenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;

/**
 * Invite some users to join a session.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2012            bsteffen     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * Jan 28, 2014 2698       bclement    removed venue info
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * Mar 06, 2014 2848       bclement    get venueName directly from session
 * May 19, 2014 3180       bclement    replaced getNewSessions() with new logic in getFilteredSessions()
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class InviteAction extends Action {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(InviteAction.class);

    private final UserId[] users;

    private final IVenueSession session;

    private String inviteMessage;

    public InviteAction(UserId... users) {
        super("Invite...");
        this.users = users;
        this.session = null;
        this.setToolTipText("Invite selected user(s) to join a session.");
        // set the initial enabled state.
        updateEnabled();
        setMenuCreator(new MenuCreator());
    }

    public InviteAction(IVenueSession session, String description,
            UserId... users) {
        super(description);
        this.users = users;
        this.session = session;
        this.setToolTipText("Invite selected user(s) to join a session.");
    }

    @Override
    public void run() {
        try {
            VenueInvite invite = null;
            if (session instanceof ISharedDisplaySession) {
                SharedDisplayVenueInvite displayInvite = new SharedDisplayVenueInvite();
                displayInvite.setDataProvider(SharedDisplaySessionMgr
                        .getSessionContainer(session.getSessionId())
                        .getSession().getCurrentDataProvider());
                displayInvite.setSessionLeader(SharedDisplaySessionMgr
                        .getSessionContainer(session.getSessionId())
                        .getSession().getCurrentSessionLeader());
                invite = displayInvite;
            } else {
                invite = new VenueInvite();
            }
            invite.setMessage(inviteMessage);
            invite.setSessionId(session.getSessionId());
            invite.setSubject(session.getVenue().getSubject());
            List<UserId> inviteList = new ArrayList<UserId>();
            UserId inviter = CollaborationConnection.getConnection().getUser();
            for (UserId user : users) {

                // don't invite the user sending the invite
                if (!inviter.equals(user)) {
                    inviteList.add(user);
                }
            }
            session.sendInvitation(inviteList, invite);
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM, "Error sending invitiation",
                    e);
        }
    }

    /**
     * Set the enabled status of this action to be determined based off what
     * users are available.
     */
    public void updateEnabled() {
        boolean enabled = false;
        Collection<IVenueSession> filteredSessions = getFilteredSessions();
        if ((session == null && !filteredSessions.isEmpty())
                || filteredSessions.contains(session)) {
            enabled = true;
        }
        setEnabled(enabled);
    }

    /**
     * @return a collection of VenueSessions in which the current user is a
     *         participant and any of the {@link #users} are not
     * @see #allInSession(Collection, IVenueSession)
     */
    private Collection<IVenueSession> getFilteredSessions() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        Collection<IVenueSession> allSessions = connection
                .getJoinedVenueSessions();
        List<IVenueSession> result = new ArrayList<IVenueSession>();
        List<UserId> userlist = Arrays.asList(this.users);
        for (IVenueSession session : allSessions) {
            if (!allInSession(userlist, session)) {
                result.add(session);
            }
        }
        return result;
    }

    /**
     * Note: this method will always return false for a session in which this
     * user cannot know the real user IDs of the participants
     * 
     * @param users
     * @param session
     * @return false if any of the users are not in the session
     */
    private static boolean allInSession(Collection<UserId> users,
            IVenueSession session) {
        boolean rval = true;
        Collection<VenueParticipant> participants = session.getVenue()
                .getParticipants();
        Set<String> participantIds = new HashSet<String>(participants.size());
        for (VenueParticipant vp : participants) {
            if (vp.hasActualUserId()) {
                participantIds.add(vp.getUserid().getNormalizedId());
            }
        }
        for (UserId user : users) {
            if (!participantIds.contains(user.getNormalizedId())) {
                rval = false;
                break;
            }
        }
        return rval;
    }

    private class MenuCreator implements IMenuCreator {

        private Menu menu;

        @Override
        public void dispose() {
            menu.dispose();
        }

        @Override
        public Menu getMenu(Control parent) {
            menu = new Menu(parent);
            fill();
            return menu;
        }

        @Override
        public Menu getMenu(Menu parent) {
            menu = new Menu(parent);
            fill();
            return menu;
        }

        private void fill() {
            for (IVenueSession session : getFilteredSessions()) {
                String name = session.getVenueName();
                Action action = new InviteAction(session, name, users);
                IContributionItem contrib = new ActionContributionItem(action);
                contrib.fill(menu, -1);
            }
        }
    }

    /**
     * @param inviteMessage
     *            the inviteMessage to set
     */
    public void setInviteMessage(String inviteMessage) {
        this.inviteMessage = inviteMessage;
    }
}
